                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃    Pyrocosm, version 0.1.0.                                                                      ┃
┃    © Copyright 2026 Jon Pretty, Propensive OÜ.                                                   ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://propensive.dev/pyrocosm/                                                          ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package pyrocosm

import scala.collection.concurrent.TrieMap

// Excluded from the umbrella: `Control`, `Origin` and `Service`, which would outrank this
// package's own definitions.
import soundness.{Service as _, *}

import codepages.utf8Codepage
import dynamicAccess.dynamicJson
import formatting.compactJsonFormatting
import strategies.throwUnsafely

// One MCP endpoint for every tool on the machine, at `/mcp` on the hub's origin, over the
// streamable HTTP transport: a client initialises one session here, lists the tools, prompts
// and resources of every tool at once — each tool's tools and prompts named under its own
// prefix, `fume_run` for fume's `run` — and calls them by those names, which the hub routes to
// the tool's own `/mcp` under a session it opens there as it is first needed. A resource is
// read from whichever tool answers for it. The server-to-client stream a GET opens is not
// carried: no tool sends notifications yet. Nothing here is a tool's own: the hub knows only
// which tool is which.
object Aggregator:
  // The protocol spoken to a client which names none.
  val protocol: Text = t"2025-06-18"

  // A JSON-RPC request, and the two answers.
  case class Rpc
    ( jsonrpc: Text = t"2.0",
      id:      Optional[Json] = Unset,
      method:  Text,
      params:  Optional[Json] = Unset )

  case class Result(jsonrpc: Text, id: Optional[Json], result: Json)
  case class Failure(jsonrpc: Text, id: Optional[Json], error: Fault)
  case class Fault(code: Int, message: Text)

  // What `initialize` answers with.
  case class Initialization
    ( protocolVersion: Text,
      capabilities:    Capabilities,
      serverInfo:      Implementation,
      instructions:    Text )

  case class Capabilities
    ( tools:     Listing      = Listing(),
      prompts:   Listing      = Listing(),
      resources: Subscription = Subscription() )

  case class Listing(listChanged: Boolean = false)
  case class Subscription(subscribe: Boolean = false, listChanged: Boolean = false)
  case class Implementation(name: Text, version: Text, title: Text)

  // The parameters a call or a read names its target by.
  case class Named(name: Text)
  case class Addressed(uri: Text)
  case class Versioned(protocolVersion: Optional[Text] = Unset)

  // The merged listings.
  case class Tools(tools: List[Json])
  case class Prompts(prompts: List[Json])
  case class Resources(resources: List[Json])
  case class Templates(resourceTemplates: List[Json])

  val ParseError: Int = -32700
  val InvalidRequest: Int = -32600
  val MethodNotFound: Int = -32601
  val InternalError: Int = -32603

  private val sessionHeader: Text = t"mcp-session-id"

class Aggregator(hub: Hub, version: Text):
  import Aggregator.*

  // Each client session's sessions with the tools, by the tool's name.
  private val sessions: TrieMap[Text, TrieMap[Text, Text]] = TrieMap()

  // The tools serving a front-end on this machine, and the port of each.
  private def tools: List[(Text, Int)] =
    hub.nodes()
    . filter: advert => advert.node.shares(hub.node) && advert.node.tool != Wire.tool
    . bind: advert =>
        advert.services.seek(_.name == t"serve").lay(Nil: List[(Text, Int)]): service =>
          List((advert.node.tool, service.port))

  private def header(request: Http.Request, key: Text): Optional[Text] =
    request.textHeaders.seek(_.key.lower == key).let(_.value)

  private def json(status: Http.Status, session: Optional[Text], body: Text): Http.Response =
    val headers: List[Http.Header] =
      Http.Header(t"content-type", t"application/json") ::
        session.lay(Nil: List[Http.Header]): id => List(Http.Header(sessionHeader, id))

    status(headers, Http.Body.Fixed(body.in[Data]))

  private def result(id: Optional[Json], result: Json): Text =
    Result(t"2.0", id, result).in[Json].show

  private def failure(id: Optional[Json], code: Int, message: Text): Text =
    Failure(t"2.0", id, Fault(code, message)).in[Json].show

  // Answers a request for `/mcp`; what goes wrong here is said in the answer, since a handler
  // which throws closes the connection with nothing said.
  def answer(request: Http.Request): Http.Response =
    try respond(request) catch case error: Throwable =>
      Http.Response(Http.InternalServerError)(t"The hub could not answer: ${error.toString}")

  private def respond(request: Http.Request): Http.Response =
    val session: Optional[Text] = header(request, sessionHeader)

    request.method match
      case Http.Post =>
        val body: Text = Loopback.bounded(request).utf8

        safely(body.read[Json].as[Rpc]) match
          case rpc: Rpc => post(session, rpc, body)

          case _ =>
            val message: Text = t"The request was not a JSON-RPC request"
            json(Http.Ok, session, failure(Unset, ParseError, message))

      case Http.Delete =>
        session.let(sessions.remove(_).getOrElse(Unset)).or(Unset).let: upstreams =>
          upstreams.foreach: (tool, id) =>
            tools.seek(_(0) == tool).let: (_, port) => send(port, Http.Delete, id, Unset)

        Http.Response(Http.NoContent)()

      case Http.Options =>
        Http.Response(Http.NoContent)()

      case _ =>
        Http.Response(Http.MethodNotAllowed)()

  private def post(session: Optional[Text], rpc: Rpc, body: Text): Http.Response = rpc.method match
    case t"initialize" =>
      val id: Text = Uuid().show
      sessions(id) = TrieMap()

      val protocol: Text =
        rpc.params.let: params => safely(params.as[Versioned]).let(_.protocolVersion).or(Unset)
        . or(Aggregator.protocol)

      val instructions: Text =
        t"Every Pyrocosm tool on this machine, through its hub: a tool or prompt is named by its " +
          t"tool and its own name, as fume_run."

      val server: Implementation = Implementation(t"pyrocosm", version, t"Pyrocosm")

      val initialization: Initialization =
        Initialization(protocol, Capabilities(), server, instructions)

      json(Http.Ok, id, result(rpc.id, initialization.in[Json]))

    case method if method.starts(t"notifications/") =>
      Http.Response(Http.Accepted)()

    case t"ping" =>
      json(Http.Ok, session, result(rpc.id, t"{}".read[Json]))

    case t"tools/list" =>
      val listing: Tools = Tools(merged(session, body, t"tools", renamed = true))
      json(Http.Ok, session, result(rpc.id, listing.in[Json]))

    case t"prompts/list" =>
      val listing: Prompts = Prompts(merged(session, body, t"prompts", renamed = true))
      json(Http.Ok, session, result(rpc.id, listing.in[Json]))

    case t"resources/list" =>
      val listing: Resources = Resources(merged(session, body, t"resources", renamed = false))
      json(Http.Ok, session, result(rpc.id, listing.in[Json]))

    case t"resources/templates/list" =>
      val listing: Templates =
        Templates(merged(session, body, t"resourceTemplates", renamed = false))

      json(Http.Ok, session, result(rpc.id, listing.in[Json]))

    case t"tools/call" | t"prompts/get" =>
      rpc.params.let { params => safely(params.as[Named]) }.or(Unset) match
        case Named(name) =>
          name.cut(t"_", 2) match
            case List(tool, rest) if tools.exists(_(0) == tool) =>
              val port: Int = tools.seek(_(0) == tool).let(_(1)).or(0)
              val params = rpc.params.let(_.updateDynamic("name")(rest))
              val renamed: Text = rpc.copy(params = params).in[Json].show
              val silent: Text = failure(rpc.id, InternalError, t"$tool did not answer")

              call(session, tool, port, renamed).lay(json(Http.Ok, session, silent)): text =>
                json(Http.Ok, session, text)

            case _ =>
              json(Http.Ok, session, failure(rpc.id, MethodNotFound, t"No tool named $name"))

        case _ =>
          json(Http.Ok, session, failure(rpc.id, InvalidRequest, t"No name was given"))

    case t"resources/read" =>
      // Whichever tool answers without an error has the resource.
      val answers: List[Text] =
        tools.bind: (tool, port) => call(session, tool, port, body).let(List(_)).or(Nil)

      def ok(text: Text): Boolean = safely(text.read[Json].error).absent

      val absent: Text = failure(rpc.id, MethodNotFound, t"No tool has that resource")

      answers.seek(ok(_)).lay(json(Http.Ok, session, absent)): text => json(Http.Ok, session, text)

    case other =>
      json(Http.Ok, session, failure(rpc.id, MethodNotFound, t"The method $other is not known"))

  // The listing under `key` of every tool's answer to `body`, each entry's `name` prefixed with
  // the tool's where names are the tool's own; a tool which does not answer lists nothing.
  private def merged(session: Optional[Text], body: Text, key: Text, renamed: Boolean): List[Json] =
    tools.bind: (tool, port) =>
      call(session, tool, port, body).lay(Nil: List[Json]): text =>
        val entries: List[Json] = safely(text.read[Json].result.apply(key).as[List[Json]]).or(Nil)

        entries.map: (entry: Json) =>
          if renamed
          then safely(entry.updateDynamic("name")(t"${tool}_${entry.name.as[Text]}")).or(entry)
          else entry

  // Posts `body` to the tool's `/mcp`, under this client's session with the tool, opened first
  // if there is none: the tool's answer, or nothing if the tool does not serve MCP.
  private def call(session: Optional[Text], tool: Text, port: Int, body: Text): Optional[Text] =
    val upstreams: TrieMap[Text, Text] = session.let(sessions.getOrElse(_, TrieMap())).or(TrieMap())

    val id: Optional[Text] =
      upstreams.get(tool).map(Optional(_)).getOrElse(initialise(port)).tap: id =>
        id.let: id => upstreams(tool) = id

    id.let(send(port, Http.Post, _, body))

  // Opens a session with a tool: the id it gives, or nothing if it does not serve MCP.
  private def initialise(port: Int): Optional[Text] =
    val client: Text = t"""{"name":"pyrocosm","version":"$version"}"""

    val parameters: Text =
      t"""{"protocolVersion":"${Aggregator.protocol}","capabilities":{},"clientInfo":$client}"""

    val initialize: Text =
      Rpc(id = 0.in[Json], method = t"initialize", params = parameters.read[Json]).in[Json].show

    val response: Optional[Http.Response] = exchange(port, Http.Post, headers(Unset), initialize)

    response.let: response =>
      if response.status.code != 200 then Unset else
        response.textHeaders.seek(_.key.lower == sessionHeader).let(_.value).let: id =>
          send(port, Http.Post, id, Rpc(method = t"notifications/initialized").in[Json].show)
          id

  private def headers(session: Optional[Text]): List[Http.Header] =
    val accept: Text = t"application/json, text/event-stream"

    List(Http.Header(t"content-type", t"application/json"), Http.Header(t"accept", accept)) +
      session.lay(Nil: List[Http.Header]): id => List(Http.Header(sessionHeader, id))

  // One exchange with a tool's `/mcp`.
  private def exchange
    ( port: Int, method: Http.Method, headers: List[Http.Header], body: Optional[Text] )
  :   Optional[Http.Response] =

    val data: Data = body.let(_.in[Data]).or(Data())

    val request: Http.Request =
      Http.Request(method, 1.1, Loopback.loopback, t"/mcp", headers, () => Stream(data))

    Loopback.exchange(port, request)

  // One request to a tool's `/mcp` under a session: the body of its answer, or nothing.
  private def send(port: Int, method: Http.Method, session: Text, body: Optional[Text])
  :   Optional[Text] =

    exchange(port, method, headers(session), body).let(Loopback.text(_))
