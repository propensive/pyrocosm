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

import java.util.concurrent as juc

// Excluded from the umbrella: `Control` (coaxial), `Origin` and `Service`, which would outrank
// this package's own definitions.
import soundness.{Control as _, Origin as _, Service as _, *}

import perihelion.{Message, wsClient, wsSessional}

import attributives.textAttributive
import codepages.utf8Codepage
import denominative.capped
import dysasymptotics.{linearAccess, linearSize}
import formatting.compactCssFormatting
import htmlDoms.whatwg
import htmlDoms.whatwg.*
import httpBackends.javaNetHttp
import internetAccess.online
import logging.silentLogging
import nomenclature.CssClass.nominative
import strategies.throwUnsafely

// The machine's one web origin, which the hub serves: every tool's front-end under it, at
// `/<tool>/`, reached by the port the tool's node advertises; a page naming the tools at `/`;
// and the stylesheet. A request for a tool's path is passed to the tool's own server as it is
// — the tool's front-end knows its prefix, and names its paths under it — and the response
// back; a browser's socket is tunnelled to the tool's. Nothing is rendered here: a tool's
// pages are its own, and the hub knows only which tool is which.
class Portal(hub: Hub, port: Int, theme: WebTheme = WebTheme.default)
  ( using monitor: Monitor, probate: Probate, errorPage: WebserverErrorPage ):

  @volatile
  private var service: Optional[urticose.Service] = Unset

  // The port actually served on, once `serve` has bound one.
  @volatile
  private var bound: Optional[Int] = Unset

  def serving: Optional[Int] = bound

  private lazy val stylesheet: Text =
    PyrocosmPage(Interface(Nil, Nil), HtmlRenderer(), theme).css.show

  // The headers which describe one hop, not the message, which a proxy does not pass on.
  private val hopByHop: Set[Text] =
    Set(t"connection", t"keep-alive", t"proxy-authenticate", t"proxy-authorization", t"te",
        t"trailer", t"transfer-encoding", t"upgrade", t"host")

  private def endToEnd(header: Http.Header): Boolean = !hopByHop.has(header.key.lower)

  // The port the tool of this name serves its front-end on, on this machine, if it does.
  private def upstream(tool: Text): Optional[Int] =
    hub.nodes().seek { advert => advert.node.tool == tool && advert.node.shares(hub.node) } match
      case advert: Advert => advert.services.seek(_.name == t"serve").let(_.port).or(Unset)
      case _              => Unset

  private def websocket(request: Http.Request): Boolean =
    request.textHeaders.exists { header => header.key.lower == t"upgrade" && header.value.lower == t"websocket" }

  // Binds the origin and serves it in the background: the port, or nothing if it could not be
  // bound.
  def serve(): Optional[Int] =
    val started: Optional[urticose.Service] = safely:
      SocketServer(port).handle:
        val request0: Http.Request = summon[Http.Request]
        val target: Text = request0.target
        val path: Text = target.cut(t"?").at(Prim).or(t"/")

        path match
          case t"/" | t"/index.html" =>
            Http.Ok(List(Http.Header(t"content-type", t"text/html; charset=utf-8")), Http.Body.Fixed(landing.in[Data]))

          case t"/pyrocosm.css" =>
            Http.Ok(List(Http.Header(t"content-type", t"text/css; charset=utf-8")), Http.Body.Fixed(stylesheet.in[Data]))

          case _ =>
            val tool: Text = path.cut(t"/").at(Sec).or(t"")

            upstream(tool) match
              case port: Int =>
                if websocket(request0) then tunnel(tool, port, request0) else forward(tool, port, request0)

              case _ =>
                Http.Response(Http.NotFound)(t"No tool named $tool is running on this machine")

    service = started
    bound = started.let(_.port)
    bound

  def stop(): Unit =
    service.let(_.cancel())
    service = Unset
    bound = Unset

  // A request passed to the tool's server, and its response passed back.
  private def forward(tool: Text, port: Int, request: Http.Request): Http.Response =
    val url: Text = t"http://127.0.0.1:${port.show}${request.target}"
    val headers: List[Http.Header] = request.textHeaders.filter(endToEnd(_))
    val response: Optional[Http.Response] = safely(summon[Http.Backend].request(url, request.method, headers, request.body))

    response.lay(Http.Response(Http.BadGateway)(t"The $tool front-end did not answer")): response =>
      response.status(response.textHeaders.filter(endToEnd(_)), Http.Body.Flowing(() => response.body.stream))

  // A browser's socket tunnelled to the tool's: what the browser sends goes to the tool, what
  // the tool sends comes back, and either end closing closes the other. The tool's end is lent
  // by a session scope on a task of its own, which reads it message by message (one refill
  // window is one message, as coaxial's own `react` reads) until the tool closes; the browser's
  // handler waits for that connection before it sends.
  private def tunnel(tool: Text, port: Int, request: Http.Request): Http.Response =
    val url: Websocket.Url = t"ws://127.0.0.1:${port.show}${request.target}".as[Websocket.Url]
    val transmissible: Message is Transmissible = summon[Message is Transmissible]
    val upstream: Promise[Websocket.Connection] = Promise()

    val handler: (state: Unit) ?=> Message => coaxial.Control[Unit] = message =>
      safely(upstream.await(Portal.patience)).let(_.send(transmissible.serialize(message)))
      coaxial.Control.Continue(())

    val websocket: Websocket[Message, Unit] = Websocket(request, (), identity, handler)

    async:
      try
        url.session: connection ?=>
          upstream.offer(connection)
          val input = connection.messages
          val demand = Credit(summon[Buffering].capacity(Substrate.Bytes))
          var reading: Boolean = true

          while reading do input.refill(demand) match
            case count: Int =>
              if count > 0 then
                val data: Data = input.lend { region => range => region.materialize(range.capped(count)) }
                input.skip(count)
                websocket.channel.send(Message.Text(data.utf8))

            case _ =>
              reading = false

      finally safely(websocket.channel.close())

    // The browser going closes the tool's socket, which ends the task above.
    async:
      safely(websocket.task.await())
      safely(upstream.await(Portal.patience)).let { connection => safely(connection.close()) }

    websocket

  // The page at `/`: the tools serving a front-end on this machine, each a link to its path
  // under this origin, and those on other machines, each a link to where it is.
  private def landing: Text =
    val adverts: List[Advert] = hub.nodes().filter(_.node.tool != Wire.tool)
    val local: List[Advert] = adverts.filter(_.node.shares(hub.node))
    val remote: List[Advert] = adverts.filter(!_.node.shares(hub.node))

    def item(advert: Advert, href: Optional[Text]): Html of "li" =
      val label: Text = t"${advert.node.tool} ${advert.node.version}"
      href.lay(Li(label)) { href => Li(A(href = href)(label)) }

    def listing(title: Text, items: List[Html of "li"]): Html of Flow =
      if items.nil then Html.Fragment[Flow]() else Html.Fragment[Flow](H2(title), Ul(items*))

    val here: List[Html of "li"] =
      local.filter(_.services.exists(_.name == t"serve")).map { advert => item(advert, t"/${advert.node.tool}/") }

    val elsewhere: List[Html of "li"] =
      remote.map { advert => item(advert, advert.url.let { url => t"$url${advert.node.tool}/" }) }

    val page: Html of "html" =
      Html
        ( Head(Title(t"Pyrocosm"), Link.Stylesheet(href = t"/pyrocosm.css")),
          Body
            ( Nav(`class` = List(TopMenu.menuClass, HtmlRenderer.cls(t"pyro-menubar")))
                ( Div(`class` = HtmlRenderer.cls(t"pyro-menubar-inner"))
                    ( A(href = t"/", `class` = HtmlRenderer.cls(t"pyro-wordmark"))(t"Pyrocosm") ) ),
              Main
                ( H1(t"Pyrocosm on ${hub.node.machine}"),
                  listing(t"Tools on this machine", here),
                  listing(t"Tools on other machines", elsewhere) ) ) )

    t"<!DOCTYPE html>${page.show}"

object Portal:
  // How long a browser's first message waits for the tool's socket to open.
  val patience: Duration = 5.0*Second
