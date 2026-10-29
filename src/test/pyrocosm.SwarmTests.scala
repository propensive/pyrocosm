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

import soundness.{Service as _, Standing as _, Status as _, Step as _, *}

import Journal.{Event, Party}
import perihelion.{Message, wsClient, wsSessional}

import alphabets.hexLowerCase
import codepages.utf8Codepage
import denominative.dysasymptotics.linearSize
import errorDiagnostics.emptyDiagnostics
import httpBackends.javaNetHttp
import internetAccess.online
import logging.silentLogging
import probates.cancelProbate
import proscenium.List
import strategies.throwUnsafely
import threads.platformThreads
import webserverErrorPages.minimalErrorPage

// A later build's wire, for the acceptance tests: the same messages and one more, and an advert
// with one more, optional, field. Derived here, in the unchecked test module, as `Wire` is in
// `bus`; each derivation in an object of its own, as together they exceed what one class may
// hold.
object Later:
  import Scalars.given

  enum Wire:
    case Beat(sent: Instant over Unix, load: Optional[Double])
    case Ping(id: Uuid, sent: Instant over Unix, note: Text)
    case Pong(id: Uuid, received: Instant over Unix, from: Text)
    case Request(id: Uuid, command: Text, argument: Text)
    case Reply(id: Uuid, ok: Boolean, text: Text)
    case Snapshot(self: pyrocosm.Advert, known: List[pyrocosm.Advert])
    case Advertised(origin: Text, sequence: Long, sent: Instant over Unix, hops: Int, advert: pyrocosm.Advert)
    case Left(origin: Text, sequence: Long, sent: Instant over Unix, hops: Int, node: Node)
    case Updated(origin: Text, sequence: Long, sent: Instant over Unix, hops: Int, node: Node, activity: Text)
    case Ended(origin: Text, sequence: Long, sent: Instant over Unix, hops: Int, node: Node, activity: Text)
    case Tool(origin: Text, sequence: Long, sent: Instant over Unix, hops: Int, tool: Text, kind: Text, payload: Data)
    case Extra(note: Text)

  import Channel.derivation.throwing

  // What a v2 build reads: its own form first, then this build's.
  lazy val accepting: Tel.Acceptance.Typed[(Wire, pyrocosm.Wire)] = Tel.Acceptance[(Wire, pyrocosm.Wire)]()
  lazy val acceptance: Tel.Acceptance = accepting.acceptance

  def write(message: Wire, theirs: Tel.Acceptance): Optional[Data] =
    try message.fulfil(theirs).let(_.document) catch case _: Exception => Unset

  def read(data: Data): Optional[Wire | pyrocosm.Wire] =
    try accepting.read(data) catch case _: Exception => Unset

object Adverts:
  import Scalars.given

  case class Advert
    ( node:         Node,
      services:     List[Service],
      capabilities: List[Text],
      os:           Text,
      arch:         Text,
      cores:        Int,
      load:         Optional[Double],
      gateway:      Boolean,
      region:       Optional[Text] )

  import Channel.derivation.throwing

  lazy val later: Tel.Acceptance.Typed[Tuple1[Advert]] = Tel.Acceptance[Tuple1[Advert]]()
  lazy val current: Tel.Acceptance.Typed[Tuple1[pyrocosm.Advert]] = Tel.Acceptance[Tuple1[pyrocosm.Advert]]()

  def writeAdvert(advert: pyrocosm.Advert, theirs: Tel.Acceptance): Optional[Data] =
    try advert.fulfil(theirs).let(_.document) catch case _: Exception => Unset

  def writeAdvertV2(advert: Advert, theirs: Tel.Acceptance): Optional[Data] =
    try advert.fulfil(theirs).let(_.document) catch case _: Exception => Unset

  def readAdvert(data: Data): Optional[pyrocosm.Advert] =
    try current.read(data) catch case _: Exception => Unset

  def readAdvertV2(data: Data): Optional[Advert] =
    try later.read(data) catch case _: Exception => Unset

// A tool's own events, for the payload tests.
case class Greeting(from: Text, count: Int)
case class Farewell(reason: Text, count: Int, extra: Text)

object SwarmTests extends Suite(m"Pyrocosm swarm tests"):
  private val moment: Instant over Unix = Instant.of[Unix](1791108000000L)
  private val id: Uuid = Uuid(0x3f2a91c000000000L, 0L)

  private val node: Node =
    Node(t"linux-box", t"ab"*32, t"fume", t"0.8.0", 4242L, moment, List(t"192.168.1.20", t"linux-box"))

  private val advert: Advert =
    Advert(node, List(Service(t"serve", 8090, t"http://192.168.1.20:8090/")), List(t"linux"), t"Linux", t"amd64", 16, 0.5, true)

  private val activity: Activity =
    Activity
      ( t"run-1", Inline.text(t"alpha tests"), Status.Fraction(0.25),
        Inline.Destination.External(t"http://192.168.1.20:8090/open?activity=run-1"),
        List(Inline.Toned(Tone.Success, Inline.text(t"green so far"))) )

  private def envelope(event: Bus.Event): Envelope = Envelope(node.key, 7L, moment, 0, event)

  // Waits, for a few seconds at most, for `condition` to hold.
  private def await(attempts: Int)(condition: => Boolean)(using Monitor): Unit =
    if attempts > 0 && !condition then
      snooze(0.2*Second)
      await(attempts - 1)(condition)

  // What kind of event each entry's message tells of.
  private def kinds(entries: List[Journal.Entry]): scala.List[Text] =
    entries.stdlib.map(_.message.text).map: text =>
      if text.starts(t"→ carry updated") then t"sent-updated"
      else if text.starts(t"← carry updated") then t"received-updated"
      else if text.starts(t"→ carry ended") then t"sent-ended"
      else if text.starts(t"→ beat") then t"sent-beat"
      else if text.starts(t"← beat") then t"received-beat"
      else if text.starts(t"keeping the link") then t"linked"
      else if text.starts(t"lost the link") then t"lost"
      else if text.starts(t"registered") then t"registered"
      else if text.starts(t"this node is the machine's gateway") then t"gateway"
      else if text.starts(t"advertising this machine") then t"advertised"
      else if text.starts(t"no longer advertising") then t"withdrawn"
      else if text.contains(t"is on the swarm") then t"appeared"
      else if text.contains(t"has left the swarm") then t"vanished"
      else t"other"

  // What the two nodes of one machine showed.
  private case class Hubbed
    ( seen:      (Int, Int, Int),
      leaves:    Int,
      linked:    (Boolean, Boolean),
      arrived:   Optional[Activity],
      coalesced: Int,
      ended:     Boolean,
      greeted:   Optional[Greeting],
      oversized: Boolean,
      gateway:   (Boolean, Boolean, Boolean),
      survived:  Boolean,
      orphaned:  Int,
      relinked:  Boolean )

  private case class Served
    ( prefixed: Boolean,
      content:  Boolean,
      listed:   Boolean,
      direct:   Boolean,
      missing:  Int,
      pong:     Optional[Text],
      session:  Boolean,
      tools:    Optional[Text],
      called:   Optional[Text],
      deleted:  Int,
      patched:  Optional[Int] )

  // A tool's own MCP server, as its front-end's fallback answers `/mcp`: one tool, `hello`,
  // which says who called it; a session named by the header the protocol uses.
  private def mcpFake(request: Http.Request): Optional[Http.Response] =
    import codepages.utf8Codepage
    if request.target != t"/mcp" || request.method != Http.Post then Unset else
      val body: Text = request.body().memoize.utf8
      val rpc: Optional[Aggregator.Rpc] = safely(body.read[Json].as[Aggregator.Rpc])
      val id: Text = rpc.let(_.id).or(Unset).let { json => safely(json.as[Int]).let(_.show) }.or(Unset).or(t"null")

      def answer(result: Text): Http.Response =
        Http.Ok
          ( List(Http.Header(t"content-type", t"application/json"), Http.Header(t"mcp-session-id", t"alpha-session")),
            Http.Body.Fixed(t"""{"jsonrpc":"2.0","id":$id,"result":$result}""".in[Data]) )

      rpc.let(_.method).or(t"") match
        case t"initialize" => answer(t"""{"protocolVersion":"2025-06-18","capabilities":{},"serverInfo":{"name":"alpha","version":"0"}}""")
        case t"tools/list" => answer(t"""{"tools":[{"name":"hello","inputSchema":{"type":"object"}}]}""")
        case t"tools/call" =>
          val name: Text = rpc.let(_.params).or(Unset).let { params => safely(params.as[Aggregator.Named]).let(_.name) }.or(Unset).or(t"?")
          answer(t"""{"content":[{"type":"text","text":"hello from alpha: $name"}]}""")
        case method if method.starts(t"notifications/") => Http.Response(Http.Accepted)()
        case _ => answer(t"{}")

  private case class Relayed
    ( seen:      (Int, Int, Int, Int),
      distinct:  Boolean,
      hops:      (List[Int], List[Int], List[Int]),
      forgotten: (Int, Int) )

  // What the invitation showed.
  private case class Invited
    ( words:      Int,
    read:         Boolean,
    advertised:   Boolean,
    nearby:       List[Text],
    stranger:     List[Text],
    joined:       Boolean,
    again:        Boolean,
    declared:     Boolean,
    admitted:     Boolean,
    fumeAdmits:   Boolean,
    withdrawn:    Boolean,
    revoked:      Int )

  def run(): Unit =
    suite(m"The wire"):
      test(m"a beat survives being written and read"):
        Wire.write(Wire.Beat(moment, 1.25), Wire.acceptance).let(Wire.read(_))
      . assert(_ == Wire.Beat(moment, 1.25))

      test(m"a ping and a pong survive being written and read"):
        val ping: Wire = Wire.Ping(id, moment, t"hello there")
        val pong: Wire = Wire.Pong(id, moment, node.key)
        (Wire.write(ping, Wire.acceptance).let(Wire.read(_)), Wire.write(pong, Wire.acceptance).let(Wire.read(_)))
      . assert(_ == (Wire.Ping(id, moment, t"hello there"), Wire.Pong(id, moment, node.key)))

      test(m"a snapshot carries an advert whole"):
        Wire.write(Wire.Snapshot(advert, List(advert)), Wire.acceptance).let(Wire.read(_))
      . assert(_ == Wire.Snapshot(advert, List(advert)))

      test(m"an updated activity, with its inlines, status and destination, survives"):
        val carry: Wire = Wire.carry(envelope(Bus.Event.Updated(node, Bus.encode(activity))))
        Wire.write(carry, Wire.acceptance).let(Wire.read(_)).let(Wire.envelope(_)).let(_.event) match
          case Bus.Event.Updated(_, text) => Bus.decode(text)
          case _                          => Unset
      . assert(_ == activity)

      test(m"an activity's internal destination is dropped before it travels"):
        val internal: Activity = activity.copy(destination = Inline.Destination.Internal(Action(t"open")))
        Bus.decode(Bus.encode(internal)).let(_.destination)
      . assert(_ == Unset)

      test(m"every other kind of event survives"):
        val events: List[Bus.Event] =
          List
            ( Bus.Event.Advertised(advert), Bus.Event.Left(node), Bus.Event.Ended(node, t"run-1"),
              Bus.Event.Tool(t"fume", t"greeting", Bus.payload(Greeting(t"fume", 3))) )

        events.map { event => Wire.write(Wire.carry(envelope(event)), Wire.acceptance).let(Wire.read(_)) }
      . assert: read =>
          read.stdlib.zip(List(Bus.Event.Advertised(advert), Bus.Event.Left(node), Bus.Event.Ended(node, t"run-1")).stdlib)
            .forall { (read, event) => read == Wire.carry(envelope(event)) }
          && read.stdlib.last.present

      test(m"an acceptance names the protocol's one form, and survives being sent"):
        (Wire.acceptance.alternatives.stdlib.length, Wire.offered(Wire.offer).let(_ == Wire.acceptance))
      . assert(_ == (1, true))

      test(m"a message is not an acceptance, and is not taken for one"):
        Wire.write(Wire.Beat(moment, Unset), Wire.acceptance).let(Wire.offered(_)).absent
      . assert(_ == true)

      test(m"the schema has a stable signature"):
        Wire.signature.length
      . assert(_ == 32)

    suite(m"Evolution"):
      test(m"a later build reads what this one writes"):
        Wire.write(Wire.Beat(moment, 0.5), Later.acceptance).let(Later.read(_)).present
      . assert(_ == true)

      // A message more is a base of its own: the later build must downgrade to this one's
      // form to be read here, as `pyrocosm.Wire.write` of the message it has in common does.
      test(m"a later build with a message more cannot write to this one without downgrading"):
        Later.write(Later.Wire.Beat(moment, 0.5), Wire.acceptance).absent
      . assert(_ == true)

      test(m"a message this build does not know is not written to it"):
        Later.write(Later.Wire.Extra(t"new"), Wire.acceptance).absent
      . assert(_ == true)

      test(m"a later build with an optional field more reads this build's advert, field unset"):
        Adverts.writeAdvert(advert, Adverts.later.acceptance).let(Adverts.readAdvertV2(_)).let(_.region)
      . assert(_ == Unset)

      test(m"this build reads a later advert, dropping the field it does not know"):
        val later = Adverts.Advert(node, advert.services, advert.capabilities, t"Linux", t"amd64", 16, 0.5, true, t"eu-west")
        Adverts.writeAdvertV2(later, Adverts.current.acceptance).let(Adverts.readAdvert(_))
      . assert(_ == advert)

    suite(m"Activities on the bus"):
      test(m"an activity of every status survives the bus"):
        val statuses: List[Status] =
          List
            ( Status.Fraction(0.5), Status.Indeterminate(), Status.Reckoning(3, 10), Status.Reckoning(3, Unset),
              Status.Standing(Standing.Running), Status.Elapsed(1.5), Status.Remaining(2.5),
              Status.Steps(List(Step(Inline.text(t"compile"), Standing.Succeeded, Inline.text(t"done")))) )

        statuses.map { status => Bus.decode(Bus.encode(activity.copy(status = status))).let(_.status) }
      . assert(_ == List(Status.Fraction(0.5), Status.Indeterminate(), Status.Reckoning(3, 10), Status.Reckoning(3, Unset),
              Status.Standing(Standing.Running), Status.Elapsed(1.5), Status.Remaining(2.5),
              Status.Steps(List(Step(Inline.text(t"compile"), Standing.Succeeded, Inline.text(t"done"))))))

    suite(m"Payloads"):
      test(m"a tool's own event round-trips"):
        Bus.parse[Greeting](Bus.payload(Greeting(t"fume", 3)))
      . assert(_ == Greeting(t"fume", 3))

      test(m"a payload of another type is not read as this one"):
        Bus.parse[Greeting](Bus.payload(Farewell(t"bye", 1, t"more"))).absent
      . assert(_ == true)

    suite(m"Filters"):
      val remote: Node = node.copy(identity = t"cd"*32)
      val self: Node = node.copy(tool = t"fury")

      test(m"a filter admits by tool, kind and origin"):
        val greeting = envelope(Bus.Event.Tool(t"fume", t"greeting", Data()))
        ( Swarm.Filter(tools = Set[Text](t"fume")).admits(node, greeting, self),
          Swarm.Filter(tools = Set[Text](t"flame")).admits(node, greeting, self),
          Swarm.Filter(kinds = Set[Text](t"greeting")).admits(node, greeting, self),
          Swarm.Filter(kinds = Set[Text](t"updated")).admits(node, greeting, self),
          Swarm.Filter(origins = Set[Text](node.key)).admits(node, greeting, self) )
      . assert(_ == (true, false, true, false, true))

      test(m"a filter tells this machine's nodes from others'"):
        val updated = envelope(Bus.Event.Updated(node, Bus.encode(activity)))
        ( Swarm.Filter(remote = false).admits(node, updated, self),
          Swarm.Filter(local = false).admits(node, updated, self),
          Swarm.Filter(local = false).admits(remote, updated, self) )
      . assert(_ == (true, false, true))

    suite(m"The registry"):
      // The hub's record, in a registry of the test's own, so that the machine's is untouched.
      val runtime: Text = java.nio.file.Files.createTempDirectory("pyrocosm-registry").nn.toString.tt

      given Environment = name =>
        if name == t"XDG_RUNTIME_DIR" then runtime
        else Optional(java.lang.System.getenv(name.s)).let(_.tt)

      val registry: Registry = Registry(Registry.directory)
      val own: Long = java.lang.ProcessHandle.current.nn.pid
      val begun: Long = java.lang.System.currentTimeMillis - 1000L

      test(m"a live hub's record is read back whole, gateway port included"):
        registry.registerHub(Registry.Record(t"pyrocosm", t"0.2.0", own, begun, 40003, t"cd"*32, 8093))
        registry.hub().let { record => (record.tool, record.version, record.port, record.gateway, record.identity) }
      . assert(_ == (t"pyrocosm", t"0.2.0", 40003, 8093, t"cd"*32))

      test(m"a record is replaced by the next, and removed"):
        registry.registerHub(Registry.Record(t"pyrocosm", t"0.2.0", own, begun, 40004, t"cd"*32, Unset))
        val replaced: Optional[(Int, Optional[Int])] = registry.hub().let { record => (record.port, record.gateway) }
        registry.unregisterHub()
        (replaced, registry.hub().present)
      . assert(_ == ((40004, Unset), false))

      test(m"a dead hub's record is dropped, and deleted"):
        registry.registerHub(Registry.Record(t"pyrocosm", t"0.1.0", 2147483646L, begun, 40002, t"ab"*32, 8093))
        val found: Boolean = registry.hub().present
        val remaining: Boolean = java.nio.file.Files.exists(java.nio.file.Path.of(runtime.s, "pyrocosm", "swarm", "hub.tel"))
        (found, remaining)
      . assert(_ == (false, false))

      test(m"the hub's lock is held by one process at a time"):
        val first: Optional[java.nio.channels.FileLock] = registry.lockHub()
        val second: Optional[java.nio.channels.FileLock] = registry.lockHub()
        first.let(_.release())
        val third: Optional[java.nio.channels.FileLock] = registry.lockHub()
        third.let(_.release())
        (first.present, second.present, third.present)
      . assert(_ == (true, false, true))

    suite(m"Starting the hub"):
      // A launcher such as Burdock's loads a tool's libraries through a URL class loader of its
      // own, from a cache, and leaves only itself on `java.class.path`; the hub must be started
      // from the libraries, which only that loader knows.
      val directory: java.nio.file.Path = java.nio.file.Files.createTempDirectory("pyrocosm-classpath").nn
      val jar: java.nio.file.Path = directory.resolve("library.jar").nn
      java.nio.file.Files.write(jar, scala.Array[Byte]())

      val loader: java.net.URLClassLoader =
        java.net.URLClassLoader(scala.Array(jar.toUri.nn.toURL.nn, directory.toUri.nn.toURL.nn), ClassLoader.getPlatformClassLoader)

      test(m"the hub's classpath leads with the jars a URL class loader loads, each once"):
        val entries: List[Text] = Hubs.classpath(loader)
        (entries.keep(2), entries.count(_ == jar.toString.tt))
      . assert(_ == (List(jar.toString.tt, directory.toString.tt), 1))

      test(m"the hub's classpath ends with the JVM's own"):
        val system: List[Text] =
          List(scala.collection.immutable.ArraySeq.unsafeWrapArray(java.lang.System.getProperty("java.class.path").nn.split(java.io.File.pathSeparator).nn).toList.map(_.nn.tt)*)

        system.all(Hubs.classpath(loader).has(_))
      . assert(_ == true)

      test(m"the hub's classpath holds the jar the hub's own class came from"):
        val location: Text =
          java.nio.file.Path.of(classOf[Hub].getProtectionDomain.nn.getCodeSource.nn.getLocation.nn.toURI).nn.toString.tt

        Hubs.classpath().has(location)
      . assert(_ == true)

    suite(m"A hub and two leaves"):
      // A hub and two leaves in this JVM, as the `pyrocosm` process and two tools' daemons
      // would be on one machine, with a registry of the test's own, so that the machine's is
      // untouched; the identity is the machine's real one, as it must be for them to admit
      // each other. Each leaf links to the hub alone, and knows the other through it.
      val hubbed: Hubbed =
        supervise:
          val runtime: Text = java.nio.file.Files.createTempDirectory("pyrocosm-hub").nn.toString.tt

          given Environment = name =>
            if name == t"XDG_RUNTIME_DIR" then runtime
            else Optional(java.lang.System.getenv(name.s)).let(_.tt)

          val port: Int = Port[Tcp]().number
          val start: Long = Journal.daemon.latest
          def logged: scala.List[Text] = kinds(Journal.daemon.since(start))

          val hub: Hub = Swarm.hub(t"0.1.0", Nil, port).or(panic(m"another process is the hub"))
          val alpha: Swarm = Swarm.start(t"alpha", t"0.1.0", () => List(Service(t"serve", 8081)), Nil, port)
          val beta: Swarm = Swarm.start(t"beta", t"0.1.0", () => Nil, Nil, port)

          await(50)(alpha.nodes().size == 3 && beta.nodes().size == 3 && hub.nodes().size == 3)
          val seen: (Int, Int, Int) = (alpha.nodes().size, beta.nodes().size, hub.nodes().size)
          await(25)(hub.leaves.size == 2)
          val leaves: Int = hub.leaves.size
          val linked: (Boolean, Boolean) = (alpha.hub.present, beta.hub.present)

          // A hundred updates in a hundred milliseconds reach beta, through the hub, as the
          // last of them.
          val live: Live[List[Activity]] = Live(Nil)
          alpha.expose(live)

          (1 to 100).foreach: index =>
            live() = List(activity.copy(status = Status.Fraction(index/100.0)))
            Thread.sleep(1)

          await(25)(beta.activities().exists(_(1).status == Status.Fraction(1.0)))
          val arrived: Optional[Activity] = beta.activities().head.let(_(1))
          val coalesced: Int = logged.count(_ == t"sent-updated")

          live() = Nil
          await(25)(beta.activities().nil)
          val ended: Boolean = beta.activities().nil

          // A tool's own event reaches a subscriber on the other leaf, and an oversized one
          // is refused.
          val promise: Promise[Greeting] = Promise()

          val subscription = beta.subscribe(Swarm.Filter(kinds = Set[Text](t"greeting"))): (_, envelope) =>
            envelope.event match
              case Bus.Event.Tool(_, _, payload) => Bus.parse[Greeting](payload).let(promise.offer(_))
              case _                             => ()

          alpha.publish(t"greeting", Bus.payload(Greeting(t"alpha", 1)))
          val greeted: Optional[Greeting] = safely(promise.await(3.0*Second))
          subscription.cancel()
          val oversized: Boolean = !alpha.publish(t"big", Data.fill(20000) { i => (i%7).toByte })

          // A leaf asking for the gateway makes the hub the gateway, which every node of the
          // machine then reports; the leaf which asked going does not give it up.
          alpha.wantGateway(port, Unset)
          await(25)(alpha.listening.present)
          val gateway: (Boolean, Boolean, Boolean) = (alpha.listening.present, beta.listening.present, hub.listening.present)

          Swarm.stop(t"alpha")
          await(25)(beta.nodes().size == 2)
          val survived: Boolean = hub.listening.present && beta.nodes().size == 2

          // The hub going takes every other node with it, and a hub started again is found.
          hub.stop()
          await(25)(beta.nodes().size == 1)
          val orphaned: Int = beta.nodes().size

          val again: Hub = Swarm.hub(t"0.1.0", Nil, port).or(panic(m"another process is the hub"))
          await(50)(beta.hub.present && beta.nodes().size == 2)
          val relinked: Boolean = beta.hub.present && beta.nodes().size == 2

          again.stop()
          Swarm.stop(t"beta")

          Hubbed(seen, leaves, linked, arrived, coalesced, ended, greeted, oversized, gateway, survived, orphaned, relinked)

      test(m"each leaf sees the hub and the other leaf, linked to the hub alone"):
        (hubbed.seen, hubbed.leaves, hubbed.linked)
      . assert(_ == ((3, 3, 3), 2, (true, true)))

      test(m"an activity's updates arrive, coalesced to a few"):
        (hubbed.arrived.let(_.status), hubbed.coalesced <= 5)
      . assert(_ == (Status.Fraction(1.0), true))

      test(m"an activity removed is ended on the other leaf"):
        hubbed.ended
      . assert(_ == true)

      test(m"a tool's own event reaches a subscriber, and an oversized one is refused"):
        (hubbed.greeted, hubbed.oversized)
      . assert(_ == (Greeting(t"alpha", 1), true))

      test(m"the hub is the gateway a leaf asks for, and keeps it when that leaf goes"):
        (hubbed.gateway, hubbed.survived)
      . assert(_ == ((true, true, true), true))

      test(m"a leaf forgets every node with its hub, and finds a hub started again"):
        (hubbed.orphaned, hubbed.relinked)
      . assert(_ == (1, true))

    suite(m"Three machines"):
      // Three machines in this JVM, each with an identity of its own — `Peer.identity` makes a
      // certificate under whichever state home a node's environment names — and a registry and
      // a hub of its own. Machine A has two leaves, and its hub is its gateway; machines B and
      // C each have one leaf, and their hubs keep a link to A's, which relays between its own
      // machine and each of them, and between the two of them, as the hub of its spokes.
      val relayed: Relayed =
        supervise:
          def temporary(name: String): Text = java.nio.file.Files.createTempDirectory(name).nn.toString.tt
          val config: Text = temporary("pyrocosm-relay-config")

          def environment(state: Text, runtime: Text): Environment = name =>
            if name == t"XDG_STATE_HOME" then state
            else if name == t"XDG_RUNTIME_DIR" then runtime
            else if name == t"XDG_CONFIG_HOME" then config
            else Optional(java.lang.System.getenv(name.s)).let(_.tt)

          val envA: Environment = environment(temporary("pyrocosm-a-state"), temporary("pyrocosm-a-runtime"))
          val envB: Environment = environment(temporary("pyrocosm-b-state"), temporary("pyrocosm-b-runtime"))
          val envC: Environment = environment(temporary("pyrocosm-c-state"), temporary("pyrocosm-c-runtime"))

          val port: Int = Port[Tcp]().number

          def hub(environment: Environment): Hub =
            Swarm.hub(t"0.1.0", Nil, port)(using Swarm.discovery)(using summon[Monitor], summon[Probate], environment)
            . or(panic(m"another process is the hub"))

          val hubA: Hub = hub(envA)
          val hubB: Hub = hub(envB)
          val hubC: Hub = hub(envC)
          val alpha: Swarm = Swarm.start(t"alpha3", t"0.1.0", () => Nil, Nil, port)(using summon[Monitor], summon[Probate], envA)
          val beta: Swarm = Swarm.start(t"beta3", t"0.1.0", () => Nil, Nil, port)(using summon[Monitor], summon[Probate], envA)
          val gamma: Swarm = Swarm.start(t"gamma3", t"0.1.0", () => Nil, Nil, port)(using summon[Monitor], summon[Probate], envB)
          val delta: Swarm = Swarm.start(t"delta3", t"0.1.0", () => Nil, Nil, port)(using summon[Monitor], summon[Probate], envC)

          await(50)(alpha.nodes().size == 3 && beta.nodes().size == 3 && gamma.nodes().size == 2 && delta.nodes().size == 2)
          alpha.wantGateway(port, Unset)
          await(25)(hubA.listening.present)

          // The token every machine shares here, and A's fingerprint, make the declaration B
          // and C link by; each leaf asks its hub for the link.
          val token: Text = Peer.token(using envA).or(t"")
          val fingerprint: Optional[Data] = Peer.parseFingerprint(hubA.node.identity)
          val a: Machine = Machine(t"a", List(t"127.0.0.1"), port, fingerprint, token, Nil)

          gamma.connect(a)
          await(50)(gamma.connected(t"a"))
          delta.connect(a)
          await(50)(delta.connected(t"a"))

          await(50)(alpha.nodes().size == 7 && beta.nodes().size == 7 && gamma.nodes().size == 7 && delta.nodes().size == 7)
          val seen: (Int, Int, Int, Int) = (alpha.nodes().size, beta.nodes().size, gamma.nodes().size, delta.nodes().size)
          val distinct: Boolean = Set(alpha.node.identity, gamma.node.identity, delta.node.identity).size == 3

          // An event of A's leaf reaches B's leaf relayed by two hubs; one of B's leaf reaches
          // A's other leaf by two, and C's leaf by three — each once.
          val atGamma = java.util.concurrent.ConcurrentLinkedQueue[Int]()
          val atBeta = java.util.concurrent.ConcurrentLinkedQueue[Int]()
          val atDelta = java.util.concurrent.ConcurrentLinkedQueue[Int]()
          def hopsOf(queue: java.util.concurrent.ConcurrentLinkedQueue[Int]): List[Int] =
            var found: scala.List[Int] = scala.Nil
            val iterator = queue.iterator.nn
            while iterator.hasNext do found = iterator.next.nn :: found
            List(found.reverse*)

          val subscriptions = List
            ( gamma.subscribe(Swarm.Filter(kinds = Set[Text](t"from-a"))) { (_, envelope) => atGamma.add(envelope.hops) },
              beta.subscribe(Swarm.Filter(kinds = Set[Text](t"from-b"))) { (_, envelope) => atBeta.add(envelope.hops) },
              delta.subscribe(Swarm.Filter(kinds = Set[Text](t"from-b"))) { (_, envelope) => atDelta.add(envelope.hops) } )

          alpha.publish(t"from-a", Bus.payload(Greeting(t"alpha", 1)))
          gamma.publish(t"from-b", Bus.payload(Greeting(t"gamma", 2)))
          await(25)(!atGamma.isEmpty && !atBeta.isEmpty && !atDelta.isEmpty)
          snooze(1.0*Second)
          val hops: (List[Int], List[Int], List[Int]) = (hopsOf(atGamma), hopsOf(atBeta), hopsOf(atDelta))
          subscriptions.each(_.cancel())

          // B hangs up: A's nodes, and C's, forget it.
          gamma.disconnect(t"a")
          await(50)(beta.nodes().size == 5 && delta.nodes().size == 5)
          val forgotten: (Int, Int) = (beta.nodes().size, delta.nodes().size)

          Swarm.stop(t"delta3")
          Swarm.stop(t"gamma3")
          Swarm.stop(t"beta3")
          Swarm.stop(t"alpha3")
          hubC.stop()
          hubB.stop()
          hubA.stop()

          Relayed(seen, distinct, hops, forgotten)

      test(m"every node of three machines sees every other, through the hubs"):
        (relayed.seen, relayed.distinct)
      . assert(_ == ((7, 7, 7, 7), true))

      test(m"an event crosses to a linked machine's leaf by two hubs, and to the next by three, once each"):
        relayed.hops
      . assert(_ == (List(2), List(2), List(3)))

      test(m"a machine which hangs up is forgotten on every other"):
        relayed.forgotten
      . assert(_ == (5, 5))

    suite(m"Invitations"):
      // A hub in this JVM is the gateway, its leaf invites, and this JVM joins: the invitation
      // is accepted once, the machine is declared, and once revoked it is refused. The
      // declaration and the granted token are written to a configuration directory of the
      // test's own, so that the user's `machines.tel` is untouched; the hub advertises the
      // machine, as the leaf asks, and the joiner looks for it, over an in-memory mDNS bus, so
      // that nothing is multicast on the network.
      val invited: Invited =
        supervise:
          val start: Long = Journal.daemon.latest
          def logged: scala.List[Text] = kinds(Journal.daemon.since(start))

          val bus: Mdns.Transport.Bus = Mdns.Transport.Bus()

          val inviter: Discovery.Backend =
            Mdns.Responder(() => bus.join(dns"inviter.local", List(ip"127.0.0.1")))

          val joiner: Discovery.Backend =
            Mdns.Responder(() => bus.join(dns"joiner.local", List(ip"127.0.0.2")))

          val config: Text = java.nio.file.Files.createTempDirectory("pyrocosm-config").nn.toString.tt
          val runtime: Text = java.nio.file.Files.createTempDirectory("pyrocosm-invite").nn.toString.tt

          given Environment = name =>
            if name == t"XDG_CONFIG_HOME" then config
            else if name == t"XDG_RUNTIME_DIR" then runtime
            else Optional(java.lang.System.getenv(name.s)).let(_.tt)

          val port: Int = Port[Tcp]().number
          val hub: Hub = Swarm.hub(t"0.1.0", Nil, port)(using inviter).or(panic(m"another process is the hub"))
          val gamma: Swarm = Swarm.start(t"gamma", t"0.1.0", () => Nil, Nil, port)
          await(50)(gamma.hub.present)
          gamma.wantGateway(port, Unset)
          await(25)(gamma.listening.present)

          val made: Invitation = unsafely(gamma.invite(port, 60.0*Second)(using inviter))
          val word: Text = Invitation.encode(made)
          val read: Optional[Invitation] = safely(Invitation.parse(word))

          await(25)(logged.contains(t"advertised"))
          val advertised: Boolean = gamma.advertised.present

          val nearby: List[Text] = read.let(Swarm.locate(_)(using joiner)).or(Nil)
          val other: Data = Peer.parseFingerprint(t"00"*32).or(Data())

          val stranger: List[Text] =
            read.let(_.copy(identity = other)).let(Swarm.locate(_)(using joiner)).or(Nil)

          val local: Optional[Invitation] = read.let(_.copy(hosts = List(t"127.0.0.1")))

          def join(name: Text): Optional[Machine] =
            local.let: invitation => safely(gamma.join(invitation, name))

          val joined: Optional[Machine] = join(t"inviter")
          val again: Optional[Machine] = join(t"again")

          val declared: Boolean =
            Machine.shared.let(Machine.parse(_).stdlib.exists(_.name == t"inviter")).or(false)

          val admitted: Boolean = gamma.peers.stdlib.contains(Machine.Identity.local.hostname)

          // The token granted to the joiner admits it to any tool, fume's worker among them.

          val fumeAdmits: Boolean =
            def granted(file: Path on Linux): Optional[Text] =
              try java.nio.file.Files.readString(java.nio.file.Path.of(file.encode.s)).nn.tt.trim
              catch case _: Exception => Unset

            Peer.tokenFile(t"inviter").let(granted(_)).lay(false): granted =>
              Admissions().admit(granted, t"not-the-machine-token", t"fume", t"joiner") match
                case Admission.Peer(_) => true
                case _                 => false

          await(25)(logged.contains(t"withdrawn"))
          val withdrawn: Boolean = gamma.advertised.absent

          val revoked: Int = gamma.revoke(Machine.Identity.local.hostname)
          Swarm.stop(t"gamma")
          hub.stop()

          Invited
            ( word.cut(t" ").stdlib.length, read.present, advertised, nearby, stranger,
              joined.present, again.absent, declared, admitted, fumeAdmits, withdrawn, revoked )

      test(m"an invitation is one word, and reads back"):
        (invited.words, invited.read)
      . assert(_ == (1, true))

      test(m"the inviting machine is advertised while the invitation is open, and found by its fingerprint"):
        (invited.advertised, invited.nearby, invited.stranger)
      . assert(_ == (true, List(t"inviter.local", t"127.0.0.1"), Nil))

      test(m"the hosts found nearby are tried before those the invitation lists"):
        val listed: List[Text] = List(t"192.168.1.20", t"linux-box.local")
        val invitation: Invitation =
          Invitation(t"linux-box", listed, 8093, Data(), t"token", moment, t"pyrocosm")

        Swarm.nearer(invitation, List(t"linux-box.local", t"10.0.0.5")).hosts
      . assert(_ == List(t"linux-box.local", t"10.0.0.5", t"192.168.1.20"))

      test(m"an invitation is accepted once, and the machine joined is declared"):
        (invited.joined, invited.again, invited.declared, invited.admitted)
      . assert(_ == (true, true, true, true))

      test(m"a token granted to the swarm admits its bearer to any tool"):
        invited.fumeAdmits
      . assert(_ == true)

      test(m"the advertisement is withdrawn once the invitation is used, and a machine can be revoked"):
        (invited.withdrawn, invited.revoked >= 1)
      . assert(_ == (true, true))

    suite(m"The web origin"):
      // A hub in this JVM serves the machine's origin on a port of its own; a tool's front-end,
      // on a free port behind it, is reached at `/<tool>/` — its page, and its socket, through
      // the hub — and directly, as the same page; a tool nobody runs is not found; and the
      // origin's own page lists the tool.
      val served: Served =
        supervise:
          val runtime: Text = java.nio.file.Files.createTempDirectory("pyrocosm-origin").nn.toString.tt

          given Environment = name =>
            if name == t"XDG_RUNTIME_DIR" then runtime
            else Optional(java.lang.System.getenv(name.s)).let(_.tt)

          val port: Int = Port[Tcp]().number
          val hub: Hub = Swarm.hub(t"0.1.0", Nil, port).or(panic(m"another process is the hub"))
          val portal: Portal = Portal(hub, Port[Tcp]().number)
          val origin: Int = portal.serve().or(panic(m"the origin could not be bound"))
          hub.web = origin

          val content: Live[List[Block]] = Live(List(Block.paragraph(t"Alpha's page")))

          val interface: Interface =
            Interface
              ( Inline.text(t"Alpha"),
                List(Panel(Panel.Id(t"main"), Panel.Role.Primary, Unset, content, Panel.Priority.Essential)) )

          val presence: Presence = Presence()
          val frontend: WebFrontend = WebFrontend(0, fallback = mcpFake(_), presence = presence, prefix = t"/alpha")
          async(frontend.run(interface) { _ => () })
          await(25)(presence.web().present)
          val webPort: Int = presence.web().or(0)

          val alpha: Swarm = Swarm.start(t"alpha", t"0.1.0", () => List(Service(t"serve", webPort)), Nil, port)
          await(50)(hub.nodes().exists(_.node.tool == t"alpha"))

          def fetch(base: Int, path: Text): Http.Response = t"http://127.0.0.1:${base.show}$path".as[HttpUrl].fetch()

          val page: Text = fetch(origin, t"/alpha/").receive[Text]
          val landing: Text = fetch(origin, t"/").receive[Text]
          val direct: Text = fetch(webPort, t"/").receive[Text]
          val missing: Int = fetch(origin, t"/beta/").status.code

          // A ping over the browser's socket, tunnelled to the tool's, is answered with a pong.
          val reply: Promise[Text] = Promise()
          val url: Websocket.Url = t"ws://127.0.0.1:${origin.show}/alpha/socket".as[Websocket.Url]

          val ping: Text =
            t"""{"kind":"ping","id":"","text":"","caret":0,"index":0,"version":0,"label":""}"""

          async:
            url.exchange(())[Message]
              { (message: Message) =>
                  message match
                    case Message.Binary(data) => reply.offer(data.utf8)
                    case Message.Text(text)   => reply.offer(text)
                  coaxial.Control.Terminate }
              { transmitter => transmitter.send(Message.Text(ping)) }

          val pong: Optional[Text] = safely(reply.await(5.0*Second))

          // A patch longer than one buffer window — a page of substance — comes through the
          // tunnel whole: the browser's end reads one message, not a window of it at a time.
          val whole: Promise[Int] = Promise()

          async:
            safely:
              url.session: connection ?=>
                val input = connection.messages
                val demand = Credit(Websocket.Frame.maxPayload)
                var reading: Boolean = true

                while reading do input.refill(demand) match
                  case count: Int =>
                    if count > 0 then
                      val data: Data = input.lend { region => range => region.materialize(range.capped(count)) }
                      input.skip(count)
                      val text: Text = data.utf8
                      if text.contains(t"replace") then
                        whole.offer(text.length)
                        reading = false

                  case _ => reading = false

          snooze(1.0*Second)
          content() = List(Block.paragraph(Text("x".repeat(9000).nn)))
          val patched: Optional[Int] = safely(whole.await(5.0*Second))

          // Over MCP at the origin: a session, the tool listed under alpha's name, called by it,
          // and the session ended.
          def mcp(method: Http.Method, session: Optional[Text], body: Text): Http.Response =
            val headers: List[Http.Header] =
              List(Http.Header(t"content-type", t"application/json"), Http.Header(t"accept", t"application/json, text/event-stream"))
              + session.lay(Nil: List[Http.Header]) { id => List(Http.Header(t"mcp-session-id", id)) }

            val data: Data = body.in[Data]
            Loopback.exchange(origin, Http.Request(method, 1.1, Loopback.loopback, t"/mcp", headers, () => Stream(data)))
            . or(panic(m"the origin did not answer"))


          val initialized: Optional[Http.Response] =
            safely(mcp(Http.Post, Unset, t"""{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test","version":"0"}}}"""))

          val session: Optional[Text] = initialized.let(_.textHeaders.seek(_.key.lower == t"mcp-session-id").let(_.value)).or(Unset)
          val tools: Optional[Text] = safely(mcp(Http.Post, session, t"""{"jsonrpc":"2.0","id":2,"method":"tools/list"}""").receive[Text])
          val called: Optional[Text] = safely(mcp(Http.Post, session, t"""{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"alpha_hello","arguments":{}}}""").receive[Text])
          val deleted: Int = safely(mcp(Http.Delete, session, t"").status.code).or(0)

          Swarm.stop(t"alpha")
          frontend.stop()
          portal.stop()
          hub.stop()

          Served
            ( page.contains(t"""<meta name="pyro-prefix" content="/alpha">"""), page.contains(t"Alpha's page"),
              landing.contains(t"""href="/alpha/""""), direct.contains(t"""<meta name="pyro-prefix" content="/alpha">"""),
              missing, pong, session.present, tools, called, deleted, patched )

      test(m"a tool's page is served under its path on the origin, and is the same page directly"):
        (served.prefixed, served.content, served.direct)
      . assert(_ == (true, true, true))

      test(m"the origin's own page lists the tool, and a tool nobody runs is not found"):
        (served.listed, served.missing)
      . assert(_ == (true, 404))

      test(m"a socket through the origin reaches the tool, and its answer comes back"):
        served.pong.let(_.contains(t"pong"))
      . assert(_ == true)

      test(m"a patch longer than a buffer window comes through the origin as one message"):
        served.patched.let(_ > 8192)
      . assert(_ == true)

      test(m"an MCP session at the origin lists the tool's tools under its name, calls one, and ends"):
        ( served.session, served.tools.let(_.contains(t""""alpha_hello"""")), served.called.let(_.contains(t"hello from alpha: hello")),
          served.deleted )
      . assert(_ == (true, true, true, 204))

    suite(m"The journal"):
      test(m"entries are returned oldest first, after the one asked for"):
        val journal: Journal = Journal(10)
        journal.record(Level.Info, moment, m"one")
        journal.record(Level.Info, moment, m"two")
        journal.record(Level.Info, moment, m"three")
        (journal.latest, journal.since(1L).stdlib.map(_.message.text))
      . assert(_ == (3L, scala.List(t"two", t"three")))

      test(m"only the newest entries are kept, and their numbers are not reused"):
        val journal: Journal = Journal(5)
        (1 to 12).foreach: index => journal.record(Level.Info, moment, m"entry $index")
        val held = journal.since(0L).stdlib
        (held.map(_.sequence), held.head.message.text)
      . assert(_ == (scala.List(8L, 9L, 10L, 11L, 12L), t"entry 8"))

      test(m"entries below a level are left out"):
        val journal: Journal = Journal(10)
        journal.record(Level.Fine, moment, m"detail")
        journal.record(Level.Warn, moment, m"trouble")
        journal.record(Level.Info, moment, m"news")
        journal.since(0L, Level.Info).stdlib.map(_.message.text)
      . assert(_ == scala.List(t"trouble", t"news"))

      test(m"an instant is shown as the time of day, to the millisecond"):
        Journal.time(Instant.of[Unix](1791108067215L), tz"UTC")
      . assert(_ == t"10:01:07.215")

      test(m"a beat and a carried event are fine detail, and a lost link a warning"):
        val laptop: Party = Party.Caller(t"laptop")
        val beat: Event = Event.Sent(Wire.Beat(moment, Unset), laptop)
        val carry: Event = Event.Received(Wire.carry(envelope(Bus.Event.Left(node))), laptop)
        (beat.level, carry.level, Event.Lost(laptop, 3.0*Second).level, Event.Appeared(node).level)
      . assert(_ == (Level.Fine, Level.Fine, Level.Warn, Level.Info))

      test(m"a carried event is told by its kind and number"):
        val sent: Event = Event.Sent(Wire.carry(envelope(Bus.Event.Ended(node, t"run-1"))), Party.Local(t"fury", 7L))
        sent.communicate.text
      . assert(_ == t"→ carry ended fume's run-1 #7 to fury (pid 7)")

      test(m"the pause before a link is made again doubles, to half a minute at most"):
        scala.List(0, 1, 2, 3, 4, 5, 9).map(Swarm.delay(_).value.toInt)
      . assert(_ == scala.List(1, 2, 4, 8, 16, 30, 30))
