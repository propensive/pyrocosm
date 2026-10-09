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
import alphabets.hexLowerCase
import denominative.dysasymptotics.linearSize
import errorDiagnostics.emptyDiagnostics
import probates.cancelProbate
import proscenium.List
import strategies.throwUnsafely
import threading.platformThreading

// A later build's wire, for the acceptance tests: the same messages and one more, and an advert
// with one more, optional, field. Derived here, in the unchecked test module, as `Wire` is in
// `bus`; each derivation in an object of its own, as together they exceed what one class may
// hold.
object Later:
  import pyrocosm.Wire.given

  enum Wire:
    case Beat(sent: Instant over Unix, load: Optional[Double])
    case Ping(id: Uuid, sent: Instant over Unix, note: Text)
    case Pong(id: Uuid, received: Instant over Unix, from: Text)
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
  import pyrocosm.Wire.given

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
  private case class Meshed
    ( seen:        (Int, Int),
      neighbours:  (Int, Int),
      arrived:     Optional[Activity],
      coalesced:   Int,
      ended:       Boolean,
      greeted:     Optional[Greeting],
      oversized:   Boolean,
      gateway:     (Boolean, Boolean),
      handedOver:  Boolean,
      vanished:    Boolean )

  // What three machines showed.
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
      val runtime: Text = java.nio.file.Files.createTempDirectory("pyrocosm-registry").nn.toString.tt

      given Environment = name =>
        if name == t"XDG_RUNTIME_DIR" then runtime
        else Optional(java.lang.System.getenv(name.s)).let(_.tt)

      val registry: Registry = Registry(Registry.directory)
      val own: Long = java.lang.ProcessHandle.current.nn.pid
      val begun: Long = java.lang.System.currentTimeMillis - 1000L

      test(m"a live record is found again, and a dead one is dropped and deleted"):
        registry.register(Registry.Record(t"alpha", t"0.1.0", own, begun, 40001, t"ab"*32, Unset))
        registry.register(Registry.Record(t"beta", t"0.1.0", 2147483646L, begun, 40002, t"ab"*32, 8093))
        val found: List[Text] = registry.scan().map(_.tool)
        val remaining: Int =
          java.nio.file.Files.list(java.nio.file.Path.of(runtime.s, "pyrocosm", "swarm", "nodes")).nn.count.toInt
        (found, remaining)
      . assert(_ == (List(t"alpha"), 1))

      test(m"a record is parsed whole, gateway port included"):
        registry.register(Registry.Record(t"gamma", t"0.2.0", own, begun, 40003, t"cd"*32, 8093))
        registry.gateway().let { record => (record.tool, record.port, record.gateway, record.identity) }
      . assert(_ == (t"gamma", 40003, 8093, t"cd"*32))

      test(m"a record is removed by its process and tool"):
        registry.unregister(own, t"gamma")
        registry.scan().map(_.tool)
      . assert(_ == List(t"alpha"))

      test(m"registrations from many tasks all land"):
        supervise:
          val tasks = (1 to 8).map: index =>
            async(registry.register(Registry.Record(t"tool$index", t"0.1.0", own, begun, 40100 + index, t"ab"*32, Unset)))
          tasks.foreach(_.await())

        registry.scan().size
      . assert(_ == 9)

    suite(m"Two nodes of one machine"):
      // Two nodes in this JVM, as two tools' daemons would be on one machine, with a registry of
      // the test's own, so that the machine's is untouched; the identity is the machine's real
      // one, as it must be for the two to admit each other.
      val meshed: Meshed =
        supervise:
          val runtime: Text = java.nio.file.Files.createTempDirectory("pyrocosm-mesh").nn.toString.tt

          given Environment = name =>
            if name == t"XDG_RUNTIME_DIR" then runtime
            else Optional(java.lang.System.getenv(name.s)).let(_.tt)

          val port: Int = Port[Tcp]().number
          val start: Long = Journal.daemon.latest
          def logged: scala.List[Text] = kinds(Journal.daemon.since(start))

          val alpha: Swarm = Swarm.start(t"alpha", t"0.1.0", () => List(Service(t"serve", 8081)), Nil, port)
          val beta: Swarm = Swarm.start(t"beta", t"0.1.0", () => Nil, Nil, port)

          await(50)(alpha.nodes().size == 2 && beta.nodes().size == 2)
          val seen: (Int, Int) = (alpha.nodes().size, beta.nodes().size)
          await(25)(alpha.neighbours.size == 1 && beta.neighbours.size == 1)
          val neighbours: (Int, Int) = (alpha.neighbours.size, beta.neighbours.size)

          // A hundred updates in a hundred milliseconds reach beta as the last of them.
          val live: Live[List[Activity]] = Live(Nil)
          alpha.expose(live)

          (1 to 100).foreach: index =>
            live() = List(activity.copy(status = Status.Fraction(index/100.0)))
            Thread.sleep(1)

          await(25)(beta.activities().exists(_(1).status == Status.Fraction(1.0)))
          val arrived: Optional[Activity] = beta.activities().prim.let(_(1))
          val coalesced: Int = logged.count(_ == t"sent-updated")

          live() = Nil
          await(25)(beta.activities().nil)
          val ended: Boolean = beta.activities().nil

          // A tool's own event reaches a subscriber on the other node, and an oversized one
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

          // One of the two takes the gateway port; when it goes, the other does.
          alpha.wantGateway(port, Unset)
          await(25)(alpha.listening.present)
          beta.wantGateway(port, Unset)
          snooze(2.5*Second)
          val gateway: (Boolean, Boolean) = (alpha.listening.present, beta.listening.present)

          Swarm.stop(t"alpha")
          await(50)(beta.listening.present)
          val handedOver: Boolean = beta.listening.present
          await(25)(beta.nodes().size == 1)
          val vanished: Boolean = beta.nodes().size == 1
          Swarm.stop(t"beta")

          Meshed(seen, neighbours, arrived, coalesced, ended, greeted, oversized, gateway, handedOver, vanished)

      test(m"each node sees the other, and links to it"):
        (meshed.seen, meshed.neighbours)
      . assert(_ == ((2, 2), (1, 1)))

      test(m"an activity's updates arrive, coalesced to a few"):
        (meshed.arrived.let(_.status), meshed.coalesced <= 5)
      . assert(_ == (Status.Fraction(1.0), true))

      test(m"an activity removed is ended on the other node"):
        meshed.ended
      . assert(_ == true)

      test(m"a tool's own event reaches a subscriber, and an oversized one is refused"):
        (meshed.greeted, meshed.oversized)
      . assert(_ == (Greeting(t"alpha", 1), true))

      test(m"one node is the gateway, and the other takes over when it goes"):
        (meshed.gateway, meshed.handedOver, meshed.vanished)
      . assert(_ == ((true, false), true, true))

    suite(m"Three machines"):
      // Three machines in this JVM, each with an identity of its own — `Peer.identity` makes a
      // certificate under whichever state home a node's environment names — and a registry of
      // its own, so that none finds the others as a neighbour. Machine A has two nodes, one its
      // gateway; machines B and C each keep a link to that gateway, which relays between its
      // own machine and each of them, and between the two of them, as the hub of its spokes.
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
          val alpha: Swarm = Swarm.start(t"alpha3", t"0.1.0", () => Nil, Nil, port)(using summon[Monitor], summon[Probate], envA)
          val beta: Swarm = Swarm.start(t"beta3", t"0.1.0", () => Nil, Nil, port)(using summon[Monitor], summon[Probate], envA)
          val gamma: Swarm = Swarm.start(t"gamma3", t"0.1.0", () => Nil, Nil, port)(using summon[Monitor], summon[Probate], envB)
          val delta: Swarm = Swarm.start(t"delta3", t"0.1.0", () => Nil, Nil, port)(using summon[Monitor], summon[Probate], envC)

          await(50)(alpha.nodes().size == 2 && beta.nodes().size == 2)
          alpha.wantGateway(port, Unset)
          await(25)(alpha.listening.present)

          // The token every machine shares here, and A's fingerprint, make the declaration B
          // and C link by.
          val token: Text = Peer.token(using envA).or(t"")
          val fingerprint: Optional[Data] = Peer.parseFingerprint(alpha.node.identity)
          val a: Machine = Machine(t"a", List(t"127.0.0.1"), port, fingerprint, token, Nil)

          gamma.connect(a)
          await(50)(gamma.connected(t"a"))
          delta.connect(a)
          await(50)(delta.connected(t"a"))

          await(50)(gamma.nodes().size == 4 && delta.nodes().size == 4 && beta.nodes().size == 4)
          val seen: (Int, Int, Int, Int) = (alpha.nodes().size, beta.nodes().size, gamma.nodes().size, delta.nodes().size)
          val distinct: Boolean = Set(alpha.node.identity, gamma.node.identity, delta.node.identity).size == 3

          // An event of A's gateway reaches B over their link, relayed by nobody; one of B's
          // reaches A's other node and C relayed once, by the gateway — each once.
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

          // B hangs up: A's nodes, and C, forget it.
          gamma.disconnect(t"a")
          await(50)(beta.nodes().size == 3 && delta.nodes().size == 3)
          val forgotten: (Int, Int) = (beta.nodes().size, delta.nodes().size)

          Swarm.stop(t"delta3")
          Swarm.stop(t"gamma3")
          Swarm.stop(t"beta3")
          Swarm.stop(t"alpha3")

          Relayed(seen, distinct, hops, forgotten)

      test(m"every node of three machines sees every other, through one gateway"):
        (relayed.seen, relayed.distinct)
      . assert(_ == ((4, 4, 4, 4), true))

      test(m"an event crosses to a linked machine unrelayed, and on to the next relayed once, once each"):
        relayed.hops
      . assert(_ == (List(0), List(1), List(1)))

      test(m"a machine which hangs up is forgotten on every other"):
        relayed.forgotten
      . assert(_ == (3, 3))

    suite(m"Invitations"):
      // A gateway in this JVM invites, and this JVM joins: the invitation is accepted once, the
      // machine is declared, and once revoked it is refused. The declaration and the granted
      // token are written to a configuration directory of the test's own, so that the user's
      // `machines.tel` is untouched; the inviter advertises itself, and the joiner looks for
      // it, over an in-memory mDNS bus, so that nothing is multicast on the network.
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
          val gamma: Swarm = Swarm.start(t"gamma", t"0.1.0", () => Nil, Nil, port)
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
