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
import scala.collection.concurrent.TrieMap

// `Service` is excluded from the umbrella: pyrocosm's own, declared in another file, would be
// outranked by urticose's.
import soundness.{Service as _, *}

import Journal.{Event, Fingerprint, Obstacle, Party}
import alphabets.hexLowerCase
import denominative.dysasymptotics.{linearAccess, linearSize}
import codepages.utf8Codepage
import errorDiagnostics.emptyDiagnostics

// The machine's hub: the one node every tool's node on the machine is a leaf of, and the
// machine's gateway when it is asked to be one. It listens for leaves on a port of its own,
// registered as the machine's `hub` record beside the lock it holds for as long as it runs,
// so that two hubs started at once leave one; relays what it hears on any link to every other,
// as any node does; and does what a leaf asks of it. It runs in the `pyrocosm` process, which
// a tool's daemon starts when it finds no hub, and which ends once its last leaf has gone.
final class Hub private[pyrocosm]
  ( version: Text, capabilities: List[Text], swarmPort: Int, backend: Discovery.Backend^{} )
  ( using environment: Environment )
extends Swarm(Wire.tool, version, () => Nil, capabilities, swarmPort):

  import Swarm.{Connection, Caller}

  // Lazy, since `Swarm` asks `gateway` for its first advert before this class's fields exist.
  private lazy val gateway0: Atomic[Int] = Atomic(0)
  protected def gateway: Boolean = gateway0() != 0

  def hub: Optional[Node] = node

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var lock: Optional[java.nio.channels.FileLock] = Unset

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var loopback: Optional[Peer.Listener[Data]] = Unset

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var loopbackPort: Int = 0

  private def record: Registry.Record =
    Registry.Record(tool, version, pid, started.long, loopbackPort, fingerprint, gateway0() match
      case 0    => Unset
      case port => port)

  // The leaves linked to this hub now.
  def leaves: List[Node] = neighbours.map(_(0))

  // Starts the hub, if this process can take the hub's lock: its listener for the machine's
  // leaves, on a port of its own, its registration, and its housekeeping. False if another
  // process is the hub.
  private[pyrocosm] def start()(using Monitor, Probate): Boolean = (identity, token) match
    case (identity: Peer.Identity, token: Text) =>
      registry.lockHub() match
        case held: java.nio.channels.FileLock =>
          lock = held
          async(listen(identity, token))
          async(housekeep())
          async(announce())
          async(coalesce())
          true

        case _ =>
          false

    case (identity, _) =>
      if identity.absent then Journal.log(Event.ListenFailed(0, Obstacle.NoIdentity))
      else Journal.log(Event.ListenFailed(0, Obstacle.NoToken))
      false

  // Listens for the machine's leaves, on an unused port, for as long as the hub lives: a port
  // which could not be bound, or stops being served, is replaced by another.
  private def listen(identity: Peer.Identity, token: Text)(using Monitor, Probate): Unit =
    val gate: () => Optional[Text] = () => Unset

    while !stopping() do
      val port: Int = Port[Tcp]().number

      val listener: Peer.Listener[Data] =
        Peer.Listener[Data](Wire.tool, version, Wire.codec, token, identity, capabilities, gate):
          session => answer(session)

      loopback = listener
      loopbackPort = port
      registry.registerHub(record)
      Journal.log(Event.Registered(tool, pid, port))
      listener.serve(port)
      loopbackPort = 0
      if !stopping() then snooze(Swarm.housekeeping)

  // Every couple of seconds: forgets any node not heard from for too long, and takes the
  // gateway port if it is wanted and free.
  private def housekeep()(using Monitor, Probate): Unit =
    while !stopping() do
      safely:
        val moment: Long = java.lang.System.currentTimeMillis

        heard.toList.foreach: (key, last) =>
          if moment - last > (Swarm.forgetting.value*1000.0).toLong then
            known(key).let(forget(_))

        elect()

      snooze(Swarm.housekeeping)

  private[pyrocosm] def halt()(using Monitor): Unit =
    stopGateway()
    loopback.let(_.stop())

  private[pyrocosm] def retire(): Unit =
    registry.unregisterHub()
    lock.let { held => safely(held.release()); safely(held.channel.nn.close()) }
    lock = Unset
    Journal.log(Event.Unregistered(tool, pid))

  // What a snapshot teaches this hub of nodes beyond the link, its leaves and the machines
  // linked to it learn only through this hub, so it passes each on, as an advert of its own
  // numbering: a snapshot is link-local, and the nodes it names would otherwise be known here
  // only once they advertise themselves again, up to half a minute away.
  override def snapshotted(tether: Tether, advert: Advert, known: List[Advert]): Unit =
    val before: List[Text] = nodes().map(_.node.key)
    super.snapshotted(tether, advert, known)

    known.filter { other => other.node.key != node.key && !before.has(other.node.key) }.each: other =>
      broadcast(Envelope(node.key, sequence.incrementAndGet(), now(), 1, Bus.Event.Advertised(other)), tether)

  // ── what a leaf asks ─────────────────────────────────────────────────────────────────────

  // Does what the other end asks, and says so: `listen <port> [token]` and `unlisten` take and
  // give up the gateway port; `invite <port> <word>` advertises an invitation the leaf issued;
  // `connect <machine block>` and `disconnect <name>` keep and drop a link to another machine;
  // `status` says how the hub's links stand, one per line.
  override def requested(tether: Tether, request: Wire.Request)(using Monitor, Probate): Unit =
    val words: List[Text] = request.argument.cut(t" ").filter(_ != t"")
    def port: Optional[Int] = words.head.let { text => safely(text.as[Int]) }.or(Unset)

    val (ok, text): (Boolean, Text) = request.command match
      case t"listen" =>
        port.lay((false, t"no port")): port =>
          wantGateway(port, words.at(Sec).let(Machine.secret(_)))
          (true, t"")

      case t"unlisten" =>
        stopGateway()
        (true, t"")

      case t"invite" =>
        port.lay((false, t"no port")): port =>
          words.at(Sec).let { word => safely(Invitation.parse(word)) }.or(Unset).lay((false, t"no invitation")):
            invitation =>
              advertise(invitation, port)(using environment, summon[Monitor], summon[Probate], backend)
              (true, t"")

      case t"connect" =>
        safely(request.argument.in[Data].read[Tel]).let(Machine.parse(_).head).or(Unset).lay((false, t"no machine")): (machine: Machine) =>
          connect(machine)
          (true, t"")

      case t"disconnect" =>
        words.head.lay((false, t"no machine")): name =>
          (disconnect(name), t"")

      case t"status" =>
        (true, status)

      case _ =>
        (false, t"unknown command")

    tether.send(Wire.Reply(request.id, ok, text))

  // How this hub's links stand: `listening <port>` and `advertised <name>` where they apply,
  // then `connection <name> <heard millis or 0>` for each machine it keeps a link to and
  // `visitor <hostname or -> <heard millis or 0>` for each caller from another machine.
  def status(using Monitor): Text =
    def heard(standing: Tether.Standing): Text = standing.heard.let(_.long.show).or(t"0")

    val lines: List[Text] =
      listening.let { port => List(t"listening ${port.show}") }.or(Nil)
      + advertised.let { name => List(t"advertised $name") }.or(Nil)
      + connections.map { connection => t"connection ${connection.machine.name} ${heard(connection.standing)}" }
      + visitors.map: visitor =>
          val host: Text = visitor.peer match
            case Party.Caller(hostname) => hostname.or(t"-")
            case _                      => t"-"
          t"visitor $host ${heard(visitor.standing)}"

    lines.join(t"\n")

  // ── the gateway ──────────────────────────────────────────────────────────────────────────

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var gatewayListener: Optional[Peer.Listener[Data]] = Unset

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var gatewayWanted: Optional[(Int, Text)] = Unset

  private val gatewayStopping: Atomic[Boolean] = Atomic(false)

  // The port this node accepts other machines on now, if it is the gateway.
  def listening: Optional[Int] = gateway0() match
    case 0      => Unset
    case number => number

  // Asks this node to be the machine's gateway on `port`, with `token` as the secret callers
  // present (the machine's own, if none): it takes the port when it is free, and again whenever
  // the node holding it goes.
  def wantGateway(port: Int, token: Optional[Text])(using Monitor, Probate): Unit =
    gatewayWanted = (port, token.or(this.token.or(t"")))
    elect()

  private def elect()(using Monitor, Probate): Unit =
    gatewayWanted.let: (port, secret) =>
      if listening.absent && registry.gateway().absent then async(serveGateway(port, secret))

  // Accepts other machines on `port`, until `stopGateway` or until the port cannot be held.
  private def serveGateway(port: Int, secret: Text)(using Monitor, Probate): Unit =
    identity.let: identity =>
      if gateway0.swap(port) == 0 then
        val gate: () => Optional[Text] = () => Unset

        val listener: Peer.Listener[Data] =
          Peer.Listener[Data](Wire.tool, version, Wire.codec, secret, identity, capabilities, gate):
            session => answer(session)

        gatewayListener = listener
        gatewayStopping() = false
        registry.registerHub(record)
        Journal.log(Event.Listening(port, Fingerprint(identity.fingerprint)))
        Journal.log(Event.Gateway(port))

        // `serve` blocks until `stop`, and returns at once, saying nothing, if the port could
        // not be bound; which of the two happened is what `gatewayStopping` tells apart.
        try listener.serve(port) finally
          if gatewayStopping() then Journal.log(Event.Stopped(port))
          else Journal.log(Event.ListenFailed(port, Obstacle.Unbound))

          gateway0() = 0
          gatewayListener = Unset
          registry.registerHub(record)

  // Stops accepting other machines, hangs up on those linked, and withdraws any advertisement.
  def stopGateway()(using Monitor): Unit =
    gatewayWanted = Unset
    gatewayStopping() = true
    gatewayListener.let(_.stop())
    mutex(tethers).filter(_.side == Tether.Side.Remote).each(_.close())
    mutex { open = Nil }

  // ── inviting ─────────────────────────────────────────────────────────────────────────────

  // An invitation to this machine, for one other to accept before `lifetime` has passed, on
  // the gateway port. While it is open, this machine is advertised on the local network, so
  // that the other finds it by name. The tactic is a parameter rather than `raises`, since a
  // context-function result would hide the monitor the advertisement runs under.
  def invite(port: Int, lifetime: Duration)(using backend: Discovery.Backend^{})
    ( using Environment, Monitor, Probate, Tactic[Peer.Error] )
  :   Invitation =

    val invitation: Invitation = Peer.invite(Wire.tool, port, lifetime)
    Journal.log(Event.Invited(invitation.expires))
    advertise(invitation, port)
    invitation

  // ── advertising ──────────────────────────────────────────────────────────────────────────

  // The invitations this node has issued and not yet seen used or expire, newest first; the
  // advertisement runs while there are any.
  @scala.caps.unsafe.untrackedCaptures
  private var open: List[Invitation] = Nil

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var announced: Optional[Text] = Unset

  // The name this machine is advertised as now, if it is.
  def advertised(using Monitor): Optional[Text] = announced

  // This machine's name as the one DNS label an instance is called by.
  private def label: Text = node.machine.cut(t".").head.keep(63)

  // Adds `invitation` to those open, and starts advertising, in the background under the
  // daemon's monitor, if nothing was open before; an advertisement running already covers it.
  private def advertise(invitation: Invitation, port: Int)
    ( using Environment, Monitor, Probate, Discovery.Backend^{} )
  :   Unit =

    val first: Boolean = mutex:
      val idle: Boolean = open.nil
      open = invitation :: open
      idle

    if first then async(advertise(port, Peer.peers.size))

  // Advertises this machine for as long as an invitation is open: until the last expires or is
  // used — which this node tells by one more machine among those it has admitted — or the
  // gateway stops. The advertisement ends with a goodbye on the link. `admitted` is how many
  // machines had been admitted when the advertisement began.
  private def advertise(port: Int, admitted: Int)
    ( using environment: Environment, monitor: Monitor, probate: Probate )
    ( using backend: Discovery.Backend^{} )
  :   Unit =

    val made: Optional[Boolean] = identity.let: identity =>
      val txt: Optional[Discovery.Txt] = safely:
        Discovery.Txt
          ( Swarm.fingerprintKey -> Peer.render(identity.fingerprint),
            Swarm.versionKey     -> version,
            Swarm.portKey        -> port.show )

      txt.let: txt =>
        val description: Discovery.Description = Discovery.Description(label, Port.unsafe[Tcp](port), txt)

        val outcome: Attempt[Unit, Discovery.Error] = attempt[Discovery.Error]:
          Swarm.nearby.advertise(description)(using backend):
            val instance: Text = summon[Discovery.Advertisement].instance.label
            announced = instance
            Journal.log(Event.Advertised(instance, port))
            linger(admitted)(using environment, monitor)
            announced = Unset
            Journal.log(Event.Withdrawn(instance))

        outcome match
          case Attempt.Failure(error) =>
            Journal.log(Event.Unadvertised(error.reason))
            false

          case _ =>
            true

    // An advertisement which was never made leaves nothing open, or an invitation issued
    // meanwhile would be waited for by nobody. One which ran left nothing open when it ended,
    // and anything issued since has an advertisement of its own.
    if !made.or(false) then mutex { open = Nil }

  // Waits, a second at a time, while any invitation is open: each time it looks, it drops
  // those which have expired, and the oldest for each machine admitted since the last look.
  private def linger(admitted: Int)(using Environment, Monitor): Unit =
    var count: Int = admitted
    var lingering: Boolean = true

    while lingering do
      val moment: Instant over Unix = now()
      val latest: Int = Peer.peers.size
      val used: Int = (latest - count).max(0)
      count = latest

      val remaining: List[Invitation] = mutex:
        val live: List[Invitation] = open.filter(_.expires > moment)
        open = List(live.reverse.stdlib.drop(used).reverse*)
        open

      if !remaining.nil && listening.present then snooze(1.0*Second) else lingering = false
