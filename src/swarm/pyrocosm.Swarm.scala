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
import denominative.dysasymptotics.linearSize
import errorDiagnostics.emptyDiagnostics
// The swarm every Pyrocosm tool's daemon is a NODE of. On one machine, every tool's node is
// a LEAF of the machine's one HUB — a process of its own, `pyrocosm`, started by the first tool
// which needs it — which it finds through the `Registry` and links to over loopback, so that
// every tool on a machine knows every other, with no configuration, through one link each.
// Between machines, the hub is the GATEWAY: it accepts other machines on the machine's swarm
// port — by an invitation (`invite`, `join`), or a `machine` declaration and `connect` — and
// relays what it hears on one side to the other, so a leaf need link to nothing but its hub,
// and a machine to nothing but another's hub. Every link carries the BUS: each node
// advertises itself as it joins and every half minute after, says when it leaves, and
// publishes each change of its activities, so that the work in flight anywhere on the swarm
// is known everywhere; and a tool may publish events of its own, which only its instances
// need understand. A node acts on each event once, however many links bring it, by the
// sequence its origin numbered it with. What a leaf cannot do itself — listen for other
// machines, advertise an invitation, keep a link to another machine — it asks of its hub, in
// a `request` the hub answers with a `reply`.
//
// A link is kept honest by a heartbeat: each end sends a `beat` every second, and an end which
// hears nothing for three seconds takes the link for lost, says so, and closes it; the end
// which made the link then makes it again. Both ends log what they do, so `<tool> swarm log`
// on each machine shows a message arrive and its answer return.
//
// The transport is Pyrocosm's, as fume's is: TLS to the machine's self-signed certificate,
// which the caller pins by fingerprint, with a shared token proving the caller — the
// machine's own, on loopback — and a length-prefixed framing. What is framed is the bus's own:
// BinTEL documents under `Wire`'s schema, each written to the acceptance the other end sent
// when the link was made, so two builds whose protocols differ still say what both can read.
object Swarm:
  object Error:
    object Reason:
      given communicable: Reason is Communicable =
        case Connection(reason) => m"$reason"
        case Unanswered         => m"the connection closed before a pong arrived"
        case Silent             => m"no pong arrived in time"
        case Mismatched         => m"the two nodes could not agree a protocol"

    // Why a machine could not be pinged: the link to it could not be made, for the reason
    // Pyrocosm gives; it was made and then closed without the `pong`; it stayed open and the
    // `pong` did not come; or the two ends, having exchanged acceptances, found that one could
    // not read what the other writes.
    enum Reason:
      case Connection(reason: Peer.Error.Reason)
      case Unanswered
      case Silent
      case Mismatched

  case class Error(machine: Machine, reason: Error.Reason)(using Diagnostics)
  extends fulminate.Error(m"the ping to ${machine.name} failed because $reason")

  // What a `pong` told the caller: which node answered, with which version, and how long the
  // round trip took.
  case class Reply(node: Text, version: Optional[Text], elapsed: Duration)

  // A lasting link to another machine this node is asked to keep, and how it stands.
  case class Connection(machine: Machine, standing: Tether.Standing)

  // A caller which has made a lasting link to this node, and how it stands.
  case class Caller(peer: Party, standing: Tether.Standing)

  // Which events a subscriber hears: those of these tools, of these kinds (a tool event's
  // `kind`, or `advertised`, `left`, `updated`, `ended`), from these origins — any, where a set
  // is empty — and from other machines, from this one, or both.
  case class Filter
    ( tools:   Set[Text] = Set(),
      kinds:   Set[Text] = Set(),
      origins: Set[Text] = Set(),
      remote:  Boolean   = true,
      local:   Boolean   = true ):

    def admits(origin: Optional[Node], envelope: Envelope, self: Node): Boolean =
      val tool: Optional[Text] = envelope.event match
        case Bus.Event.Tool(tool, _, _) => tool
        case _                          => origin.let(_.tool)

      val kind: Text = Swarm.kind(envelope.event)
      val near: Boolean = origin.lay(true)(_.shares(self))

      (tools.nil || tool.lay(false)(tools.has(_)))
      && (kinds.nil || kinds.has(kind))
      && (origins.nil || origins.has(envelope.origin))
      && (if near then local else remote)

  // A subscription, until it is cancelled.
  trait Subscription:
    def cancel(): Unit

  // The word by which a filter names each kind of event.
  def kind(event: Bus.Event): Text = event match
    case _: Bus.Event.Advertised    => t"advertised"
    case _: Bus.Event.Left          => t"left"
    case _: Bus.Event.Updated       => t"updated"
    case _: Bus.Event.Ended         => t"ended"
    case Bus.Event.Tool(_, kind, _) => kind

  // How long the end which made a link waits before making it again: a second at first, twice
  // as long after each failure, and half a minute at most.
  def delay(failures: Int): Duration = (1 << failures.min(5)).min(30).toDouble*Second

  // How often a node repeats its advert, and how long the others keep a node they have not
  // heard from; how often a hub looks over its links; how often coalesced activity updates are
  // sent; and the most links an event may pass through: a leaf's to its hub, the hub's to
  // another machine's, and that hub's to its leaf, with one to spare.
  val announcement: Duration = 30.0*Second
  val forgetting: Duration = 90.0*Second
  val housekeeping: Duration = 2.0*Second
  val coalescing: Duration = 0.25*Second
  val reach: Int = 4

  // The name a leaf keeps its link to the hub under.
  val hubName: Text = t"hub"

  // The most events a tool may publish in a second, beyond which they are dropped.
  val budget: Int = 50

  // This machine's one-minute load average, where its platform keeps one: the JVM answers a
  // negative number where it does not, as it always does on Windows.
  def load: Optional[Double] =
    val average: Double =
      java.lang.management.ManagementFactory.getOperatingSystemMXBean.nn.getSystemLoadAverage

    if average < 0.0 then Unset else average

  // The leaves started in this JVM, by tool: one in a tool's daemon, more in a test.
  private val instances: TrieMap[Text, Swarm] = TrieMap()

  // Starts this tool's node, if it has not been started: a leaf, which finds the machine's hub
  // and keeps a link to it. `revive` is what starts a hub when there is none — the tool's
  // daemon spawns one — and nothing, in a test. Idempotent per tool, for as long as the JVM
  // lives or until `stop`.
  def start
    ( tool: Text, version: Text, services: () => List[Service], capabilities: List[Text] = Nil,
      swarmPort: Int = Wire.port, revive: () => Unit = () => () )
    ( using Monitor, Probate, Environment )
  :   Swarm =

    instances.get(tool) match
      case Some(swarm) =>
        swarm

      case None =>
        // The closures are the tool's daemon's own, there for as long as the node is, so they
        // are vouched pure.
        val pure: () -> List[Service] = scala.caps.unsafe.unsafeAssumePure(services)
        val revive0: () -> Unit = scala.caps.unsafe.unsafeAssumePure(revive)
        val leaf: Leaf = Leaf(tool, version, pure, capabilities, swarmPort, revive0)
        instances.putIfAbsent(tool, leaf) match
          case Some(existing) =>
            existing

          case None =>
            leaf.start()
            // The process's frontends publish through its first node, which in a daemon is the
            // only one.
            if instances.size == 1 then Presences.start(leaf)
            leaf

  // Starts this machine's hub in this JVM, if no process is the hub already: it takes the
  // hub's lock, listens for the machine's leaves, and takes the gateway port when asked to.
  // Absent if another process holds the lock. A hub is not among `instances`: it is the
  // `pyrocosm` process's own, or a test's, and is stopped by whoever started it.
  def hub(version: Text, capabilities: List[Text] = Nil, swarmPort: Int = Wire.port)
    ( using backend: Discovery.Backend^{} = discovery )
    ( using Monitor, Probate, Environment )
  :   Optional[Hub] =

    val hub: Hub = Hub(version, capabilities, swarmPort, backend)
    if hub.start() then hub else Unset

  def of(tool: Text): Optional[Swarm] = instances.get(tool).getOrElse(Unset)

  // The node of this JVM's tool: the first started, which in a daemon is the only one.
  def current: Optional[Swarm] = instances.values.minByOption(_.node.started.long).getOrElse(Unset)

  def stop(tool: Text)(using Monitor): Unit = instances.remove(tool).foreach(_.stop())

  // ── discovering ──────────────────────────────────────────────────────────────────────────

  // The service by which a machine's gateway is found on the local network (RFC 6763):
  // `_pyrocosm._tcp`. A machine advertises an instance of it only while an invitation it issued
  // is open, so that the machine invited finds it by name, whatever addresses the invitation
  // lists; the invitation is still what admits it.
  private[pyrocosm] val nearby: Discovery.Service = unsafely(Discovery.Service(t"pyrocosm", Tcp))

  // The one mDNS responder of this daemon, over the JVM's multicast sockets. It opens its socket
  // at the first advertisement or browse and closes it after the last, so a daemon which never
  // invites or joins never joins the multicast group. Bound once: each summons of the backend
  // would be a responder of its own, with a socket of its own.
  lazy val discovery: Discovery.Backend =
    import socketBackends.javaBaseSockets
    discoveryBackends.mdnsSockets

  // The TXT record's keys: the fingerprint a joiner matches against its invitation's, the
  // version of the tool advertising, and the port its gateway listens on.
  private[pyrocosm] val fingerprintKey: Text = t"fp"
  private[pyrocosm] val versionKey: Text = t"v"
  private[pyrocosm] val portKey: Text = t"port"

  // How long a joiner looks for the inviting machine nearby before it tries the invitation's
  // addresses alone, and how long each instance it finds is given to resolve.
  val lookout: Duration = 3.0*Second
  val resolution: Duration = 1.5*Second

  // Whether a found instance is the machine an invitation is to: its TXT fingerprint is the
  // invitation's identity. Fingerprints are compared as bytes, through Pyrocosm's parser, so
  // either spelling of one matches.
  private def claims(resolution: Discovery.Resolution, identity: Data): Boolean =
    resolution.txt(fingerprintKey).let(Peer.parseFingerprint(_)).let(Channel.same(_, identity))
    . or(false)

  // The hosts the inviting machine was found at nearby — its `.local` name, then its addresses
  // — in the order a link should try them; or none, if it was not found within `lookout`.
  // Nothing here can fail a join: a network without multicast, a responder which cannot start,
  // or an instance which does not resolve in time simply finds nothing. Link-local IPv6
  // addresses are left out, being unusable without a scope.
  def locate(invitation: Invitation)(using backend: Discovery.Backend^{} = discovery)
    ( using Monitor, Probate )
  :   List[Text] =

    given sink: (LogSink[Any, Message]^{}) = Journal.sink

    val found: Optional[Discovery.Resolution] = safely:
      nearby.browse(using backend):
        val browser: Discovery.Browser = summon[Discovery.Browser]
        val promise: Promise[Discovery.Resolution] = Promise()

        // Each instance found is resolved on a task of its own making, so that the wait for
        // the right one is bounded by `lookout` alone; the task ends when the browse does,
        // which ends its events.
        def scan(events: scala.collection.immutable.LazyList[Discovery.Event])
          ( using Monitor, Probate )
        :   Unit =
          if !events.isEmpty then
            events.head match
              case Discovery.Event.Found(instance) =>
                safely(instance.resolve(resolution)(using backend)).let: resolved =>
                  if claims(resolved, invitation.identity) then promise.offer(resolved)

              case _ =>
                ()

            if !promise.ready then scan(events.tail)

        async(scan(browser.events.stdlib))
        safely(promise.await(lookout))

    found match
      case resolution: Discovery.Resolution =>
        val addresses: List[Text] =
          resolution.endpoints.map(_.remote).filter(!_.lower.starts(t"fe80"))

        val hosts: List[Text] = resolution.host.show :: addresses
        Journal.log(Event.Discovered(invitation.name, hosts))
        hosts

      case _ =>
        Journal.log(Event.Undiscovered(invitation.name))
        Nil

  // The invitation with the hosts the machine was found at ahead of those it lists.
  def nearer(invitation: Invitation, hosts: List[Text]): Invitation =
    if hosts.nil then invitation
    else
      val listed: List[Text] = invitation.hosts.filter(!hosts.has(_))
      invitation.copy(hosts = List((hosts.stdlib ++ listed.stdlib)*))

// A node of the swarm: a tool's (`Leaf`, made through `Swarm.start`) or the machine's (`Hub`,
// made through `Swarm.hub`). What both do is here — the bus, the activities, the links and
// what travels over them, pinging — and what each does of its own is in its class: a leaf
// keeps its one link to the hub and asks it for the rest; the hub listens for leaves, is the
// gateway, advertises invitations and keeps the links to other machines.
abstract class Swarm private[pyrocosm]
  ( val tool:    Text,
    val version: Text,
    services:    () -> List[Service],
    val capabilities: List[Text],
    val swarmPort: Int )
  ( using environment: Environment )
extends Tether.Host, scala.caps.Pure:

  import Swarm.{Connection, Caller, Filter, Subscription}

  // What this object does of its own accord — for the daemon, outside any invocation — it logs
  // to the daemon's journal.
  protected given sink: (LogSink[Any, Message]^{}) = Journal.sink

  protected val mutex: Mutex = Mutex()
  protected val started: Instant over Unix = now()
  protected val pid: Long = java.lang.ProcessHandle.current.nn.pid

  // This machine's identity and token, had once; a node of a machine without either cannot
  // link, and says so in the journal.
  protected val identity: Optional[Peer.Identity] = safely(Peer.identity)
  protected val token: Optional[Text] = Peer.token

  protected def fingerprint: Text = identity.let(_.fingerprint.serialize[Hex]).or(t"")

  // The registry by which the machine's nodes find the hub, and whether this node has begun
  // to stop.
  protected val registry: Registry = Registry(Registry.directory)
  protected val stopping: Atomic[Boolean] = Atomic(false)

  val node: Node =
    Node
      ( Machine.Identity.local.hostname, fingerprint, tool, version, pid, started,
        Machine.addresses )

  // Whether this node is its machine's gateway now: only a hub can be.
  protected def gateway: Boolean

  private def url(host: Text, port: Int): Text = t"http://$host:${port.show}/"

  // What this node says of itself now.
  def self: Advert =
    val machine: Machine.Identity = Machine.Identity.local

    val host: Text = node.hosts.head.or(node.machine)

    // Named rather than a lambda around the interpolation (the 3.9.0 `wildApprox` crash).
    def located(service: Service): Service =
      if service.name == t"serve" && service.url.absent then service.copy(url = url(host, service.port))
      else service

    // A web front-end started outside the tool's services (`gallery serve <port>`) says so
    // through the shared presence.
    val declared: List[Service] = services()

    val web: List[Service] =
      if declared.exists(_.name == t"serve") then Nil
      else Presence.shared.web().lay(Nil: List[Service]) { port => List(Service(t"serve", port)) }

    val offered: List[Service] = (declared + web).map(located(_))

    Advert
      ( node, offered, capabilities, machine.os, machine.arch, machine.cores, Swarm.load,
        gateway )

  // ── what each kind of node does of its own ───────────────────────────────────────────────

  // Stops what the node does of its own, before its links close; and tidies after them.
  private[pyrocosm] def halt()(using Monitor): Unit
  private[pyrocosm] def retire(): Unit

  // The hub this node is linked to (itself, for a hub), once its snapshot has arrived.
  def hub: Optional[Node]

  // The port this machine accepts other machines on now, if it does.
  def listening: Optional[Int]

  // Asks the machine to be a gateway on `port`, with `token` as the secret callers present
  // (the machine's own, if none), and to stop being one.
  def wantGateway(port: Int, token: Optional[Text])(using Monitor, Probate): Unit
  def stopGateway()(using Monitor): Unit

  // The name this machine is advertised under on the local network now, if it is.
  def advertised(using Monitor): Optional[Text]

  // An invitation to this machine, for one other to accept before `lifetime` has passed, on
  // the gateway port; while it is open the machine is advertised on the local network, so that
  // the other finds it by name.
  def invite(port: Int, lifetime: Duration)(using backend: Discovery.Backend^{} = Swarm.discovery)
    ( using Environment, Monitor, Probate, Tactic[Peer.Error] )
  :   Invitation

  // What the other end of a link asks of this node; only a hub does anything.
  def requested(tether: Tether, request: Wire.Request)(using Monitor, Probate): Unit =
    tether.send(Wire.Reply(request.id, false, t"not a hub"))

  // ── the bus ──────────────────────────────────────────────────────────────────────────────

  // Every node known to this one, itself included, with what each last advertised; and every
  // activity of every other node, as it was last updated.
  val nodes: Live[List[Advert]] = Live(List(self))
  val activities: Live[List[(Node, Activity)]] = Live(Nil)

  // When each node was last heard from, by key, and the newest sequence heard from each
  // origin, by which an event heard twice is acted on once.
  protected val heard: TrieMap[Text, Long] = TrieMap()
  private val seen: TrieMap[Text, Long] = TrieMap()
  protected val sequence: juc.atomic.AtomicLong = juc.atomic.AtomicLong(0L)

  @scala.caps.unsafe.untrackedCaptures
  private var subscribers: List[(Filter, (Optional[Node], Envelope) -> Unit)] = Nil

  // The node of this key, if it is known.
  def known(key: Text): Optional[Node] = nodes().seek(_.node.key == key).let(_.node)

  // Hears `handler` of every event the filter admits, until cancelled. The handler is the
  // tool's own, and outlives any one event, so it is vouched pure as a `Live`'s wakes are.
  def subscribe(filter: Filter = Filter())(handler: (Optional[Node], Envelope) => Unit)
  :   Subscription =

    val pure: (Optional[Node], Envelope) -> Unit = scala.caps.unsafe.unsafeAssumePure(handler)
    val entry: (Filter, (Optional[Node], Envelope) -> Unit) = (filter, pure)
    mutex { subscribers = entry :: subscribers }

    new Subscription:
      def cancel(): Unit = mutex { subscribers = subscribers.filter(_ ne entry) }

  // Publishes an event of this tool's own, as a self-contained document (`Bus.payload`); false
  // if it is larger than the bus allows, or the tool has published too many this second.
  def publish(kind: Text, payload: Data): Boolean =
    if payload.length > Bus.limit then
      Journal.log(Event.Oversized(tool, kind, payload.length))
      false
    else if !spend() then
      Journal.log(Event.Throttled(tool, kind))
      false
    else
      emit(Bus.Event.Tool(tool, kind, payload))
      true

  // The second's budget of events.
  private val window: juc.atomic.AtomicLong = juc.atomic.AtomicLong(0L)
  private val spent: juc.atomic.AtomicInteger = juc.atomic.AtomicInteger(0)

  private def spend(): Boolean =
    val second: Long = java.lang.System.currentTimeMillis/1000L
    if window.getAndSet(second) != second then spent.set(0)
    spent.incrementAndGet() <= Swarm.budget

  // Numbers an event of this node's own, acts on it here, and sends it down every link.
  protected def emit(event: Bus.Event): Unit =
    val envelope: Envelope = Envelope(node.key, sequence.incrementAndGet(), now(), 0, event)
    seen(node.key) = envelope.sequence
    notify(node, envelope)
    broadcast(envelope, Unset)

  // Sends an envelope down every link but the one it arrived by, and the one to its origin: a
  // link counts once the other end's snapshot has arrived, which is before its first beat.
  protected def broadcast(envelope: Envelope, from: Optional[Tether]): Unit =
    val carry: Wire = Wire.carry(envelope)

    mutex(tethers).each: tether =>
      val notSource: Boolean = from.lay(true)(_ ne tether)
      val notOrigin: Boolean = tether.node.lay(true)(_.key != envelope.origin)
      if notSource && notOrigin && tether.side != Tether.Side.Unknown then tether.send(carry)

  // Acts on an event arriving by a link: once, by its origin's numbering; then passes it on,
  // if it has not travelled too far.
  def carried(tether: Tether, envelope: Envelope): Unit =
    val fresh: Boolean = mutex:
      val before: Long = seen.getOrElse(envelope.origin, 0L)
      if envelope.sequence > before then seen(envelope.origin) = envelope.sequence
      envelope.sequence > before

    if fresh then
      heard(envelope.origin) = java.lang.System.currentTimeMillis
      val origin: Optional[Node] = apply(envelope.event)
      notify(origin, envelope)

      if envelope.hops < Swarm.reach
      then broadcast(envelope.copy(hops = envelope.hops + 1), tether)

  // What a snapshot says: the other end's advert, and the adverts it holds of nodes this one
  // may not know. A snapshot is link-local, so nothing of it is passed on: the nodes it names
  // will advertise themselves in time, and their adverts travel. The link is now known on
  // this side, so this node says what it is again, as an event, for the nodes beyond the other
  // end which the snapshot did not reach.
  def snapshotted(tether: Tether, advert: Advert, known: List[Advert]): Unit =
    heard(advert.node.key) = java.lang.System.currentTimeMillis
    apply(Bus.Event.Advertised(advert))

    known.each: other =>
      if other.node.key != node.key && this.known(other.node.key).absent then
        heard(other.node.key) = java.lang.System.currentTimeMillis
        apply(Bus.Event.Advertised(other))

    advertise()

  // Updates what this node knows from one event; the node the event is about, if known.
  private def apply(event: Bus.Event): Optional[Node] = event match
    case Bus.Event.Advertised(advert) =>
      val before: Optional[Node] = known(advert.node.key)
      nodes.amend { all => advert :: all.filter(_.node.key != advert.node.key) }
      if before.absent && advert.node.key != node.key then Journal.log(Event.Appeared(advert.node))
      advert.node

    case Bus.Event.Left(gone) =>
      forget(gone)
      gone

    case Bus.Event.Updated(origin, text) =>
      Bus.decode(text).let: activity =>
        activities.amend: all =>
          (origin, activity) :: all.filter { (other, existing) => other.key != origin.key || existing.id != activity.id }
      origin

    case Bus.Event.Ended(origin, id) =>
      activities.amend(_.filter { (other, existing) => other.key != origin.key || existing.id != id })
      origin

    case Bus.Event.Tool(_, _, _) =>
      Unset

  protected def forget(gone: Node): Unit =
    val was: Boolean = known(gone.key).present
    nodes.amend(_.filter(_.node.key != gone.key))
    activities.amend(_.filter(_(0).key != gone.key))
    heard.remove(gone.key)
    if was then Journal.log(Event.Vanished(gone))

  private def notify(origin: Optional[Node], envelope: Envelope): Unit =
    mutex(subscribers).each: (filter, handler) =>
      if filter.admits(origin, envelope, node) then
        try handler(origin, envelope) catch case _: Exception => ()

  // ── activities ───────────────────────────────────────────────────────────────────────────

  // The activities this node has published, by id, and those changed since the last flush.
  private val published: TrieMap[Text, Activity] = TrieMap()
  private val pending: TrieMap[Text, Activity] = TrieMap()

  // Publishes every change to `live`'s activities: an ended one at once, a changed one at the
  // next flush, a quarter-second away at most, so that a chatty run sends its latest state
  // and not every state.
  def expose(live: Live[List[Activity]])(using Monitor): Unit =
    live.bindWake: () =>
      val current: List[Activity] = live()
      val ids: Set[Text] = current.map(_.id).to[Set]

      published.keys.toList.foreach: id =>
        if !ids.has(id) then
          published.remove(id)
          pending.remove(id)
          emit(Bus.Event.Ended(node, id))

      current.each: activity =>
        if published.get(activity.id) != Some(activity) then pending(activity.id) = activity

  protected def flush()(using Monitor): Unit =
    pending.keys.toList.foreach: id =>
      pending.remove(id).foreach: activity =>
        published(id) = activity
        emit(Bus.Event.Updated(node, Bus.encode(activity)))

  // ── links ────────────────────────────────────────────────────────────────────────────────

  // Every link, whichever side it is on and whichever end made it.
  @scala.caps.unsafe.untrackedCaptures
  protected var tethers: List[Tether] = Nil

  // The machines this node is asked to stay linked to, and the links it has made, by the
  // machine's name, or a local node's record name.
  @scala.caps.unsafe.untrackedCaptures
  protected var wanted: List[Machine] = Nil

  @scala.caps.unsafe.untrackedCaptures
  protected var links: List[(Text, Tether)] = Nil

  protected def link(name: Text): Optional[Tether] = mutex(links.filter(_(0) == name)).head.let(_(1))
  protected def wants(name: Text): Boolean = mutex(wanted.filter(_.name == name)).head.present

  // What this node is asked to stay linked to, and how each link stands. A leaf asks the hub,
  // whose links they are, hence the monitor.
  def connections(using Monitor): List[Connection] =
    def standing(machine: Machine): Tether.Standing = link(machine.name) match
      case link: Tether => link.standing
      case _            => Tether.Standing()

    mutex(wanted).reverse.map: machine => Connection(machine, standing(machine))

  // The callers from other machines which have made a lasting link to this node.
  def visitors(using Monitor): List[Caller] =
    mutex(tethers).reverse.filter { t => t.lasting && t.side == Tether.Side.Remote && link(t.peer.show).absent }
    . map: link => Caller(link.peer, link.standing)

  // The other nodes of this machine this one is linked to, and how each stands.
  def neighbours: List[(Node, Tether.Standing)] =
    mutex(tethers).reverse.filter(_.side == Tether.Side.Local).bind: tether =>
      tether.node.lay(Nil: List[(Node, Tether.Standing)]) { node => List((node, tether.standing)) }

  // Whether a lasting link to the machine of this name is open now.
  def connected(name: Text)(using Monitor): Boolean = link(name).present

  protected def hostname(text: Text): Optional[Text] = if text == t"" then Unset else text

  protected def welcomed(peer: Party, session: Peer.Session[Data]): Unit =
    val host: Optional[Text] = hostname(session.peer.identity.hostname)
    Journal.log(Event.Welcomed(peer, host, hostname(session.peer.version), session.address))

  // One caller's link, for as long as the caller keeps it: a caller which sends a beat means to
  // stay, and is sent beats in return, and watched for silence.
  protected def answer(session: Peer.Session[Data])(using Monitor, Probate): Unit =
    val peer: Party = Party.Caller(hostname(session.peer.identity.hostname))
    Journal.log(Event.Accepted(peer, hostname(session.peer.version)))

    Tether.negotiate(peer, session).let: theirs =>
      val tether: Tether = Tether(peer, hostname(session.peer.version), session, theirs, this)
      mutex { tethers = tether :: tethers }
      tether.snapshot(nodes())

      def lasting(): Unit =
        Journal.log(Event.Linked(peer))
        async(tether.beat())

      tether.read(lasting())
      ended(tether)
      if !tether.lost then Journal.log(Event.Closed(peer))

  // A link has closed: what was known through it alone is forgotten, and the other side told.
  protected def ended(tether: Tether)(using Monitor): Unit =
    tether.close()
    mutex { tethers = tethers.filter(_ ne tether) }
    tether.node.let(severed(tether, _))

  // What a closed link to `other` took with it: that node, if it was on this machine; every
  // node of its machine otherwise, all of which were reached through this link alone.
  protected def severed(tether: Tether, other: Node)(using Monitor): Unit =
    tether.side match
      case Tether.Side.Local =>
        forget(other)
        broadcast(Envelope(node.key, sequence.incrementAndGet(), now(), 1, Bus.Event.Left(other)), tether)

      case Tether.Side.Remote =>
        nodes().filter(_.node.identity == other.identity).each: advert =>
          forget(advert.node)
          broadcast(Envelope(node.key, sequence.incrementAndGet(), now(), 1, Bus.Event.Left(advert.node)), tether)

      case _ =>
        ()

  // Makes a link to a machine and keeps it for as long as it lasts.
  protected def dial(name: Text, machine: Machine, defaultPort: Int)(using Monitor, Probate)
  :   Attempt[Unit, Peer.Error] =

    val peer: Party = Party.Callee(machine)

    attempt[Peer.Error]:
      Peer.connect[Data, Unit](machine, Wire.tool, version, Wire.codec, defaultPort): session =>
        welcomed(peer, session)

        Tether.negotiate(peer, session).let: theirs =>
          val tether: Tether = Tether(peer, hostname(session.peer.version), session, theirs, this)
          mutex { tethers = tether :: tethers; links = (name, tether) :: links }
          Journal.log(Event.Linked(peer))
          tether.snapshot(nodes())
          async(tether.beat())
          tether.read(())
          mutex { links = links.filter(_(0) != name) }
          ended(tether)
          if !tether.lost && wants(name) then Journal.log(Event.Closed(peer))

  // Keeps a lasting link to `machine` from now on, in the background, under the daemon's
  // monitor: until `disconnect`, or the daemon's end. Nothing more is done for a machine this
  // node is keeping a link to already.
  def connect(machine: Machine)(using Monitor, Probate): Unit =
    val added: Boolean = mutex:
      val absent: Boolean = wanted.filter(_.name == machine.name).head.absent
      if absent then wanted = machine :: wanted
      absent

    if added then async(keep(machine, 0))

  // Stops keeping a link to the machine of this name, closing it if it is open.
  def disconnect(name: Text)(using Monitor): Boolean =
    val found: Boolean = mutex:
      val present: Boolean = wanted.filter(_.name == name).head.present
      wanted = wanted.filter(_.name != name)
      present

    link(name).let(_.close())
    found

  def disconnect()(using Monitor): Unit = mutex(wanted).each: machine => disconnect(machine.name)

  // Makes the link, keeps it for as long as it lasts, and makes it again — after a pause which
  // grows with each failure in a row — until this node no longer wants it.
  protected def keep(machine: Machine, failures: Int)(using Monitor, Probate): Unit =
    val peer: Party = Party.Callee(machine)
    var count: Int = failures

    while wants(machine.name) do
      Journal.log(Event.Connecting(machine, machine.portOr(swarmPort)))

      count = dial(machine.name, machine, swarmPort) match
        case Attempt.Failure(error) =>
          Journal.log(Event.Failed(machine, Swarm.Error.Reason.Connection(error.reason)))
          count + 1

        case Attempt.Success(_) =>
          0

      if wants(machine.name) then
        Journal.log(Event.Retrying(machine, Swarm.delay(count)))
        snooze(Swarm.delay(count))

    Journal.log(Event.Unlinked(peer))

  // ── this node ────────────────────────────────────────────────────────────────────────────

  protected def coalesce()(using Monitor): Unit =
    while !stopping() do
      flush()
      snooze(Swarm.coalescing)

  // Says what this node is now: when something it offers has changed, and every half minute
  // regardless, so that a node which joined since, or forgot it, learns of it.
  def advertise(): Unit =
    val advert: Advert = self
    nodes.amend { all => advert :: all.filter(_.node.key != node.key) }
    emit(Bus.Event.Advertised(advert))

  protected def announce()(using Monitor, Probate): Unit =
    while !stopping() do
      advertise()
      snooze(Swarm.announcement)

  // Says goodbye, stops what the node does of its own, and closes every link.
  private[pyrocosm] def stop()(using Monitor): Unit =
    if !stopping.swap(true) then
      broadcast(Envelope(node.key, sequence.incrementAndGet(), now(), 0, Bus.Event.Left(node)), Unset)
      halt()
      disconnect()
      mutex(tethers).each(_.close())
      retire()

  // ── joining ──────────────────────────────────────────────────────────────────────────────

  // Accepts `invitation`: links to the machine it invites to, as `name`, says what this node
  // accepts, and records the machine and the token it granted, for `connect`.
  def join(invitation: Invitation, name: Text)(using Environment)
  :   Machine raises Invitation.Error raises Peer.Error =

    val peer: Party = Party.Callee(invitation.machine(name))

    val (machine, _) =
      Peer.join(invitation, name, Wire.tool, version, Wire.codec): session =>
        welcomed(peer, session)
        Tether.negotiate(peer, session)

    Journal.log(Event.Joined(machine))
    machine

  // The machines this one has admitted by invitation.
  def peers(using Environment): List[Text] = Peer.peers.filter(_(1) == Wire.tool).map(_(0))

  // Refuses the machine of this name from now on; how many tokens that undid.
  def revoke(name: Text)(using Environment): Int =
    val count: Int = Peer.revoke(name, Wire.tool)
    Journal.log(Event.Revoked(name, count))
    count

  // ── pinging ──────────────────────────────────────────────────────────────────────────────

  private def failed(machine: Machine, reason: Swarm.Error.Reason): Swarm.Error.Reason =
    Journal.log(Event.Failed(machine, reason))
    reason

  // A `ping` over a link made for the purpose, and closed once it is answered.
  private def once(machine: Machine, note: Text): Swarm.Reply raises Swarm.Error =
    val peer: Party = Party.Callee(machine)
    Journal.log(Event.Connecting(machine, machine.portOr(swarmPort)))

    mitigate:
      case Peer.Error(reason) => Swarm.Error(machine, failed(machine, Swarm.Error.Reason.Connection(reason)))

    . protect:
        Peer.connect[Data, Swarm.Reply](machine, Wire.tool, version, Wire.codec, swarmPort):
          session =>
            welcomed(peer, session)

            val theirs: Tel.Acceptance = Tether.negotiate(peer, session).or:
              abort(Swarm.Error(machine, failed(machine, Swarm.Error.Reason.Mismatched)))

            val tether: Tether = Tether(peer, hostname(session.peer.version), session, theirs, this)
            val sent: Instant over Unix = now()
            tether.send(Wire.Ping(Uuid(), sent, note))

            // The answer is the first `pong`; the snapshot the other end sends first is read
            // past, as is anything else.
            def answer(): Optional[Wire.Pong] =
              session.receive() match
                case Channel.Frame.Message(document) => Wire.read(document) match
                  case pong: Wire.Pong => pong
                  case _: Wire         => answer()
                  case _               => Unset
                case _ => Unset

            answer() match
              case pong: Wire.Pong =>
                Journal.log(Event.Received(pong, peer))
                Swarm.Reply(pong.from, hostname(session.peer.version), now() - sent)

              case _ =>
                abort(Swarm.Error(machine, failed(machine, Swarm.Error.Reason.Unanswered)))

  // Says `ping` to `machine` and waits for its `pong`: over the lasting link to it, if there is
  // one, and otherwise over a link made for the purpose. A failure is logged, to the daemon's
  // journal, as it is raised.
  def ping(machine: Machine, note: Text)(using Monitor): Swarm.Reply raises Swarm.Error =

    link(machine.name) match
      case tether: Tether =>
        tether.ask(note).lay(abort(Swarm.Error(machine, failed(machine, Swarm.Error.Reason.Silent)))): (pong, elapsed) =>
          Swarm.Reply(pong.from, tether.release, elapsed)

      case _ =>
        once(machine, note)
