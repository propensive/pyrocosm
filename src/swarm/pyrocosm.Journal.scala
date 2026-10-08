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

import soundness.*

import calendars.gregorianCalendar
import denominative.dysasymptotics.linearSize
import eucalyptus.logFormats.textLevelLogFormat
import timeFormats.iso8601TimeFormat

// What this daemon's swarm has been doing, as `<tool> swarm log` shows it. Every notable action
// the swarm takes is LOGGED — a method that does so says `logs Journal.Event` in its type, and
// emits the typed event at the level its nature deserves — and the journal keeps what a sink
// for it (`Journal.sink`) is sent: a listener starting, a node linking, each message sent and
// received.
object Journal:
  object Fingerprint:
    given showable: Fingerprint is Showable = fingerprint => Peer.render(fingerprint.data)

  // The SHA-256 fingerprint of the certificate a machine presents.
  case class Fingerprint(data: Data)

  object Obstacle:
    given communicable: Obstacle is Communicable =
      case Unbound    => m"the port could not be bound"
      case NoToken    => m"this machine has no token"
      case NoIdentity => m"this machine's identity could not be created"

  // Why a listener could not start.
  enum Obstacle:
    case Unbound, NoToken, NoIdentity

  object Party:
    private def local(tool: Text, pid: Long): Text = t"$tool (pid ${pid.show})"

    given showable: Party is Showable =
      case Callee(machine)  => machine.name
      case Caller(hostname) => hostname.or(t"an unnamed caller")
      case Local(tool, pid) => local(tool, pid)

  object Mismatch:
    given communicable: Mismatch is Communicable =
      case NoAcceptance => m"it did not begin by saying what it accepts"
      case Unservable   => m"it accepts no form of the protocol this node can write"

  // Why two nodes could not agree what to say to each other.
  enum Mismatch:
    case NoAcceptance, Unservable

  // The other end of a link: a machine this one called, which its configuration names; a
  // caller from elsewhere, known only by the hostname it gave; or another tool's daemon on
  // this machine, known by its tool and process.
  enum Party:
    case Callee(machine: Machine)
    case Caller(hostname: Optional[Text])
    case Local(tool: Text, pid: Long)

  object Event:
    private def milliseconds(duration: Duration): Int = (duration.value*1000.0).toInt

    private def release(version: Optional[Text]): Text =
      version.lay(t"an unknown version"): version => t"version $version"

    // One line per event. An arrow marks a message crossing the wire: `→` leaving this
    // node, `←` arriving at it.
    given communicable: Event is Communicable =
      case Listening(port, fingerprint) =>
        m"opening port ${port.show} to other machines, as ${fingerprint.show}"

      case ListenFailed(port, obstacle) =>
        m"could not listen on port ${port.show}: $obstacle"

      case Stopped(port) =>
        m"stopped listening on port ${port.show}"

      case Accepted(peer, version) =>
        m"accepted a connection from ${peer.show} (${release(version)})"

      case Closed(peer) =>
        m"${peer.show} closed the connection"

      case Connecting(machine, port) =>
        m"connecting to ${machine.name} on port ${port.show}, at ${machine.hosts.join(t", ")}"

      case Welcomed(peer, hostname, version, address) =>
        val name: Text = hostname.or(t"an unnamed machine")
        val at: Text = address.lay(t""): address => t" at $address"
        m"${peer.show} welcomed us$at as $name (${release(version)})"

      case Invited(expires) =>
        m"issued an invitation, good for one machine until ${Journal.time(expires)}"

      case Joined(machine) =>
        m"joined ${machine.name}, and declared it in the shared machines.tel"

      case Advertised(instance, port) =>
        m"advertising this machine on the local network as $instance, on port ${port.show}"

      case Withdrawn(instance) =>
        m"no longer advertising $instance on the local network"

      case Unadvertised(reason) =>
        m"could not advertise this machine on the local network: $reason"

      case Discovered(name, hosts) =>
        m"found $name nearby, at ${hosts.join(t", ")}"

      case Undiscovered(name) =>
        m"$name was not found nearby; trying the addresses the invitation lists"

      case Revoked(name, count) =>
        val tokens: Text = if count == 1 then t"token" else t"tokens"
        m"revoked $name: $count $tokens it was granted"

      case Sent(message, peer) =>
        m"→ ${message.show} to ${peer.show}"

      case Received(message, peer) =>
        m"← ${message.show} from ${peer.show}"

      case Failed(machine, reason) =>
        m"${machine.name}: $reason"

      case Linked(peer) =>
        m"keeping the link with ${peer.show} open, with a beat each second"

      case Lost(peer, silence) =>
        m"lost the link with ${peer.show}: nothing heard for ${milliseconds(silence)}ms"

      case Unlinked(peer) =>
        m"no longer keeping the link with ${peer.show} open"

      case Retrying(machine, delay) =>
        m"trying ${machine.name} again in ${milliseconds(delay)/1000}s"

      case Negotiated(peer) =>
        m"exchanged acceptances with ${peer.show}: each can read what the other sends"

      case Unnegotiated(peer, mismatch) =>
        m"could not agree a protocol with ${peer.show}: $mismatch"

      case Unaccepted(message, peer) =>
        m"${peer.show} does not accept ${message.show}, which was not sent"

      case Unread(peer) =>
        m"${peer.show} sent a document of no form this node accepts"

      case Registered(tool, pid, port) =>
        m"registered $tool (pid ${pid.show}) on this machine, on port ${port.show}"

      case Unregistered(tool, pid) =>
        m"removed the registration of $tool (pid ${pid.show}), which is gone"

      case Gateway(port) =>
        m"this node is the machine's gateway, on port ${port.show}"

      case Ungateway(port) =>
        m"this node is no longer the machine's gateway on port ${port.show}"

      case Appeared(node) =>
        m"${node.tool} on ${node.machine} (pid ${node.pid.show}) is on the swarm"

      case Vanished(node) =>
        m"${node.tool} on ${node.machine} (pid ${node.pid.show}) has left the swarm"

      case Oversized(tool, kind, size) =>
        m"$tool's $kind event of $size bytes is larger than the bus allows, and was not sent"

      case Throttled(tool, kind) =>
        m"$tool's $kind event was dropped: too many events in a second"

  // Each event belongs to the categories of `Log` that describe its nature, by which a sink may
  // choose what to record.
  enum Event:
    case Listening(port: Int, fingerprint: Fingerprint)       extends Event, Log.Network
    case ListenFailed(port: Int, obstacle: Obstacle)          extends Event, Log.Network
    case Stopped(port: Int)                                   extends Event, Log.Network
    case Accepted(peer: Party, version: Optional[Text])       extends Event, Log.Network, Log.Auth
    case Closed(peer: Party)                                  extends Event, Log.Network
    case Connecting(machine: Machine, port: Int)              extends Event, Log.Network

    case Welcomed
      ( peer:    Party,
        host:    Optional[Text],
        version: Optional[Text],
        address: Optional[Text] )
    extends Event, Log.Network, Log.Auth

    case Invited(expires: Instant over Unix)                  extends Event, Log.Auth
    case Joined(machine: Machine)                             extends Event, Log.Auth
    case Revoked(name: Text, count: Int)                      extends Event, Log.Auth

    case Advertised(instance: Text, port: Int)                extends Event, Log.Network
    case Withdrawn(instance: Text)                            extends Event, Log.Network
    case Unadvertised(reason: Discovery.Error.Reason)         extends Event, Log.Network
    case Discovered(name: Text, hosts: List[Text])            extends Event, Log.Network
    case Undiscovered(name: Text)                             extends Event, Log.Network

    case Sent(message: Wire, peer: Party)                     extends Event, Log.Protocol
    case Received(message: Wire, peer: Party)                 extends Event, Log.Protocol
    case Failed(machine: Machine, reason: Swarm.Error.Reason) extends Event, Log.Network
    case Linked(peer: Party)                                  extends Event, Log.Network
    case Lost(peer: Party, silence: Duration)                 extends Event, Log.Network
    case Unlinked(peer: Party)                                extends Event, Log.Network
    case Retrying(machine: Machine, delay: Duration)          extends Event, Log.Network
    case Negotiated(peer: Party)                              extends Event, Log.Protocol
    case Unnegotiated(peer: Party, mismatch: Mismatch)        extends Event, Log.Protocol
    case Unaccepted(message: Wire, peer: Party)               extends Event, Log.Protocol
    case Unread(peer: Party)                                  extends Event, Log.Protocol

    case Registered(tool: Text, pid: Long, port: Int)         extends Event, Log.Network
    case Unregistered(tool: Text, pid: Long)                  extends Event, Log.Network
    case Gateway(port: Int)                                   extends Event, Log.Network
    case Ungateway(port: Int)                                 extends Event, Log.Network
    case Appeared(node: Node)                                 extends Event, Log.Network
    case Vanished(node: Node)                                 extends Event, Log.Network
    case Oversized(tool: Text, kind: Text, size: Int)         extends Event, Log.Protocol
    case Throttled(tool: Text, kind: Text)                    extends Event, Log.Protocol

    // How much each event matters: a failure or a lost link is a warning, or worse if it stops
    // this node doing what its configuration asked; the beats of a link, one a second each way,
    // and the events the bus carries are fine detail, as is the routine welcome.
    def level: Level = this match
      case _: ListenFailed                   => Level.Fail
      case _: Failed | _: Lost               => Level.Warn
      case _: Unnegotiated | _: Unaccepted   => Level.Warn
      case _: Unread | _: Oversized          => Level.Warn
      case _: Unadvertised | _: Throttled    => Level.Warn
      case _: Welcomed                       => Level.Fine
      case Sent(_: Wire.Beat, _)             => Level.Fine
      case Received(_: Wire.Beat, _)         => Level.Fine
      case Sent(message, _) if Wire.envelope(message).present     => Level.Fine
      case Received(message, _) if Wire.envelope(message).present => Level.Fine
      case _                                 => Level.Info

  // An identifier, as the first eight characters by which it is told apart in a log.
  def brief(id: Uuid): Text = id.show.keep(8)

  given wireShowable: Wire is Showable =
    case Wire.Ping(id, _, note) =>
      if note == t"" then t"ping ${brief(id)}" else t"ping ${brief(id)} ‘$note’"

    case Wire.Pong(id, _, _) =>
      t"pong ${brief(id)}"

    case Wire.Beat(_, _) =>
      t"beat"

    case Wire.Snapshot(self, known) => snapshot(self, known)
    case other                      => Wire.envelope(other).lay(t"unknown")(carry(_))

  private def snapshot(self: Advert, known: List[Advert]): Text =
    t"snapshot (${self.node.tool} on ${self.node.machine}, ${known.size.show} others)"

  private def carry(envelope: Envelope): Text =
    val kind: Text = envelope.event match
      case Bus.Event.Advertised(advert) => t"advertised ${advert.node.tool}"
      case Bus.Event.Left(node)         => t"left ${node.tool}"
      case Bus.Event.Updated(node, text) => t"updated ${node.tool}'s ${Bus.decode(text).let(_.id).or(t"activity")}"
      case Bus.Event.Ended(node, id)    => t"ended ${node.tool}'s $id"
      case Bus.Event.Tool(tool, k, _)   => t"$tool's $k"

    t"carry $kind #${envelope.sequence.show}"

  // A logged event as the journal keeps it: numbered in the order it arrived, with its level,
  // the instant it was logged and the message it was transcribed to.
  case class Entry(sequence: Long, level: Level, instant: Instant over Unix, message: Message)

  // The number of entries the daemon's journal keeps; older ones fall off the end.
  val limit: Int = 1000

  // The daemon's own journal, which `<tool> swarm log` reads.
  val daemon: Journal = Journal(limit)

  // A sink which keeps every event logged where it is in scope in the daemon's journal: the
  // event arrives as the `Message` it was transcribed to, with its level and the instant it was
  // logged, which `Log` gives in milliseconds since the Unix epoch.
  //
  // A sink is a capability, since submitting to one is an effect on its destination. This one's
  // destination is the daemon's journal, which is there for as long as the daemon is, so it is
  // vouched pure, as Eucalyptus vouches for its own loggers: it may then be a given wherever
  // something is logged, without each use of it having to be separated from the last.
  val sink: LogSink[Any, Message]^{} = scala.caps.unsafe.unsafeAssumePure:
    new LogSink[Any, Message]:
      def accepts(level: Level): Boolean = true

      def submit(level: Level, timestamp: Long, message: Message): Unit =
        daemon.record(level, Instant.of[Unix](timestamp), message)

  // Logs `event` at its own level, to whichever sinks the caller has in scope.
  def log(event: Event): Unit logs Event = event.level match
    case Level.Fine => Log.fine(event)
    case Level.Info => Log.info(event)
    case Level.Warn => Log.warn(event)
    case Level.Fail => Log.fail(event)

  // The time zone times of day are shown in: this machine's, or UTC if that cannot be had.
  lazy val timezone: Timezone =
    safely(Timezone(java.time.ZoneId.systemDefault.nn.getId.nn.tt)).or(tz"UTC")

  // The time of day, to the millisecond: `09:41:07.215`. The clock face is the instant's, in
  // this machine's time zone; a clock face counts whole seconds, so the thousandths are read
  // from the instant directly.
  def time(instant: Instant over Unix, timezone: Timezone = timezone): Text =
    val thousandths: Text = (instant.long%1000L).show
    t"${(instant in timezone).time.show}.${t"0"*(3 - thousandths.length)}$thousandths"

  def render(entry: Entry): Text =
    t"${time(entry.instant)}  ${entry.level.show}  ${entry.message.text}"

// A journal holding at most `limit` entries, the newest. Entries are numbered from one in the
// order they are recorded, and the numbering is never reused, so a reader that remembers the
// last number it saw can ask for what has happened since.
class Journal(limit: Int):
  import Journal.Entry

  private val mutex: Mutex = Mutex()

  @scala.caps.unsafe.untrackedCaptures
  private var next: Long = 0L

  // Newest first.
  @scala.caps.unsafe.untrackedCaptures
  private var entries: List[Entry] = Nil

  def record(level: Level, instant: Instant over Unix, message: Message): Unit = mutex:
    next += 1L
    entries = (Entry(next, level, instant, message) :: entries).keep(limit)

  // The entries recorded after the one numbered `sequence`, at `level` or above, oldest first;
  // `since(0L)` is everything still held.
  def since(sequence: Long, level: Level = Level.Fine): List[Entry] = mutex:
    def wanted(entry: Entry): Boolean =
      entry.sequence > sequence && entry.level.ordinal >= level.ordinal

    entries.filter(wanted(_)).reverse

  // The number of the newest entry, or zero if nothing has been recorded.
  def latest: Long = mutex(next)
