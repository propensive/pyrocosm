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

import Journal.{Event, Mismatch, Party}

object Tether:
  // What is known of the other end of a link: when it was last heard from, what it advertised
  // itself to be, and how loaded it last said it was. All are absent of a machine this node is
  // asked to stay linked to but is not linked to now. Nested, as the model has a `Standing` of
  // its own.
  case class Standing
    ( heard:  Optional[Instant over Unix] = Unset,
      advert: Optional[Advert]            = Unset,
      load:   Optional[Double]            = Unset )

  // Which side of a machine's gateway a link is on: to another node of this machine, or to
  // another machine. Unknown until the other end's snapshot says which machine it is on.
  enum Side:
    case Local, Remote, Unknown

  // What a tether tells the swarm it belongs to: a snapshot arrived, an envelope arrived, or
  // the link ended.
  trait Host:
    def self: Advert
    def snapshotted(tether: Tether, self: Advert, known: List[Advert]): Unit
    def carried(tether: Tether, envelope: Envelope): Unit

  // What each end does first, once Pyrocosm has welcomed the connection: sends its acceptance
  // (BinTEL §8.4), which says what forms of the protocol it can read, and reads the other end's.
  // Both send before either reads, so neither waits on the other. What comes back is the other
  // end's acceptance, if it sent one and this node can write to it.
  def negotiate(peer: Party, session: Peer.Session[Data])(using LogSink[Any, Message]^{})
  :   Optional[Tel.Acceptance] =

    val frame: Channel.Frame[Data] =
      try
        session.send(Wire.offer)
        session.receive()
      catch case _: Exception => Channel.Frame.Closed

    val theirs: Optional[Tel.Acceptance] = frame match
      case Channel.Frame.Message(document) => Wire.offered(document)
      case _                               => Unset

    theirs.lay(mismatch(peer, Mismatch.NoAcceptance)): theirs =>
      val probe: Wire = Wire.Beat(now(), Unset)

      if Wire.write(probe, theirs).absent then mismatch(peer, Mismatch.Unservable) else
        Journal.log(Event.Negotiated(peer))
        theirs

  private def mismatch(peer: Party, mismatch: Mismatch)(using LogSink[Any, Message]^{})
  :   Optional[Tel.Acceptance] =

    Journal.log(Event.Unnegotiated(peer, mismatch))
    Unset

  // How often each end of a link says it is still there, and how long an end waits, hearing
  // nothing, before it takes the link for lost.
  val interval: Duration = 1.0*Second
  val patience: Duration = 3.0*Second

// One link, at either end of it. Both ends do the same things with it: answer a `ping`, deliver
// a `pong` to whoever asked for it, hand a `snapshot` or a `carry` to the swarm, and — once the
// link is a lasting one — send a `beat` each second and listen for the other end's.
class Tether
  ( val peer:    Party,
    val release: Optional[Text],
    session:     Peer.Session[Data],
    theirs:      Tel.Acceptance,
    host:        Tether.Host )
  ( using sink: LogSink[Any, Message]^{} ):

  private val mutex: Mutex = Mutex()
  private val open: Atomic[Boolean] = Atomic(true)
  private val beating: Atomic[Boolean] = Atomic(false)
  private val silent: Atomic[Boolean] = Atomic(false)

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var last: Instant over Unix = now()

  @scala.caps.unsafe.untrackedCaptures
  private var asked: List[(Uuid, Promise[Wire.Pong])] = Nil

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var told: Optional[Advert] = Unset

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var burden: Optional[Double] = Unset

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var side0: Tether.Side = Tether.Side.Unknown

  // When the other end was last heard from, what it advertised, and its load at its last beat.
  def standing: Tether.Standing = Tether.Standing(last, told, burden)

  // The node at the other end, once its snapshot has arrived.
  def node: Optional[Node] = told.let(_.node)

  // Whether the other end is on this machine, on another, or has not yet said.
  def side: Tether.Side = side0

  // Whether this is a lasting link, on which beats have begun.
  def lasting: Boolean = beating()

  // Whether the link was given up for lost, having gone silent.
  def lost: Boolean = silent()

  def close(): Unit = if open.swap(false) then safely(session.close())

  // Says what this node is, and what it knows of the others.
  def snapshot(known: List[Advert]): Unit = send(Wire.Snapshot(host.self, known))

  // Writes a message as the other end said it can read it, having logged it; a link which
  // cannot be written to is closed. A message the other end does not accept is not sent, and
  // the log says so.
  def send(message: Wire): Unit =
    Wire.write(message, theirs).lay(Journal.log(Event.Unaccepted(message, peer))): document =>
      Journal.log(Event.Sent(message, peer))
      try session.send(document) catch case _: Exception => close()

  // Sends a beat each second for as long as the link is open, and gives it up for lost if the
  // other end has not been heard from for too long. Run on a task of its own.
  def beat()(using Monitor): Unit =
    while open() do
      val silence: Duration = now() - last

      if silence > Tether.patience then
        Journal.log(Event.Lost(peer, silence))
        silent() = true
        close()
      else
        send(Wire.Beat(now(), host.self.load))
        snooze(Tether.interval)

  // Reads what the other end says until the link closes. `lasting` is run when the first beat
  // arrives, which is how a listener learns that the caller means to stay.
  def read(lasting: => Unit): Unit =
    var reading: Boolean = true

    while reading do
      val frame: Channel.Frame[Data] =
        try session.receive() catch case _: Exception => Channel.Frame.Closed

      frame match
        case Channel.Frame.Message(document) =>
          Wire.read(document) match
            case beat: Wire.Beat =>
              last = now()
              burden = beat.load
              Journal.log(Event.Received(beat, peer))
              if !beating.swap(true) then lasting

            case snapshot: Wire.Snapshot =>
              last = now()
              told = snapshot.self
              side0 = if snapshot.self.node.shares(host.self.node) then Tether.Side.Local else Tether.Side.Remote
              Journal.log(Event.Received(snapshot, peer))
              host.snapshotted(this, snapshot.self, snapshot.known)

            case ping: Wire.Ping =>
              last = now()
              Journal.log(Event.Received(ping, peer))
              send(Wire.Pong(ping.id, now(), host.self.node.key))

            case pong: Wire.Pong =>
              last = now()
              Journal.log(Event.Received(pong, peer))
              mutex(asked.filter(_(0) == pong.id)).each: (_, promise) => promise.offer(pong)

            case other: Wire =>
              Wire.envelope(other) match
                case envelope: Envelope =>
                  last = now()
                  Journal.log(Event.Received(other, peer))
                  host.carried(this, envelope)

                case _ =>
                  Journal.log(Event.Unread(peer))

            case _ =>
              Journal.log(Event.Unread(peer))

        case Channel.Frame.Closed =>
          close()
          reading = false

        case _ =>
          ()

  // Says `ping` and waits for the `pong` which answers it, which `read` delivers; the round
  // trip's duration, or nothing if no pong came in time.
  def ask(note: Text)(using Monitor): Optional[(Wire.Pong, Duration)] =
    val id: Uuid = Uuid()
    val promise: Promise[Wire.Pong] = Promise()
    mutex { asked = (id, promise) :: asked }
    val sent: Instant over Unix = now()
    send(Wire.Ping(id, sent, note))
    val pong: Optional[Wire.Pong] = safely(promise.await(Tether.patience))
    mutex { asked = asked.filter(_(0) != id) }
    pong.let { pong => (pong, now() - sent) }
