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


// `Service` is excluded from the umbrella: pyrocosm's own, declared in another file, would be
// outranked by urticose's.
import soundness.{Service as _, *}

import Journal.{Event, Obstacle, Party}
import denominative.dysasymptotics.{linearAccess, linearSize}
import errorDiagnostics.emptyDiagnostics

// A tool's node: a leaf of the machine's hub. It finds the hub through the registry, keeps one
// link to it — made again, after a pause which grows with each failure, whenever it ends —
// and, finding no hub, has one started (`revive`: the tool's daemon spawns the `pyrocosm`
// process; a test does nothing). Everything it knows of other nodes it knows through that
// link, so when the link ends it forgets them all. What only the hub can do — listen for
// other machines, advertise an invitation, keep a link to another machine — it asks of the
// hub, and what a tool does with files the machine shares (joining by an invitation, revoking
// a machine) it does itself.
final class Leaf private[pyrocosm]
  ( tool:         Text,
    version:      Text,
    services:     () -> List[Service],
    capabilities: List[Text],
    swarmPort:    Int,
    revive:       () -> Unit )
  ( using environment: Environment )
extends Swarm(tool, version, services, capabilities, swarmPort):

  import Swarm.{Connection, Caller}

  protected def gateway: Boolean = false

  // The link to the hub, while there is one, and the hub at its other end.
  private def tether: Optional[Tether] = link(Swarm.hubName)
  def hub: Optional[Node] = tether.let(_.node).or(Unset)

  // Whether the leaf is linked to the hub now.
  def linked: Boolean = tether.present

  // Starts the leaf: the link to the hub, kept for as long as the leaf lives, and its
  // announcements.
  private[pyrocosm] def start()(using Monitor, Probate): Unit = (identity, token) match
    case (identity: Peer.Identity, token: Text) =>
      async(keepHub())
      async(announce())
      async(coalesce())
      ()

    case (identity, _) =>
      if identity.absent then Journal.log(Event.ListenFailed(0, Obstacle.NoIdentity))
      else Journal.log(Event.ListenFailed(0, Obstacle.NoToken))

  private[pyrocosm] def halt()(using Monitor): Unit = ()
  private[pyrocosm] def retire(): Unit = ()

  // The hub, as a machine to link to: this machine, at the hub's port, with its own token.
  private def machine(record: Registry.Record): Machine =
    Machine(Swarm.hubName, List(t"127.0.0.1"), record.port, identity.let(_.fingerprint), token, Nil)

  // Links to the hub the registry names, for as long as the link lasts, and again after it
  // ends; finding none, has one started, and looks again after a pause which grows with each
  // failure in a row.
  private def keepHub()(using Monitor, Probate): Unit =
    var failures: Int = 0

    while !stopping() do
      registry.hub() match
        case record: Registry.Record =>
          val hub: Machine = machine(record)
          Journal.log(Event.Connecting(hub, record.port))

          failures = dial(Swarm.hubName, hub, record.port) match
            case Attempt.Failure(error) =>
              Journal.log(Event.Failed(hub, Swarm.Error.Reason.Connection(error.reason)))
              failures + 1

            case Attempt.Success(_) =>
              0

        case _ =>
          revive()
          failures += 1

      if !stopping() then snooze(Swarm.delay(failures))

  // The hub's link has ended: every other node was known through it.
  override protected def severed(tether: Tether, other: Node)(using Monitor): Unit =
    nodes().filter(_.node.key != node.key).each { advert => forget(advert.node) }

  // ── what the hub does for this leaf ──────────────────────────────────────────────────────

  // Asks the hub, if it is linked; nothing, otherwise, or if it did not answer in time.
  private def ask(command: Text, argument: Text)(using Monitor): Optional[Wire.Reply] =
    tether.let(_.request(command, argument)).or(Unset)

  private def done(command: Text, argument: Text)(using Monitor): Boolean =
    ask(command, argument).lay(false)(_.ok)

  // The hub's `status`, as its lines, split into words.
  private def status(using Monitor): List[List[Text]] =
    ask(t"status", t"").let(_.text).or(t"").cut(t"\n").filter(_ != t"").map(_.cut(t" "))

  // The gateway port is the machine's, which the hub's record says.
  def listening: Optional[Int] = registry.hub().let(_.gateway).or(Unset)

  def wantGateway(port: Int, token: Optional[Text])(using Monitor, Probate): Unit =
    done(t"listen", token.lay(port.show) { token => t"${port.show} $token" })
    ()

  def stopGateway()(using Monitor): Unit =
    done(t"unlisten", t"")
    ()

  def advertised(using Monitor): Optional[Text] =
    status.filter(_.head == t"advertised").head.let(_.at(Sec)).or(Unset)

  // The invitation is this leaf's to issue, from the files the machine shares; advertising it
  // is the hub's, which the gateway port belongs to.
  def invite(port: Int, lifetime: Duration)(using backend: Discovery.Backend^{})
    ( using Environment, Monitor, Probate, Tactic[Peer.Error] )
  :   Invitation =

    val invitation: Invitation = Peer.invite(Wire.tool, port, lifetime)
    Journal.log(Event.Invited(invitation.expires))
    done(t"invite", t"${port.show} ${Invitation.encode(invitation)}")
    invitation

  // A link to another machine is the hub's to keep, as the machine's; a leaf asks for it, and
  // a tool quitting does not drop it.
  override def connect(machine: Machine)(using Monitor, Probate): Unit =
    done(t"connect", Machine.block(machine))
    ()

  override def disconnect(name: Text)(using Monitor): Boolean = done(t"disconnect", name)
  override def disconnect()(using Monitor): Unit = ()

  override def connected(name: Text)(using Monitor): Boolean =
    connections.exists { connection => connection.machine.name == name && connection.standing.heard.present }

  private def heard(word: Optional[Text]): Optional[Instant over Unix] =
    word.let { text => safely(text.as[Long]) }.let { millis => if millis == 0L then Unset else Instant.of[Unix](millis) }

  override def connections(using Monitor): List[Connection] =
    status.filter(_.head == t"connection").bind: words =>
      words.at(Sec).lay(Nil: List[Connection]): name =>
        List(Connection(Machine(name, Nil, Unset, Unset, Unset, Nil), Tether.Standing(heard(words.at(Ter)))))

  override def visitors(using Monitor): List[Caller] =
    status.filter(_.head == t"visitor").map: words =>
      val host: Optional[Text] = words.at(Sec).let { text => if text == t"-" then Unset else text }
      Caller(Party.Caller(host), Tether.Standing(heard(words.at(Ter))))
