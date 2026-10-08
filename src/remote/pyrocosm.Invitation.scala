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

import java.nio.file as jnf

import soundness.*

import alphabets.hexLowerCase
import codepages.utf8Codepage
import filesystemBackends.javaBaseFilesystem
import logging.silentLogging
import stratiform.{Base256, TelSchematic}
import systems.javaBaseSystem
import textSanitizers.skipSanitizer

object Invitation:
  // An instant is written as the milliseconds since the Unix epoch, and a fingerprint as hex.
  given instantSchematic: (Instant over Unix) is TelSchematic over Tels.Type =
    () => Tels.Scalar(Array.empty)

  given dataSchematic: Data is TelSchematic over Tels.Type = () => Tels.Scalar(Array.empty)

  given instantEncodable: (Instant over Unix) is Tel.Encodable =
    Tel.Encodable(() => Morphology.Whole, Tel.Nature.Scalar): instant =>
      Tel.scalar(instant.long.show)

  given instantDecodable: Tactic[Tel.Error] => (Instant over Unix) is Tel.Decodable =
    Tel.Decodable(() => Morphology.Whole, Tel.Nature.Scalar): tel =>
      Instant.of[Unix](summon[Long is Tel.Decodable].decoded(tel))

  given dataEncodable: Data is Tel.Encodable =
    Tel.Encodable(() => Morphology.Str, Tel.Nature.Scalar): data => Tel.scalar(data.serialize[Hex])

  given dataDecodable: Tactic[Tel.Error] => Data is Tel.Decodable =
    Tel.Decodable(() => Morphology.Str, Tel.Nature.Scalar): tel =>
      unsafely(summon[Text is Tel.Decodable].decoded(tel).deserialize[Hex])

  private lazy val schema: Tels =
    import Channel.derivation.throwing
    Tels.tels[Invitation](t"pyrocosm-invitation")

  // The invitation as one word, its BinTEL form in BASE-256.
  def encode(invitation: Invitation): Text = Base256.encode(Channel.encode(invitation, schema))

  // The invitation a word is, if it is one, and has not expired.
  def parse(word: Text): Invitation raises Error =
    import Channel.derivation.throwing

    val invitation: Optional[Invitation] =
      try Channel.decode[Invitation](Base256.decode(word.trim)) catch case _: Exception => Unset

    invitation match
      case invitation: Invitation =>
        if invitation.identity.length != 32 || invitation.hosts.nil
        then abort(Error(Error.Reason.Malformed))
        else if invitation.expires < now() then abort(Error(Error.Reason.Expired))
        else invitation

      case _ =>
        abort(Error(Error.Reason.Malformed))

  object Error:
    enum Reason(val number: Int) extends Clarification:
      case Malformed        extends Reason(1)
      case Expired          extends Reason(2)
      case Used             extends Reason(3)
      case Tool(tool: Text) extends Reason(4)
      case Unwritable       extends Reason(5)

    given communicable: Reason is Communicable =
      case Reason.Malformed  => m"it is not an invitation"
      case Reason.Expired    => m"it has expired"
      case Reason.Used       => m"it has been used already, or has expired"
      case Reason.Tool(tool) => m"it is an invitation to $tool"
      case Reason.Unwritable => m"the machine it invites to could not be recorded"

  case class Error(reason: Error.Reason)(using Diagnostics)
  extends fulminate.Error(6041, reason.number)(m"the invitation was not accepted because $reason")

// An invitation to connect to a machine: everything a tool on another machine needs to reach
// this one and be let in, written as one word to be copied from one to the other. It names the
// machine, every address it may be reached at (`Machine.addresses`, nearest first), the port
// the inviting tool listens on, the fingerprint of the certificate it will present, and a token
// which admits its bearer ONCE, before `expires`: the listener exchanges it, on first use, for a
// token of the joiner's own (`Peer.join`), and forgets it.
case class Invitation
  ( name:     Text,
    hosts:    List[Text],
    port:     Int,
    identity: Data,
    token:    Text,
    expires:  Instant over Unix,
    tool:     Text ):

  // The machine the invitation is to, as a joiner declares it before it has a token of its own.
  def machine(name: Text): Machine = Machine(name, hosts, port, identity, token, Nil)

// How a listener let a caller in, by the token it presented: the machine's own token, a token
// it granted a joiner, or an invitation's, which it has just exchanged for `granted`.
enum Admission:
  case Owner
  case Peer(name: Text)
  case Invited(granted: Text)
  case Refused

object Admissions:
  // The tool whose grants admit to every tool.
  val swarm: Text = t"pyrocosm"

  def apply()(using Environment): Admissions = new Admissions(Peer.directory)

  private[pyrocosm] def restrict(path: Path on Linux): Unit =
    try
      jnf.Files.setPosixFilePermissions
        ( jnf.Path.of(path.encode.s), jnf.attribute.PosixFilePermissions.fromString("rw-------") )
    catch case _: Exception => ()

  private[pyrocosm] def random(bytes: Int): Text =
    val array = new scala.Array[Byte](bytes)
    java.security.SecureRandom().nextBytes(array)
    Array.unsafeFrozen(array).serialize[Hex]

// The tokens a machine's listeners admit beside its own: the invitations it has issued and not
// seen used, and the tokens it granted in exchange for them, each kept as the SHA-256 of the
// token alone, never the token, under `$XDG_STATE_HOME/pyrocosm/remote/`, in `invites/` and
// `peers/`. Each file is a small TEL document. Shared by every tool on the machine, as its
// identity is, though an invitation, and the token it is exchanged for, admit to one tool.
class Admissions(directory: Optional[Path on Linux]):
  private def under(name: Text): Optional[Path on Linux] =
    directory.let: directory => safely(directory / name)

  private def invites: Optional[Path on Linux] = under(t"invites")
  private def granted: Optional[Path on Linux] = under(t"peers")

  private def file(in: Optional[Path on Linux], token: Text): Optional[Path on Linux] =
    in.let: directory => safely(directory / Blobs.digest(token.sysData))

  private def record(path: Path on Linux, fields: List[(Text, Text)]): Unit =
    safely:
      path.parent.let: parent =>
        if !parent.existent() then parent.create[Directory](CreateFlag.Parents)

      path.write(fields.map { (key, value) => t"$key $value\n" }.join)
      Admissions.restrict(path)

  private def read(path: Path on Linux): Optional[Tel] =
    if !path.existent() then Unset else safely(path.read[Data]).let: data => safely(data.read[Tel])

  private def field(tel: Tel, keyword: Text): Optional[Text] =
    tel.field(keyword).let(_.primaryAtom)

  // Records an invitation's token, to admit one caller of `tool` before `expires`.
  def issue(token: Text, tool: Text, expires: Instant over Unix): Unit =
    file(invites, token).let: path =>
      record(path, List(t"tool" -> tool, t"expires" -> expires.long.show))

  // Decides what a caller presenting `presented`, hostname `hostname`, is to `tool` on a machine
  // whose own token is `own`. An invitation's token is consumed here, by deleting its record —
  // which only one caller can do, however many arrive at once, from however many tools — and a
  // token is granted in its place. A token granted to the swarm (`pyrocosm`) admits its bearer
  // to every tool: joining a machine's swarm is joining it for all of them.
  def admit(presented: Text, own: Text, tool: Text, hostname: Text): Admission =
    if presented == own then Admission.Owner else
      val peer: Optional[Tel] = file(granted, presented).let(read(_))

      peer match
        case peer: Tel =>
          val admitted: Optional[Text] = field(peer, t"tool")

          if admitted == tool || admitted == Admissions.swarm
          then Admission.Peer(field(peer, t"name").or(hostname))
          else Admission.Refused

        case _ =>
          file(invites, presented).lay(Admission.Refused): path =>
            val invite: Optional[Tel] = read(path)

            val live: Boolean = invite.lay(false): invite =>
              val expires: Optional[Long] = field(invite, t"expires").let: text =>
                safely(text.as[Long])

              field(invite, t"tool") == tool && expires.lay(false)(Instant.of[Unix](_) >= now())

            val claimed: Boolean =
              try
                jnf.Files.delete(jnf.Path.of(path.encode.s))
                true
              catch case _: Exception => false

            if !(live && claimed) then Admission.Refused else
              val token: Text = Admissions.random(32)

              file(granted, token).let: path =>
                val granted: Text = now().long.show
                record(path, List(t"name" -> hostname, t"tool" -> tool, t"granted" -> granted))

              Admission.Invited(token)

  // The machines this one has admitted by invitation, and to which tool.
  def peers: List[(Text, Text)] =
    granted.lay(Nil): directory =>
      safely(directory.children.to[List]).or(Nil).bind: path =>
        safely(path.as[Path on Linux]).let(read(_)).lay(Nil): tel =>
          List((field(tel, t"name").or(t""), field(tel, t"tool").or(t"")))

  // Forgets every token granted to the machine of this name, for `tool`: it will be refused from
  // now on. How many there were.
  def revoke(name: Text, tool: Text): Int =
    granted.lay(0): directory =>
      val paths = safely(directory.children.to[List]).or(Nil)

      paths.map: path =>
        safely(path.as[Path on Linux]).lay(0): path =>
          read(path).lay(0): tel =>
            if field(tel, t"name") == name && field(tel, t"tool") == tool then
              safely(jnf.Files.delete(jnf.Path.of(path.encode.s))).lay(0)(_ => 1)
            else
              0

      . fold(0)(_ + _)
