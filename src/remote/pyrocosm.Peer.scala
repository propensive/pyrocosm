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
import java.security as js

import soundness.*

import alphabets.hexLowerCase
import charsets.utf8Charset
import codepages.utf8Codepage
import filesystemBackends.javaBaseFilesystem
import internetAccess.online
import logging.silentLogging
import systems.javaBaseSystem
import textSanitizers.skipSanitizer

// How two Pyrocosm tools on different machines reach and trust each other. A machine has ONE
// identity, however many tools listen on it: a self-signed certificate generated on the first
// `listen` (by the JDK's own `keytool`) and kept, with its keystore password, under
// `$XDG_STATE_HOME/pyrocosm/remote/`. A controller pins that certificate's fingerprint in its
// `Machine` declaration — `<tool> identity` prints it — and presents a shared token, kept at
// `$XDG_CONFIG_HOME/pyrocosm/token` on the listening machine, in its `hello`. Each connection
// then carries one tool's messages over a `Channel`, after a handshake that is Pyrocosm's own:
//
//   controller → worker   hello    tool, protocol fingerprint, tool version, token, hostname
//   worker → controller   granted  a token of the controller's own, if it presented an invitation's
//   worker → controller   welcome  tool, protocol, version, the worker's identity and capabilities
//                    or   refused  a reason
//
// The `Refused` reasons are fixed words so a tool can render them; the tool's own protocol runs
// from the first frame after `welcome`.
//
// Beside the machine's own token, a listener admits the bearer of an `Invitation`'s token, once,
// before it expires, and sends it `granted` — a token of its own, which goes on admitting it until
// it is revoked — before `welcome` (see `Admissions`). `join` is the other side of that: it
// records the granted token, and declares the machine in the shared `machines.tel`. A machine
// may have several addresses; a connection is made to the nearest which answers (`nearest`).
object Peer:
  enum Handshake:
    case Hello(tool: Text, protocol: Text, version: Text, token: Text, hostname: Text)

    case Welcome
      ( tool:       Text,
        protocol:   Text,
        version:    Text,
        hostname:   Text,
        os:         Text,
        arch:       Text,
        cores:      Int,
        jvm:        Text,
        capability: List[Text] )

    case Refused(reason: Text)

    // Sent before `welcome` to a caller which presented an invitation's token: the token it
    // is to present from now on, which the invitation's was exchanged for.
    case Granted(token: Text)

  lazy val handshake: Channel.Codec[Handshake] =
    import Channel.derivation.throwing
    val schema: Tels = Tels.tels[Handshake](t"pyrocosm-handshake")

    Channel.Codec
      ( t"pyrocosm-handshake",
        schema,
        message => Channel.encode(message, schema),
        data => Channel.decode[Handshake](data) )

  // What a worker says about itself in `welcome`.
  case class Info(tool: Text, version: Text, identity: Machine.Identity, capabilities: List[Text])

  object Error:
    enum Reason(val number: Int) extends Clarification:
      case NoIdentity(machine: Text)           extends Reason(1)
      case NoToken(machine: Text)              extends Reason(2)
      case Unreachable(machine: Text)          extends Reason(3)
      case Refused(reason: Text)               extends Reason(4)
      case Protocol(theirs: Text, ours: Text)  extends Reason(5)
      case Disconnected                        extends Reason(6)
      case Identity                            extends Reason(7)

    given communicable: Reason is Communicable =
      case Reason.NoIdentity(machine) =>
        m"machine $machine declares no identity fingerprint to pin"

      case Reason.NoToken(machine)   => m"machine $machine declares no token"
      case Reason.Unreachable(machine) => m"machine $machine could not be connected to"
      case Reason.Refused(reason)    => m"the peer refused the connection: $reason"
      case Reason.Protocol(theirs, ours) => m"the peer speaks protocol $theirs, not $ours"
      case Reason.Disconnected       => m"the peer closed the connection during the handshake"
      case Reason.Identity           => m"this machine's identity could not be created or read"

  case class Error(reason: Error.Reason)(using Diagnostics)
  extends fulminate.Error(6040, reason.number)(m"the remote connection failed because $reason")

  // The words a worker refuses with.
  object Refusal:
    val token: Text = t"bad-token"
    val protocol: Text = t"protocol"
    val tool: Text = t"tool"
    val busy: Text = t"busy"

  def render(fingerprint: Data): Text = t"sha256:${fingerprint.serialize[Hex]}"

  // `sha256:` followed by 64 hex digits, or the digits alone, or `openssl`'s colon-separated
  // uppercase pairs.
  def parseFingerprint(text: Text): Optional[Data] =
    val stripped: Text =
      (if text.lower.starts(t"sha256:") then text.s.substring(7).nn.tt else text).lower.s.replace(":", "").nn.tt

    val hex: Boolean = stripped.s.forall { c => (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') }
    if stripped.s.length != 64 || !hex then Unset else safely(stripped.deserialize[Hex])

  // A machine's key material and the certificate it presents.
  case class Identity(keystore: Data, password: Text):
    lazy val certificate: Data = Tls.certificate(keystore, password).or(Array.empty[Byte])
    lazy val fingerprint: Data = Tls.fingerprint(certificate)
    def tls: Tls = Tls.keyed(keystore, password)

  def directory(using Environment): Optional[Path on Linux] =
    safely(Xdg.stateHome[Path on Linux] / "pyrocosm" / "remote")

  def tokenFile(using Environment): Optional[Path on Linux] =
    safely(Directories.configHome[Path on Linux] / "pyrocosm" / "token")

  private def restrict(path: Path on Linux): Unit =
    try
      jnf.Files.setPosixFilePermissions
        ( jnf.Path.of(path.encode.s), jnf.attribute.PosixFilePermissions.fromString("rw-------") )
    catch case _: Exception => ()

  private def random(bytes: Int): Text =
    val array = new scala.Array[Byte](bytes)
    js.SecureRandom().nextBytes(array)
    Array.unsafeFrozen(array).serialize[Hex]

  private def ensure(directory: Path on Linux): Unit raises Io.Error =
    if !directory.existent() then directory.create[Directory](CreateFlag.Parents)

  // This machine's identity, generated on first use: `identity.p12` and `identity.password`
  // under `directory`, both readable by the user alone.
  def identity(using Environment): Identity raises Error =
    directory.lay(abort(Error(Error.Reason.Identity))): directory =>
      val keystore: Path on Linux = unsafely(directory / "identity.p12")
      val passwordFile: Path on Linux = unsafely(directory / "identity.password")

      safely:
        if keystore.existent() && passwordFile.existent() then
          Identity(keystore.read[Data], passwordFile.read[Text].s.trim.nn.tt)
        else
          ensure(directory)
          val password: Text = random(24)
          val keytool: String = java.lang.System.getProperty("java.home").nn + "/bin/keytool"
          val hostname: Text = Machine.Identity.local.hostname

          val process =
            ProcessBuilder
              ( keytool, "-genkeypair", "-alias", "pyrocosm", "-keyalg", "EC", "-groupname",
                "secp256r1", "-dname", s"CN=$hostname", "-validity", "36500", "-storetype",
                "PKCS12", "-keystore", keystore.encode.s, "-storepass", password.s, "-keypass",
                password.s )
            . redirectErrorStream(true).nn.start().nn

          process.getInputStream.nn.readAllBytes()

          if process.waitFor() != 0 || !keystore.existent()
          then abort(Error(Error.Reason.Identity))

          passwordFile.write(password)
          restrict(keystore)
          restrict(passwordFile)
          Identity(keystore.read[Data], password)

      . or(abort(Error(Error.Reason.Identity)))

  // The token this machine's listeners expect, generated on first use.
  def token(using Environment): Optional[Text] =
    tokenFile.let: file =>
      safely:
        if file.existent() then file.read[Text].s.trim.nn.tt
        else
          file.parent.let(ensure(_))
          val secret: Text = random(32)
          file.write(secret)
          restrict(file)
          secret

  // One connection after its handshake: the tool's messages and raw payloads both ways, and
  // what the other side said about itself (a controller sees the worker's identity and
  // capabilities; a worker sees the controller's tool version and hostname).
  // `address` is which of the machine's hosts the connection was made to, and `granted` the
  // token a worker gave in exchange for an invitation's, on the side which made the connection.
  class Session[message]
    ( channel:     Channel[message],
      val peer:    Info,
      val address: Optional[Text] = Unset,
      val granted: Optional[Text] = Unset ):

    def send(message: message): Unit = channel.send(message)
    def send(message: message, raw: Data): Unit = channel.send(message, raw)
    def sendRaw(data: Data): Unit = channel.sendRaw(data)
    def receive(): Channel.Frame[message] = channel.receive()
    def close(): Unit = channel.close()

  private def refuse[message](channel: Channel[message], reason: Text): Unit =
    channel.sendHandshake(handshake.fingerprint, handshake.encode(Handshake.Refused(reason)))

  // A listener for one tool: after the handshake, each connection is lent to `handler` on its
  // own task. `gate` is asked before `welcome` and answers a refusal word to turn a connection
  // away (a worker already running a job says `busy`). `serve` blocks until `stop`.
  class Listener[message]
    ( tool:         Text,
      version:      Text,
      codec:        Channel.Codec[message],
      token:        Text,
      identity:     Identity,
      capabilities: List[Text],
      gate:         () => Optional[Text] )
    ( handler: Session[message] => Unit )
    ( using environment: Environment ):

    // Who else, beside the holder of `token`, may be let in: callers this machine invited, and
    // those it admitted by invitation before.
    private val admissions: Admissions = Admissions()

    private val stopped: Promise[Unit] = Promise()

    private def connection(duplex: Duplex): Data =
      val channel = Channel(codec, duplex)

      channel.receive() match
        case Channel.Frame.Handshake(fingerprint, document) =>
          if !Channel.same(fingerprint, handshake.fingerprint) then refuse(channel, Refusal.protocol)
          else safely(handshake.decode(document)) match
            case Handshake.Hello(theirTool, protocol, theirVersion, theirToken, theirHost) =>
              if theirTool != tool then refuse(channel, Refusal.tool)
              else if protocol != codec.protocol then refuse(channel, Refusal.protocol)
              else gate() match
                case reason: Text =>
                  refuse(channel, reason)

                case _ => admissions.admit(theirToken, token, tool, theirHost) match
                  case pyrocosm.Admission.Refused =>
                    refuse(channel, Refusal.token)

                  case admission =>
                    admission match
                      case pyrocosm.Admission.Invited(granted) =>
                        val grant = Handshake.Granted(granted)
                        channel.sendHandshake(handshake.fingerprint, handshake.encode(grant))

                      case _ =>
                        ()

                    val local = Machine.Identity.local

                    val welcome =
                      Handshake.Welcome
                        ( tool, codec.protocol, version, local.hostname, local.os, local.arch,
                          local.cores, local.jvm, capabilities )

                    channel.sendHandshake(handshake.fingerprint, handshake.encode(welcome))

                    // What the worker knows of the controller: its tool's version and hostname.
                    val controller =
                      Info(theirTool, theirVersion, Machine.Identity(theirHost, t"", t"", 0, t""), Nil)

                    handler(Session(channel, controller))

            case _ =>
              refuse(channel, Refusal.protocol)

        case _ =>
          ()

      Array.empty[Byte]

    def serve(port: Int)(using Monitor, Probate): Unit =
      given Tls = identity.tls
      val secure: SecurePort = SecurePort(Port.unsafe[Tcp](port))

      safely:
        secure.listen[Data](connection(_)):
          safely(stopped.attend())
          ()

      ()

    def stop(): Unit = stopped.offer(())

  // Connects to `machine` as `tool`, pinning the identity it declares, and lends the session
  // to `lambda` once the worker has welcomed it. A machine without an identity or a token is
  // refused here, before anything is sent: an unpinned connection would trust any certificate.
  def connect[message, result]
    ( machine: Machine, tool: Text, version: Text, codec: Channel.Codec[message], defaultPort: Int )
    ( lambda: Session[message] => result )
  :   result raises Error =

    exchange(machine, tool, version, codec, defaultPort)(lambda) match
      case scala.Right(value) => value
      case scala.Left(reason) => abort(Error(reason))

  // As `connect`, but yielding the reason the session could not be had instead of raising it:
  // the form for a caller whose own error handling cannot host a `raises`.
  def exchange[message, result]
    ( machine: Machine, tool: Text, version: Text, codec: Channel.Codec[message], defaultPort: Int )
    ( lambda: Session[message] => result )
  :   scala.Either[Error.Reason, result] =

    machine.identity match
      case fingerprint: Data =>
        machine.token.let(Machine.secret(_)) match
          case secret: Text => exchange(machine, fingerprint, secret, tool, version, codec, defaultPort)(lambda)
          case _            => scala.Left(Error.Reason.NoToken(machine.name))

      case _ =>
        scala.Left(Error.Reason.NoIdentity(machine.name))

  // ── invitations ──────────────────────────────────────────────────────────────────────────

  // An invitation to this machine for `tool`, listening on `port`, which admits one caller
  // before `lifetime` has passed. Its token is recorded, as a digest, where every listener on
  // this machine looks (`Admissions`), and is shown nowhere else but in the invitation.
  def invite(tool: Text, port: Int, lifetime: Duration)(using Environment)
  :   Invitation raises Error =

    val fingerprint: Data = identity.fingerprint
    val token: Text = Admissions.random(32)
    val expires: Instant over Unix = now() + lifetime
    val name: Text = Machine.Identity.local.hostname
    Admissions().issue(token, tool, expires)
    Invitation(name, Machine.addresses, port, fingerprint, token, expires, tool)

  // Where a joiner keeps the token it was granted by the machine of this name.
  def tokenFile(name: Text)(using Environment): Optional[Path on Linux] =
    safely(Directories.configHome[Path on Linux] / "pyrocosm" / "tokens" / name)

  // Accepts `invitation` for `tool`: connects to the machine it invites to, presents its token,
  // and lends the session to `lambda`; then records the token the machine granted in exchange,
  // and declares the machine, as `name`, in the shared `machines.tel` — every address, the port,
  // the fingerprint and the token's file — replacing any declaration of that name. What comes
  // back is the machine as declared, and what `lambda` made of the session.
  def join[message, result]
    ( invitation: Invitation, name: Text, tool: Text, version: Text, codec: Channel.Codec[message] )
    ( lambda: Session[message] => result )
    ( using Environment )
  :   (Machine, result) raises Invitation.Error raises Error =

    if invitation.tool != tool
    then abort(Invitation.Error(Invitation.Error.Reason.Tool(invitation.tool)))

    // A listener refuses an invitation's token, as any other it does not know, as `bad-token`:
    // to the bearer of an invitation, that means it has been used, or has expired there.
    val outcome: (Optional[Text], result) =
      import errorDiagnostics.emptyDiagnostics

      mitigate:
        case Error(Error.Reason.Refused(Refusal.token)) =>
          Invitation.Error(Invitation.Error.Reason.Used)

      . protect:
          connect(invitation.machine(name), tool, version, codec, invitation.port): session =>
            (session.granted, lambda(session))

    outcome(0) match
      case granted: Text =>
        val file: Path on Linux =
          tokenFile(name).or(abort(Invitation.Error(Invitation.Error.Reason.Unwritable)))

        val written: Optional[Unit] = safely:
          file.parent.let: parent =>
            if !parent.existent() then parent.create[Directory](CreateFlag.Parents)

          file.write(granted)
          restrict(file)

        if written.absent then abort(Invitation.Error(Invitation.Error.Reason.Unwritable))

        val machine: Machine =
          Machine(name, invitation.hosts, invitation.port, invitation.identity, file.encode, Nil)

        if Machine.declare(machine).absent
        then abort(Invitation.Error(Invitation.Error.Reason.Unwritable))

        (machine, outcome(1))

      case _ =>
        abort(Invitation.Error(Invitation.Error.Reason.Used))

  // The machines this one has admitted by invitation, with the tool each was admitted to.
  def peers(using Environment): List[(Text, Text)] = Admissions().peers

  // Refuses, from now on, the machine of this name, admitted by invitation to `tool`; how many
  // tokens that undid.
  def revoke(name: Text, tool: Text)(using Environment): Int = Admissions().revoke(name, tool)

  // The words of a reason, for a caller that has no `Diagnostics` to make an `Error` with.
  def explain(reason: Error.Reason): Text = reason match
    case Error.Reason.NoIdentity(machine) => t"machine $machine declares no identity fingerprint to pin"
    case Error.Reason.NoToken(machine)    => t"machine $machine declares no token"
    case Error.Reason.Unreachable(machine) => t"machine $machine could not be connected to"
    case Error.Reason.Refused(reason)     => t"the peer refused the connection: $reason"
    case Error.Reason.Protocol(theirs, ours) => t"the peer speaks protocol $theirs, not $ours"
    case Error.Reason.Disconnected        => t"the peer closed the connection during the handshake"
    case Error.Reason.Identity            => t"this machine's identity could not be created or read"

  // How long the next of a machine's addresses is waited for before it is tried alongside the
  // earlier ones, and how long, in all, any of them is waited for.
  private val stagger: Duration = 0.25*Second
  private val patience: Duration = 15.0*Second

  // Connects to the first of `hosts` to answer, completing TLS to the certificate `Tls` pins:
  // the nearest first (`Machine.ordered`), each of the others begun `stagger` after the one
  // before it, in the manner of RFC 8305, so that an address which never answers — one on a
  // network the caller is not on — delays the connection by `stagger`, not by the system's
  // connect timeout, which coaxial does not let a caller shorten. The first to complete is
  // kept; any other that completes after it is closed. A host whose certificate is not the one
  // pinned fails its TLS handshake, so a different machine at a reused address never wins.
  private def nearest(hosts: List[Text], port: Int)(using Tls): Optional[(Text, Duplex)] =
    import threading.platformThreading
    import probates.cancelProbate

    val connectable: SecureEndpoint is Connectable = summon[SecureEndpoint is Connectable]
    val ordered: List[Text] = Machine.ordered(hosts)
    val winner: Promise[(Text, Duplex)] = Promise()
    val failures: Atomic[Int] = Atomic(0)
    val count: Int = ordered.stdlib.length

    safely:
      supervise:
        ordered.stdlib.zipWithIndex.foreach: (host, index) =>
          async:
            if index > 0 then snooze(stagger*index.toDouble)

            if winner.ready then () else
              try
                val duplex: Duplex = connectable.connect(SecureEndpoint(host, port), Unset)
                winner.offer((host, duplex))
                if !winner().let(_(1) == duplex).or(false) then duplex.close()
              catch case _: Exception =>
                if failures.since(_ + 1) >= count then winner.cancel()

        winner.await(patience)

  private def exchange[message, result]
    ( machine: Machine, fingerprint: Data, secret: Text, tool: Text, version: Text,
      codec: Channel.Codec[message], defaultPort: Int )
    ( lambda: Session[message] => result )
  :   scala.Either[Error.Reason, result] =

    given Tls = TlsAcceptance().pinning(fingerprint).tls()

    // The exchange yields either the lambda's result or the reason it could not run; a socket
    // or TLS failure (the pin not matching, the host unreachable) at every address reads as
    // unreachable.
    nearest(machine.hosts, machine.portOr(defaultPort)) match
      case (address: Text, duplex: Duplex) =>
        try
          val channel = Channel(codec, duplex)

          val hello =
            Handshake.Hello(tool, codec.protocol, version, secret, Machine.Identity.local.hostname)

          channel.sendHandshake(handshake.fingerprint, handshake.encode(hello))

          // A worker sends `granted` before `welcome` to a caller which presented an
          // invitation's token; anything else first is the `welcome` or a refusal.
          def welcomed(granted: Optional[Text]): scala.Either[Error.Reason, result] =
            channel.receive() match
              case Channel.Frame.Handshake(theirs, document) =>
                if !Channel.same(theirs, handshake.fingerprint)
                then scala.Left(Error.Reason.Protocol(theirs.serialize[Hex], handshake.protocol))
                else safely(handshake.decode(document)) match
                  case Handshake.Granted(token) =>
                    welcomed(token)

                  case Handshake.Welcome(theirTool, protocol, theirVersion, hostname, os, arch, cores, jvm, capabilities) =>
                    if protocol != codec.protocol
                    then scala.Left(Error.Reason.Protocol(protocol, codec.protocol))
                    else
                      val identity = Machine.Identity(hostname, os, arch, cores, jvm)
                      val info = Info(theirTool, theirVersion, identity, capabilities)
                      scala.Right(lambda(Session(channel, info, address, granted)))

                  case Handshake.Refused(reason) => scala.Left(Error.Reason.Refused(reason))
                  case _                         => scala.Left(Error.Reason.Disconnected)

              case _ =>
                scala.Left(Error.Reason.Disconnected)

          try welcomed(Unset) catch case error: java.io.IOException =>
            scala.Left(Error.Reason.Unreachable(machine.name))

        finally duplex.close()

      case _ =>
        scala.Left(Error.Reason.Unreachable(machine.name))
