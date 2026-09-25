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
import charDecoders.utf8Decoder
import charEncoders.utf8Encoder
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
//   worker → controller   welcome  tool, protocol, version, the worker's identity and capabilities
//                    or   refused  a reason
//
// The `Refused` reasons are fixed words so a tool can render them; the tool's own protocol runs
// from the first frame after `welcome`.
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
  class Session[message](channel: Channel[message], val peer: Info):
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
    ( handler: Session[message] => Unit ):

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
              else if theirToken != token then refuse(channel, Refusal.token)
              else gate() match
                case reason: Text => refuse(channel, reason)
                case _ =>
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

    val fingerprint: Data = machine.identity.or(abort(Error(Error.Reason.NoIdentity(machine.name))))
    val secret: Text = machine.token.let(Machine.secret(_)).or(abort(Error(Error.Reason.NoToken(machine.name))))

    given Tls = TlsAcceptance().pinning(fingerprint).tls()
    val endpoint = SecureEndpoint(machine.host, machine.portOr(defaultPort))

    // The exchange yields either the lambda's result or the reason it could not run; a socket
    // or TLS failure (the pin not matching, the host unreachable) is an `IOException` from the
    // connection, and reads as unreachable.
    // The endpoint's `Connectable` is a tracked capability (it carries the `Online` evidence),
    // which the pure `duplex` extension cannot take, so the connection is made through it
    // directly and closed here.
    val connectable: SecureEndpoint is Connectable = summon[SecureEndpoint is Connectable]

    val exchange: scala.Either[Error.Reason, result] =
      try
        val duplex: Duplex = connectable.connect(endpoint, Unset)

        try
          val channel = Channel(codec, duplex)

          val hello =
            Handshake.Hello(tool, codec.protocol, version, secret, Machine.Identity.local.hostname)

          channel.sendHandshake(handshake.fingerprint, handshake.encode(hello))

          channel.receive() match
            case Channel.Frame.Handshake(theirs, document) =>
              if !Channel.same(theirs, handshake.fingerprint)
              then scala.Left(Error.Reason.Protocol(theirs.serialize[Hex], handshake.protocol))
              else safely(handshake.decode(document)) match
                case Handshake.Welcome(theirTool, protocol, theirVersion, hostname, os, arch, cores, jvm, capabilities) =>
                  if protocol != codec.protocol
                  then scala.Left(Error.Reason.Protocol(protocol, codec.protocol))
                  else
                    val identity = Machine.Identity(hostname, os, arch, cores, jvm)
                    val info = Info(theirTool, theirVersion, identity, capabilities)
                    scala.Right(lambda(Session(channel, info)))

                case Handshake.Refused(reason) => scala.Left(Error.Reason.Refused(reason))
                case _                         => scala.Left(Error.Reason.Disconnected)

            case _ =>
              scala.Left(Error.Reason.Disconnected)

        finally duplex.close()

      catch case error: java.io.IOException => scala.Left(Error.Reason.Unreachable(machine.name))

    exchange match
      case scala.Right(value) => value
      case scala.Left(reason) => abort(Error(reason))
