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

import java.net as jn

import soundness.*

import charsets.utf8Charset
import codepages.utf8Codepage
import filesystemBackends.javaBaseFilesystem
import logging.silentLogging
import systems.javaBaseSystem
import textSanitizers.skipSanitizer

// A machine another Pyrocosm tool can be reached on: how to connect to it, how to recognise
// it, and what it offers. Declared in TEL, once for every tool, in the user's shared
// `$XDG_CONFIG_HOME/pyrocosm/machines.tel`, or in a tool's own configuration (its repository's
// `.pyrocosm/<tool>/config.tel` or the user's `<tool>/config.tel`), a tool's declaration
// overriding the shared one of the same name:
//
//   machine linux-box
//     host      192.168.1.20
//     host      build.example.org
//     port      8091
//     identity  sha256:3f9a…          # the machine's certificate fingerprint (`<tool> identity`)
//     token     ~/.config/pyrocosm/tokens/linux-box
//     capability linux x86-64 quiet
//
// `host` may be given more than once, for a machine reachable at several addresses — on a local
// network and beyond it, say. A connection tries them all, local addresses first (`ordered`).
// `identity` is the SHA-256 fingerprint of the self-signed certificate the machine's listener
// presents (see `Peer`): the SSH known-hosts model, with the fingerprint exchanged out of band.
// `token` names a file whose trimmed content is the shared secret the machine's listener
// expects; a value that names no existing file is taken as the secret itself. `port` defaults
// per tool. `capability` words describe the machine for routing; they are not verified.
case class Machine
  ( name:         Text,
    hosts:        List[Text],
    port:         Optional[Int],
    identity:     Optional[Data],
    token:        Optional[Text],
    capabilities: List[Text] ):

  def portOr(default: Int): Int = port.or(default)

object Machine:
  // The machine this process runs on, as it describes itself to a peer: the hostname the
  // kernel reports, the JVM's view of the operating system and architecture, and its cores.
  case class Identity(hostname: Text, os: Text, arch: Text, cores: Int, jvm: Text)

  object Identity:
    def local: Identity =
      val hostname: Text =
        try jn.InetAddress.getLocalHost.nn.getHostName.nn.tt
        catch case _: jn.UnknownHostException => t"localhost"

      def property(name: String): Text =
        Optional(java.lang.System.getProperty(name)).let(_.tt).or(t"unknown")

      Identity
        ( hostname,
          property("os.name"),
          property("os.arch"),
          Runtime.getRuntime.nn.availableProcessors,
          property("java.version") )

  // The shared declarations file, whether or not it exists; `Unset` if the environment names
  // no configuration home.
  def sharedFile(using Environment): Optional[Path on Linux] =
    safely(Directories.configHome[Path on Linux] / "pyrocosm" / "machines.tel")

  // The shared declarations, parsed afresh: the file is small, and a tool's own configuration
  // (cached by `Tool`) comes first anyway. `Unset` when absent or unreadable.
  def shared(using Environment): Optional[Tel] =
    sharedFile.let: file =>
      if !file.existent() then Unset
      else safely(file.read[Data]).let { data => safely(data.read[Tel]) }

  // How near an address is likely to be, for trying the nearest first: an address on a private
  // network (RFC 1918 IPv4, IPv6 unique-local) or a name resolved by multicast DNS (`.local`)
  // is tried before another name or an address shared by carrier-grade NAT (RFC 6598, which
  // overlay networks such as Tailscale use), and those before a public address, which is
  // likeliest to be the one that does not answer from inside the same network.
  def nearness(host: Text): Int =
    val text: Text = host.lower

    def octets: List[Int] = text.cut(t".").map: part => safely(part.as[Int]).or(-1)

    val ipv4: Boolean = text.s.matches("[0-9]+(\\.[0-9]+){3}")
    val ipv6: Boolean = text.contains(t":")

    def within(value: Int, low: Int, high: Int): Boolean = value >= low && value <= high

    if ipv4 then octets match
      case 10 :: _                                       => 0
      case 192 :: 168 :: _                               => 0
      case 172 :: second :: _ if within(second, 16, 31)  => 0
      case 169 :: 254 :: _                               => 0
      case 100 :: second :: _ if within(second, 64, 127) => 1
      case _                                             => 2
    else if ipv6 then
      if text.starts(t"fc") || text.starts(t"fd") then 0 else 2
    else if text.ends(t".local") then
      0
    else
      1

  // `hosts` in the order a connection tries them: the nearest first, and otherwise as declared.
  def ordered(hosts: List[Text]): List[Text] =
    hosts.stdlib.zipWithIndex.sortBy { (host, index) => (nearness(host), index) }.map(_(0))
    . to(List)

  // The addresses this machine can be reached at from another, for an invitation to list: the
  // IPv4 addresses of every interface that is up, but not loopback, and its IPv6 unique-local
  // addresses. Not its global IPv6 addresses, which a host may have many of, temporary ones
  // (RFC 8981) that will have changed before the invitation is used; nor IPv6 link-local ones,
  // meaningless without the interface they are scoped to. Nearest first, then the machine's
  // hostname, by which any other address is still found.
  def addresses: List[Text] =
    val interfaces: List[NetworkInterface] = safely(NetworkInterface.all()).or(Nil)

    def usable(interface: NetworkInterface): List[Text] =
      val ipv6: List[Text] = interface.ipv6.map(_.show)
      interface.ipv4.map(_.show) + ipv6.filter(nearness(_) == 0)

    val found: List[Text] =
      interfaces.filter { interface => interface.up && !interface.loopback }.bind(usable(_))

    ordered(found.stdlib.distinct.to(List)) + List(Identity.local.hostname)

  // A machine's declaration, as a `machine` block.
  def block(machine: Machine): Text =
    def line(keyword: Text, value: Text): Text =
      t"  $keyword${t" "*(10 - keyword.length)}$value\n"

    val hosts: List[Text] = machine.hosts.map(line(t"host", _))
    val port: List[Text] = machine.port.lay(Nil): port => List(line(t"port", port.show))
    val token: List[Text] = machine.token.lay(Nil): token => List(line(t"token", token))

    val identity: List[Text] = machine.identity.lay(Nil): data =>
      List(line(t"identity", Peer.render(data)))

    (t"machine ${machine.name}\n" :: hosts + port + identity + token).join

  // Declares `machine` in the shared file, in place of any declaration of the same name, keeping
  // everything else in the file as it was. `Unset` if the file could not be written.
  def declare(machine: Machine)(using Environment): Optional[Unit] =
    sharedFile.let: file =>
      safely:
        val existing: List[Text] =
          if file.existent() then file.read[Text].cut(t"\n") else List(t"tel 1.0", t"")

        // A block is its `machine` line and the indented lines after it.
        def recur(lines: List[Text], skipping: Boolean, kept: List[Text]): List[Text] =
          lines match
            case line :: rest =>
              if line.starts(t"machine ") then
                val other: Boolean = line.skip(8).trim != machine.name
                recur(rest, !other, if other then line :: kept else kept)
              else if skipping && (line.starts(t" ") || line == t"") then
                recur(rest, true, kept)
              else
                recur(rest, false, line :: kept)

            case _ =>
              kept.reverse

        val kept: List[Text] = recur(existing, false, Nil)
        val body: Text = kept.join(t"\n").trim

        file.parent.let: parent =>
          if !parent.existent() then parent.create[Directory](CreateFlag.Parents)

        file.write(t"$body\n\n${block(machine)}")

  // The `machine` blocks of a TEL document, in order; a block without a `host` is skipped.
  def parse(document: Tel): List[Machine] =
    document.fields(t"machine").to[List].bind: block =>
      val name: Text = block.primaryAtom

      val hosts: List[Text] =
        block.fields(t"host").to[List].map(_.primaryAtom).filter(_ != t"")

      if name == t"" || hosts.nil then Nil else
        val port: Optional[Int] =
          block.field(t"port").let(_.primaryAtom).let { text => safely(text.as[Int]) }

        val identity: Optional[Data] =
          block.field(t"identity").let(_.primaryAtom).let(Peer.parseFingerprint(_))

        val token: Optional[Text] = block.field(t"token").let(_.primaryAtom).let(nonEmpty)

        val capabilities: List[Text] =
          block.fields(t"capability").to[List].bind(_.atomTexts.to[List]).filter(_ != t"")

        List(Machine(name, hosts, port, identity, token, capabilities))

  private def nonEmpty(text: Text): Optional[Text] = if text == t"" then Unset else text

  // The machines declared by `documents`, earlier documents taking priority over later ones
  // for a name declared more than once: a tool passes its repository's, then the user's, then
  // the shared file's, in that order.
  def resolve(documents: List[Optional[Tel]]): List[Machine] =
    def recur(remaining: List[Machine], acc: List[Machine]): List[Machine] = remaining match
      case machine :: tail =>
        if acc.exists(_.name == machine.name) then recur(tail, acc)
        else recur(tail, acc + List(machine))

      case _ =>
        acc

    recur(documents.bind(_.let(parse(_)).or(Nil)), Nil)

  // The shared secret a `token` value names: the trimmed content of the file it names, when
  // one exists, or the value itself.
  def secret(token: Text): Text =
    val expanded: Text =
      if token.starts(t"~/")
      then Optional(java.lang.System.getProperty("user.home")).lay(token) { home => t"$home${token.s.substring(1).nn}" }
      else token

    safely(expanded.as[Path on Linux]).let: path =>
      if path.existent() then safely(path.read[Text]).let(_.s.trim.nn.tt) else Unset
    . or(token)
