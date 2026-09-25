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

import charDecoders.utf8Decoder
import charEncoders.utf8Encoder
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
//     host      build.example.org
//     port      8091
//     identity  sha256:3f9a…          # the machine's certificate fingerprint (`<tool> identity`)
//     token     ~/.config/pyrocosm/tokens/linux-box
//     capability linux x86-64 quiet
//
// `identity` is the SHA-256 fingerprint of the self-signed certificate the machine's listener
// presents (see `Peer`): the SSH known-hosts model, with the fingerprint exchanged out of band.
// `token` names a file whose trimmed content is the shared secret the machine's listener
// expects; a value that names no existing file is taken as the secret itself. `port` defaults
// per tool. `capability` words describe the machine for routing; they are not verified.
case class Machine
  ( name:         Text,
    host:         Text,
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

  // The `machine` blocks of a TEL document, in order; a block without a `host` is skipped.
  def parse(document: Tel): List[Machine] =
    document.fields(t"machine").to[List].bind: block =>
      val name: Text = block.primaryAtom
      val host: Optional[Text] = block.field(t"host").let(_.primaryAtom).let(nonEmpty)

      if name == t"" || host.absent then Nil else
        val port: Optional[Int] =
          block.field(t"port").let(_.primaryAtom).let { text => safely(text.as[Int]) }

        val identity: Optional[Data] =
          block.field(t"identity").let(_.primaryAtom).let(Peer.parseFingerprint(_))

        val token: Optional[Text] = block.field(t"token").let(_.primaryAtom).let(nonEmpty)

        val capabilities: List[Text] =
          block.fields(t"capability").to[List].bind(_.atomTexts.to[List]).filter(_ != t"")

        List(Machine(name, host.or(t""), port, identity, token, capabilities))

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
