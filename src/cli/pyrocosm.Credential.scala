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

import charsets.utf8Charset
import filesystemBackends.javaBaseFilesystem
import logging.silentLogging
import systems.javaBaseSystem
import textSanitizers.skipSanitizer

object Credential:
  enum Source:
    case Env(name: Text)
    case File(path: Text)
    case Command(arguments: List[Text])

  val defaults: List[Credential] =
    List
      ( Credential(t"anthropic", List(Source.Env(t"ANTHROPIC_API_KEY"))),
        Credential(t"openai", List(Source.Env(t"OPENAI_API_KEY"))),
        Credential(t"gemini", List(Source.Env(t"GEMINI_API_KEY"))),
        Credential(t"github", List(Source.Env(t"GITHUB_TOKEN"))) )

  // The shared declarations file, whether or not it exists; `Unset` if the environment names
  // no configuration home.
  def sharedFile(using Environment): Optional[Path on Linux] =
    safely(Directories.configHome[Path on Linux] / "pyrocosm" / "credentials.tel")

  // The shared declarations, parsed afresh: the file is small, and a tool's own configuration
  // (cached by `Tool`) comes first anyway. `Unset` when absent or unreadable.
  def shared(using Environment): Optional[Tel] =
    sharedFile.let: file =>
      if !file.existent() then Unset
      else safely(file.read[Data]).let: data => safely(data.read[Tel])

  // The `credential` blocks of a TEL document, in order, each with its sources in the order
  // written; a block without a name, or a source line without a value, is skipped.
  def parse(document: Tel): List[Credential] =
    document.fields(t"credential").to[List].bind: block =>
      val name: Text = block.primaryAtom

      // Each child compound, rewrapped as a `Tel` node (childless: only its keyword and atoms
      // are read) so that its atoms come back as texts in presentation order.
      val sources: List[Source] = block.childCompounds.to[List].bind: compound =>
        val child: Tel = Tel.compound(compound.keyword, compound.atoms, Array.empty)
        val atoms: List[Text] = child.atomTexts.to[List].filter(_ != t"")

        child.keyword match
          case t"env"     => atoms.prim.let(Source.Env(_)).let(List(_)).or(Nil)
          case t"file"    => atoms.prim.let(Source.File(_)).let(List(_)).or(Nil)
          case t"command" => if atoms.nil then Nil else List(Source.Command(atoms))
          case _          => Nil

      if name == t"" then Nil else List(Credential(name, sources))

  // The credentials declared by `documents`, earlier documents taking priority over later ones
  // for a name declared more than once: a tool passes its repository's, then the user's, then
  // the shared file's, in that order.
  def resolve(documents: List[Optional[Tel]]): List[Credential] =
    def recur(remaining: List[Credential], acc: List[Credential]): List[Credential] =
      remaining match
        case credential :: tail =>
          if acc.exists(_.name == credential.name) then recur(tail, acc)
          else recur(tail, acc + List(credential))

        case _ =>
          acc

    recur(documents.bind(_.let(parse(_)).or(Nil)), Nil)

  // One source's value, trimmed, or `Unset` when it has none: an unset or empty variable, a
  // missing or empty file, a helper that fails or prints nothing.
  def obtain(source: Source)(using environment: Environment, working: WorkingDirectory)
  :   Optional[Text] =

    source match
      case Source.Env(name) =>
        environment.variable(name).let(nonEmpty)

      case Source.File(path) =>
        safely(expand(path).as[Path on Linux]).let: file =>
          if file.existent() then safely(file.read[Text]).let(trimmed) else Unset

      case Source.Command(arguments) =>
        safely(Command(arguments*).exec[Text]()).let(trimmed)

  // `~/` names the invoking user's home, as a shell would expand it.
  private def expand(path: Text): Text =
    if path.starts(t"~/")
    then
      Optional(java.lang.System.getProperty("user.home")).lay(path): home =>
        t"$home${path.s.substring(1).nn}"
    else
      path

  private def trimmed(text: Text): Optional[Text] = nonEmpty(text.s.trim.nn.tt)
  private def nonEmpty(text: Text): Optional[Text] = if text == t"" then Unset else text

// A secret a tool needs but must not own: an API key, a token. A credential names *where to
// ask*, never the value, in the convention of git's credential helpers and Docker's credential
// stores, so that no secret is ever written into a configuration file that is committed or
// shared. Declared in TEL, once for every Pyrocosm tool, in the user's shared
// `$XDG_CONFIG_HOME/pyrocosm/credentials.tel`, or in a tool's own configuration (its
// repository's `.pyrocosm/<tool>/config.tel`, which should only ever *reference* a credential,
// or the user's `<tool>/config.tel`), a tool's declaration overriding the shared one of the same
// name:
//
//   credential anthropic
//     env      ANTHROPIC_API_KEY                            # an environment variable
//     file     ~/.config/anthropic/api-key                  # a file's trimmed content
//     command  op read op://Private/Anthropic/credential    # a helper program's trimmed output
//
// The sources are tried in the order written, and the first that yields a non-empty value is the
// credential. A `command` covers every secret manager with a command-line client — 1Password's
// `op`, `pass`, `gopass`, the macOS keychain's `security find-generic-password -w`, freedesktop's
// `secret-tool` — without Pyrocosm knowing any of them. Well-known names have a default when
// nothing declares them: `anthropic` reads `ANTHROPIC_API_KEY`, `openai` `OPENAI_API_KEY`,
// `gemini` `GEMINI_API_KEY` and `github` `GITHUB_TOKEN`.
case class Credential(name: Text, sources: List[Credential.Source]):
  // The first source that yields a value, tried in order; `Unset` when none does. The value is
  // returned and nothing else: never logged, never echoed.
  def obtain()(using Environment, WorkingDirectory): Optional[Text] =
    def recur(remaining: List[Credential.Source]): Optional[Text] = remaining match
      case source :: tail => Credential.obtain(source).or(recur(tail))
      case _              => Unset

    recur(sources)
