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

// The words of a command line, as every tool reads them: `words` is every argument as text,
// `operands` the ones which are not flags, and `flagged` whether a flag is present — read,
// which is what registers it for tab-completion and the manpage.
extension (arguments: List[Argument])
  def words: List[Text] = arguments.map { (argument: Argument) => argument() }
  def operands: List[Text] = words.filter(!_.starts(t"-"))

extension (flag: Flag of Unit)
  def flagged(using Cli, Interpreter): Boolean = flag().present

// The decoders and interpreters a tool's settings and flags need and Soundness does not have:
// `true`, `yes` and `on` (case-insensitively) as a Boolean; an `Int` flag read purely, with
// `Unset` for an absent or non-numeric value; and every operand of a repeatable flag
// (`--tag a --tag b`), where the `Text` topic reads only the first.
object decodables:
  given boolean: Boolean is Decodable in Text = (value: Text) =>
    value.lower == t"true" || value.lower == t"yes" || value.lower == t"on"

case class Repeated(values: List[Text])

object interpretables:
  given int: Int is Interpretable = new Interpretable:
    type Self = Int

    def interpret(arguments: List[Argument]): Optional[Int] =
      arguments.head.lay(Unset): value => safely(value().as[Int])

  given repeated: Repeated is Interpretable = new Interpretable:
    type Self = Repeated
    def interpret(arguments: List[Argument]): Optional[Repeated] = Repeated(arguments.words)

// Every command body takes `Stdio`, `Console`, `Environment` and `WorkingDirectory` as pure
// `using` parameters; `execute` provides them through an `Invocation` whose derived
// capabilities are tracked, so each arm seals them once with `unsafeAssumePure` — they
// outlive the command they drive — before calling the body. What every tool's dispatch does
// around each of its arms.
inline def ambient[result <: Termination: scala.Precise]
  ( inline body: (Stdio, Console, Environment, WorkingDirectory) ?=> result )
  ( using Cli )
  ( using result is Status.Admissible )
:   Execution to result =

  execute:
    (erased effectful: Effectful) ?=> (invocation: Invocation) ?=>
      given Stdio = scala.caps.unsafe.unsafeAssumePure(invocation.stdio)
      given Console = scala.caps.unsafe.unsafeAssumePure(summon[Console])
      given Environment = scala.caps.unsafe.unsafeAssumePure(invocation.environment)
      given WorkingDirectory = scala.caps.unsafe.unsafeAssumePure(invocation.workingDirectory)
      body
