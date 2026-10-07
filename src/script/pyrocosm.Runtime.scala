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

import ambience.*
import anticipation.*
import turbulence.*
import vacuous.*

// What a script receives from the tool that runs it (lira's fever.md §6a): its arguments, the
// environment it was invoked in, the directory it was invoked from, and its standard streams. A
// script's entry point is `def main(using Runtime): Unit`, and everything it would otherwise
// take from ambient process state comes through this one capability — so the same script runs
// unchanged in a fresh JVM, in a tool's resident daemon (whose own streams, environment and
// directory are not the user's), or on another machine.
//
// A `Runtime` IS a `Stdio`, an `Environment` and a `WorkingDirectory` — the three have no member
// in common — exactly as exoskeleton's `Invocation` is a `Stdio`: a body with a `Runtime` in
// scope needs no other given for `Out.println`, `Environment.variable` or a relative path, and
// no import of any given at all, which is what makes a one-line script possible.
trait Runtime extends Stdio, Environment, WorkingDirectory:
  def arguments: List[Text]

object Runtime:
  def apply
    ( arguments:        List[Text],
      environment:      Environment,
      workingDirectory: WorkingDirectory,
      stdio:            Stdio )
  :   Runtime =

    Bundle(arguments, environment, workingDirectory, stdio)

  private class Bundle
    ( val arguments:    List[Text],
      environment:      Environment,
      workingDirectory: WorkingDirectory,
      stdio:            Stdio )
  extends Runtime:
    val termcap: Termcap = stdio.termcap
    val out: java.io.PrintStream = stdio.out
    val err: java.io.PrintStream = stdio.err
    val in: java.io.InputStream = stdio.in

    def variable(name: Text): Optional[Text] = environment.variable(name)
    override def entries: Optional[Map[Text, Text]] = environment.entries
    def directory(): Text = workingDirectory.directory()
