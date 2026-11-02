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

import probably.TestEvent
import stdios.fileDescriptorStdio
import termcapDefinitions.basicTermcap

// A tool's own suite, run WITHOUT fume: a `Suite` has no `main` (the host — normally fume —
// drives it through `invoke`), so this is the plain-`java` entry point `make test-plain` and
// CI use, printing one line per completed test and exiting with the suites' status (0 =
// passed, 1 = failures, 2 = a suite threw). `fume run -c <test jar>` remains the full
// experience. A tool's test main is `Harness.main(arguments, Tests)`; a suite's own `main`,
// which a fume too old to read the jar's event schema falls back to by forking
// `java -cp <classpath> <tool>.Tests <terms…>`, is `Harness.run(arguments, Tests)`.
object Harness:
  def run(arguments: Text, suites: Suite[?]*)(using Stdio): Int =
    val passes: Atomic[Int] = Atomic(0)
    val failures: Atomic[Int] = Atomic(0)

    val statuses: List[Int] = List(suites*).map: suite =>
      suite.invoke(arguments, event => event match
        case TestEvent.TestCompleted(test, _, _, outcome, _, _) =>
          val passed: Boolean = outcome.outcome == t"pass" || outcome.outcome == t"aspire-pass"
          if passed then passes.since(_ + 1) else failures.since(_ + 1)

          Out.println(t"[${outcome.outcome}] ${test.path.join(t" / ")}")

        case TestEvent.DetailMessage(_, message) =>
          Out.println(t"    $message")

        case TestEvent.DetailCompare(_, expected, found, _) =>
          Out.println(t"    expected: $expected")
          Out.println(t"    found:    $found")

        case TestEvent.RunTerminated(error, _, _) =>
          Out.println(t"suite threw: ${error.components.map(_.message).join(t"; ")}")

        case _ => ())

    Out.println(t"${passes()} passed, ${failures()} failed")
    statuses.fold(0)(_.max(_))

  def main(arguments: scala.Array[String], suites: Suite[?]*): Unit =
    val words: Text =
      List.from(scala.collection.immutable.ArraySeq.unsafeWrapArray(arguments).map(_.tt)).join(t" ")

    Exit(run(words, suites*)).terminate()
