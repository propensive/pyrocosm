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

import java.lang as jl
import java.util.concurrent.atomic as juca

import soundness.*

import probably.TestEvent

// Pyrocosm's suites, run WITHOUT fume: a `Suite` has no `main` of its own (the host, normally
// fume, drives it through `invoke`), so this is the plain-`java` entry point CI uses while no
// fume release reads the test events of the Soundness Pyrocosm is built against. It prints one
// line per completed test and exits with the worst status of the suites (0 = passed,
// 1 = failures, 2 = a suite threw). `make test`, through fume, remains the full experience.
@main
def runTests(): Unit =
  val passes = juca.AtomicInteger(0)
  val failures = juca.AtomicInteger(0)
  val out = jl.System.out.nn

  def handle(event: TestEvent): Unit = event match
    case TestEvent.TestCompleted(test, _, _, outcome, _, _) =>
      if outcome.outcome == t"pass" || outcome.outcome == t"aspire-pass"
      then passes.incrementAndGet()
      else failures.incrementAndGet()

      out.println(t"[${outcome.outcome}] ${test.path.join(t" / ")}".s)

    case TestEvent.DetailMessage(_, message) =>
      out.println(t"    $message".s)

    case TestEvent.DetailCompare(_, expected, found, _) =>
      out.println(t"    expected: $expected".s)
      out.println(t"    found:    $found".s)

    case TestEvent.RunTerminated(error, _, _) =>
      out.println(t"suite threw: ${error.components.map(_.message).join(t"; ")}".s)

    case _ => ()

  val suites = scala.List(Tests, CompilerTests, ServicesTests, SwarmTests)
  val status = suites.map(_.invoke(t"", handle)).max

  out.println(t"${passes.get} passed, ${failures.get} failed".s)
  jl.System.exit(status)
