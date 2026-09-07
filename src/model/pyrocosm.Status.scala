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

import aviation.*
import quantitative.*
import vacuous.*

// How one unit of work has turned out, or has not yet. The same six-way vocabulary as
// ultimatum's `Standing`, defined here so that the model does not depend on the terminal.
enum Standing:
  case Pending, Running, Succeeded, Failed, Warned, Skipped

// One named stage of a multi-stage process.
case class Step(name: List[Inline], standing: Standing, detail: Optional[List[Inline]] = Unset)

// What a gauge displays. Each case corresponds to a family of ultimatum gauge designs on the
// terminal and to a `<progress>`, `<meter>`, a step list or a timer on the web.
object Status:
  object Elapsed:
    def of(duration: Duration): Elapsed = Elapsed(duration.value)

  object Remaining:
    def of(duration: Duration): Remaining = Remaining(duration.value)

enum Status:
  case Fraction(value: Double)                          // a proportion in [0, 1]
  case Indeterminate()                                  // in progress, extent unknown: a spinner
  case Reckoning(done: Long, total: Optional[Long])     // 17/120, or 17 of an unknown total
  case Standing(standing: pyrocosm.Standing)
  case Elapsed(seconds: Double)
  case Remaining(seconds: Double)
  case Steps(steps: List[Step])
