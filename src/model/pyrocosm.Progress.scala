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

object Progress:
  // Progress as one captioned gauge, the activity of a board, which has no destination, as a
  // board is the run's own view; `state` is whatever the run says beside it ("2 failures").
  def activity(fraction: Double, caption: Text, state: List[Inline] = Nil): Activity =
    val status: Status = Status.Fraction(fraction.max(0.0).min(1.0))
    Activity(t"progress", Inline.text(caption), status, state = state)

// What a run has done against what it has to do, as the gauge of a board and the caption
// beside it. The work is counted in units of fixed weight — the phases, each weighted by the
// tool, which also captions them — and the fraction is the weight done over the weight planned;
// a count not known until a phase begins is guessed until it is, and said again when known.
// The counts are read and written under the lock, from the run's thread and the board's.
final class Progress[phase](weight: phase => Int, caption: phase => Text):
  private var totals: Map[phase, Int] = Map()
  private var done: Map[phase, Int] = Map()

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var current: Optional[(phase, Int, Int)] = Unset

  // How much of a phase there is; said again when a guess gives way to a count.
  def plan(phase: phase, total: Int): Unit = synchronized { totals = totals.define(phase, total.max(0)) }

  // One more unit of a phase done.
  def step(phase: phase, count: Int = 1): Unit = synchronized:
    val now: Int = done(phase).or(0) + count
    done = done.define(phase, now)
    current = (phase, now, totals(phase).or(now))

  // A phase done whole, whatever was planned for it.
  def finish(phase: phase): Unit =
    synchronized { done = done.define(phase, totals(phase).or(1)) }
    step(phase, 0)

  // The weight done over the weight planned.
  def fraction: Double = synchronized:
    // The weight of each phase's count, as `count` takes it from the phase and its total.
    def weighed(count: (phase, Int) => Int): Double =
      totals.to[List].fold(0.0): (sum, entry) =>
        sum + (count(entry(0), entry(1))*weight(entry(0))).toDouble

    val planned: Double = weighed((_, total) => total)
    val completed: Double = weighed((phase, total) => done(phase).or(0).min(total))

    if planned == 0.0 then 0.0 else (completed/planned).min(1.0)

  def status: Status = Status.Fraction(fraction)

  // The phase in hand, and how far through it: "Compiling the sources, 3 of 12".
  def text: Text = current.lay(t""): (phase, now, total) =>
    t"${caption(phase)}, ${now.show} of ${total.show}"

  def activity(id: Text = t"progress"): Activity = Progress.activity(fraction, text)
