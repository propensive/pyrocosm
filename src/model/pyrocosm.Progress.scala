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

import scala.collection.mutable as scm

import soundness.*

// What a run has done against what it has to do, as the gauge of a board and the caption
// beside it. The work is counted in units of fixed weight — the phases, each weighted by the
// tool, which also captions them — and the fraction is the weight done over the weight planned;
// a count not known until a phase begins is guessed until it is, and said again when known.
// The counts are read and written under the lock, from the run's thread and the board's.
final class Progress[phase](weight: phase => Int, caption: phase => Text):
  private val totals: scm.HashMap[phase, Int] = scm.HashMap()
  private val done: scm.HashMap[phase, Int] = scm.HashMap()

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var current: Optional[(phase, Int, Int)] = Unset

  // How much of a phase there is; said again when a guess gives way to a count.
  def plan(phase: phase, total: Int): Unit = synchronized { totals(phase) = total.max(0) }

  // One more unit of a phase done.
  def step(phase: phase, count: Int = 1): Unit = synchronized:
    val now: Int = done.getOrElse(phase, 0) + count
    done(phase) = now
    current = (phase, now, totals.getOrElse(phase, now))

  // A phase done whole, whatever was planned for it.
  def finish(phase: phase): Unit =
    synchronized { done(phase) = totals.getOrElse(phase, 1) }
    step(phase, 0)

  // The weight done over the weight planned.
  def fraction: Double = synchronized:
    val planned: Double =
      totals.iterator.map { (phase, total) => (total*weight(phase)).toDouble }.sum

    val completed: Double =
      totals.iterator
      . map: (phase, total) => (done.getOrElse(phase, 0).min(total)*weight(phase)).toDouble
      . sum

    if planned == 0.0 then 0.0 else (completed/planned).min(1.0)

  def status: Status = Status.Fraction(fraction)

  // The phase in hand, and how far through it: "Compiling the sources, 3 of 12".
  def text: Text = current.lay(t""): (phase, now, total) =>
    t"${caption(phase)}, ${now.show} of ${total.show}"

  def activity(id: Text = t"progress"): Activity = Progress.activity(fraction, text)

object Progress:
  // Progress as one captioned gauge, the activity of a board, which has no destination, as a
  // board is the run's own view; `state` is whatever the run says beside it ("2 failures").
  def activity(fraction: Double, caption: Text, state: List[Inline] = Nil): Activity =
    Activity(t"progress", Inline.text(caption), Status.Fraction(fraction.max(0.0).min(1.0)), state = state)
