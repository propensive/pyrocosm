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

import scala.caps

import soundness.*

// The board a run shows while it works: a primary panel of the run's content, following the
// newest of it, and its progress as the interface's one activity. Fume shows a test run on
// one, flare a lint run; the shape is the same, so it lives here, and each application only
// decides what the blocks are.
//
// A frontend paints the board; the run feeds it, either by `refresh` — rebuilding the content
// and the progress from its own model, at most ten times a second, so a chatty run cannot
// saturate the terminal — or by `append`, for a run whose content only grows. `aborted`
// reports that the user left the board before the run finished, which a run treats as a
// request to stop.
final class Board(val title: Text, throttle: Long = 100L):
  val content: Live[List[Block]] = Live(Nil)

  // The content again, for a web view alone — with the charts a browser can draw, say — which
  // is not a panel of this interface, so a terminal never draws it.
  val web: Live[List[Block]] = Live(Nil)

  @caps.unsafe.untrackedCaptures
  private val dirty: Atomic[Boolean] = Atomic(true)

  @caps.unsafe.untrackedCaptures
  @volatile
  private var painted: Long = 0L

  @caps.unsafe.untrackedCaptures
  @volatile
  private var left: Boolean = false

  def aborted: Boolean = left
  private[pyrocosm] def leave(): Unit = left = true

  // The content and the progress from the run's current state, unless the last repaint was too
  // recent; `force` for the final state, which must always be shown.
  def refresh(force: Boolean = false)(content: => List[Block], progress: => List[Activity]): Unit =
    val now: Long = aviation.now().long

    if force || now - painted >= throttle then
      painted = now
      this.content() = content
      interface.activities() = progress

  // Content that only grows: the newest block joins the end, where the panel keeps its view.
  def append(block: Block): Unit = content.append(block)

  // The other way to feed a board: events only MARK it, and `repaint`, called by a task of its
  // own, rebuilds the content and the progress when something has changed since the last
  // paint, or when forced — so the thread consuming a run's events never renders, and a burst
  // of events costs one repaint.
  def mark(): Unit = dirty() = true

  def repaint(force: Boolean = false)(paint: => (List[Block], List[Activity])): Unit =
    if dirty.swap(false) || force then
      val (content, progress) = paint
      this.content() = content
      interface.activities() = progress

  // The board's progress, for a card of it elsewhere.
  def activity: Optional[Activity] = interface.activities().head

  // The progress as one captioned gauge: the board's activity, which has no destination, as
  // the board is the run's own view.
  def gauge(status: Status, caption: Text): Unit =
    interface.activities() = List(Activity(t"progress", Inline.text(caption), status))

  // Fullscreen on the terminal, as a board of one panel would otherwise be inline, with no band
  // for its activity.
  val interface: Interface =
    Interface
      ( Inline.text(title),
        List
          ( Panel
              ( Panel.Id(t"content"),
                Panel.Role.Primary,
                Unset,
                content,
                Panel.Priority.Essential,
                hints = Hints(hints.Follow, hints.terminal.Border.None) ) ),
        hints = Hints(hints.terminal.Occupancy.Fullscreen) )
