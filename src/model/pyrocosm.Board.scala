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
import scala.caps

import soundness.*

// The board a run shows while it works: a primary panel of the run's content, following the
// newest of it, and a status panel of its progress. Fume shows a test run on one, flair a lint
// run; the shape is the same, so it lives here, and each application only decides what the
// blocks are.
//
// A frontend paints the board; the run feeds it, either by `refresh` — rebuilding both panels
// from its own model, at most ten times a second, so a chatty run cannot saturate the terminal
// — or by `append`, for a run whose content only grows. `aborted` reports that the user left the
// board before the run finished, which a run treats as a request to stop.
final class Board(val title: Text, throttle: Long = 100L):
  val content:  Live[List[Block]] = Live(Nil)
  val progress: Live[List[Block]] = Live(Nil)

  @caps.unsafe.untrackedCaptures
  @volatile
  private var painted: Long = 0L

  @caps.unsafe.untrackedCaptures
  @volatile
  private var left: Boolean = false

  def aborted: Boolean = left
  private[pyrocosm] def leave(): Unit = left = true

  // Both panels from the run's current state, unless the last repaint was too recent; `force`
  // for the final state, which must always be shown.
  def refresh(force: Boolean = false)(content: => List[Block], progress: => List[Block]): Unit =
    val now = jl.System.currentTimeMillis

    if force || now - painted >= throttle then
      painted = now
      this.content() = content
      this.progress() = progress

  // Content that only grows: the newest block joins the end, where the panel keeps its view.
  def append(block: Block): Unit = content.append(block)

  // The progress panel as one gauge.
  def gauge(status: Status, caption: Text): Unit =
    progress() = List(Block.Gauge(status, Inline.text(caption)))

  val interface: Interface =
    Interface
      ( Inline.text(title),
        List
          ( Panel(Panel.Id(t"content"), Panel.Role.Primary, Unset, content, Panel.Priority.Essential,
                hints = Hints(hints.Follow, hints.terminal.Border.None)),
            Panel(Panel.Id(t"progress"), Panel.Role.Status, Unset, progress, Panel.Priority.Essential,
                hints = Hints(hints.terminal.Border.None)) ) )
