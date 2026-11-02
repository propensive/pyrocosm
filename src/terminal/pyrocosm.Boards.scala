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

import java.util.concurrent as juc

import scala.caps

import soundness.*
import gigantism.Every
import pyrocosm.Board

// Showing a board in the terminal while a run works: `board.show(grace)(work)` runs `work` on
// the calling thread while a `TerminalFrontend` paints the board on another. The frontend opens
// only if the work is still running after `grace` milliseconds, so a run that finishes at once
// never flashes the alternate screen; and it is closed, and the terminal restored, before
// `show` returns, so whatever the caller prints next — the report — lands on the ordinary
// screen. Leaving the board (Escape, Ctrl+C, Ctrl+D) before the work is done marks the board
// `aborted`, for the work to notice.
object Boards:
  extension (board: Board)
    def show[result]
      ( grace:     Long                = 1000L,
        occupancy: Optional[Occupancy] = Unset,
        theme:     TerminalTheme       = TerminalTheme.default )
      ( work: => result )
      ( using Console, Monitor, Probate, Environment, Every[Terminal.Feature],
              Tactic[Terminal.Error], Text is Measurable, Hyphenation, TableStyle, Gauging,
              Gaugeable.Glyphs, InlineAnchoring, InlineGrowth, InlineShrink )
    :   result =

      val frontend: TerminalFrontend =
        caps.unsafe.unsafeAssumePure(TerminalFrontend(occupancy, theme))

      val finished: Atomic[Boolean] = Atomic(false)
      val closed: Promise[Unit] = Promise()

      // The painter is a task of the monitor in scope: it waits out the grace, and opens the
      // frontend only if the work is still running after it.
      async:
        try
          snooze((grace/1000.0)*Second)

          if !finished() then
            frontend.run(board.interface):
              case Event.Closed => if !finished() then board.leave()
              case _            => ()
        catch case _: Throwable => ()
        finally closed.offer(())

      try work
      finally
        finished() = true
        frontend.stop()
        safely(closed.await(5*Second))

  // Where a run's problems go: to the board as each is found, where there is one, or straight
  // to the terminal, so that it is seen at once either way; and every problem is kept, for the
  // report once the board has closed, unless it was already printed as it came.
  def reporter(board: Optional[Board], width: Int, theme: TerminalTheme = TerminalTheme.default)
    ( using Stdio, Text is Measurable, Hyphenation, TableStyle, Gauging, Gaugeable.Glyphs )
  :   Reporter =

    Reporter(board, width, TerminalRenderer(theme))

  final class Reporter(board: Optional[Board], width: Int, renderer: TerminalRenderer)(using Stdio):
    private val found: juc.ConcurrentLinkedQueue[Block] = juc.ConcurrentLinkedQueue()

    def report(block: Block): Unit =
      found.add(block)
      def print(): Unit = renderer.print(List(block), width)
      board.lay(print()): (board: Board) => board.append(block)

    def reports(blocks: List[Block]): Unit = blocks.each(report(_))

    // Every problem found, in order; and whether they were already printed as they came.
    def problems: List[Block] = List.from(found.iterator.nn.asScala)
    def streamed: Boolean = board.absent
