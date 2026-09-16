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
       (work: => result)
       ( using Console, Monitor, Probate, Environment, Every[Terminal.Feature],
               Tactic[Terminal.Error], Text is Measurable, Hyphenation, TableStyle, Gauging,
               Gaugeable.Glyphs, InlineAnchoring, InlineGrowth, InlineShrink )
    :   result =

      val frontend: TerminalFrontend = caps.unsafe.unsafeAssumePure(TerminalFrontend(occupancy, theme))
      val finished: juc.atomic.AtomicBoolean = juc.atomic.AtomicBoolean(false)
      val closed: juc.CountDownLatch = juc.CountDownLatch(1)

      val painter = Thread(new Runnable:
        def run(): Unit =
          try
            Thread.sleep(grace)
            if !finished.get then
              frontend.run(board.interface):
                case Event.Closed => if !finished.get then board.leave()
                case _            => ()
          catch case _: Throwable => ()
          finally closed.countDown())

      painter.setDaemon(true)
      painter.start()

      try work
      finally
        finished.set(true)
        frontend.stop()
        closed.await(5, juc.TimeUnit.SECONDS)
