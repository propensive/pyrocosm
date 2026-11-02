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

// Waiting on a terminal until the user says stop: `until` runs `action` every quarter of a
// second until Ctrl+C — the signal, or, on a terminal, the byte the launcher forwards for it,
// or Ctrl+D — or until `finished` says so. What `<tool> serve`, `<tool> listen`, a log
// follower and a presentation all do, the same way.
object Attend:
  def until(finished: => Boolean)(action: => Unit)
    ( using cli: Cli, resident: Resident, stdio: Stdio, monitor: Monitor )
  :   Unit =

    val tty: Boolean = resident.cliInput == ethereal.Terminus.Terminal
    val aborted: Atomic[Boolean] = Atomic(false)

    trap:
      case profanity.Signal(Interrupt.Int, _, _, _) =>
        aborted() = true
        SignalResponse.Accept

    var running: Boolean = true

    while running do
      if tty then while stdio.in.available() > 0 do
        val byte: Int = stdio.in.read()
        if byte == 3 || byte == 4 then aborted() = true

      action

      if !aborted() && !finished then snooze(0.25*Second) else running = false

  // Waits until Ctrl+C alone.
  def interrupt()(using Cli, Resident, Stdio, Monitor): Unit = until(false)(())
