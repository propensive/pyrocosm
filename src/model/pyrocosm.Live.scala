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

import denominative.dysasymptotics.linearSize
import rudiments.*

// A reactive cell: the one place where the model is mutable. An application assigns to it from
// any thread — a benchmark reporter, a compile task, an event handler — and every frontend
// currently showing it repaints. Modelled on ultimatum's `Reading`, and subject to the same
// capture-checking seam: the wake callbacks genuinely capture a running frontend's event loop
// and escape into this longer-lived cell, which capture checking cannot yet express, so they
// are laundered with a single, localised `unsafeAssumePure`, exactly as `Reading.bindWake` is.
// It is sound for the same reasons: a frontend re-binds on every run and unbinds when it
// finishes, so a wake never references a finished loop.
class Live[value](initial: value):
  @caps.unsafe.untrackedCaptures
  private var current: value = initial

  @caps.unsafe.untrackedCaptures
  private var wakes: List[() -> Unit] = Nil

  private[pyrocosm] def bindWake(wake: () => Unit): Unit = synchronized:
    wakes = caps.unsafe.unsafeAssumePure(wake) :: wakes

  private[pyrocosm] def unbindWakes(): Unit = synchronized { wakes = Nil }

  def apply(): value = current

  // Paired with `apply`, this gives assignment syntax: `progress() = Status.Fraction(0.42)`.
  def update(value: value): Unit =
    current = caps.unsafe.unsafeAssumePure(value)
    val current0 = synchronized(wakes)
    current0.each(_())

  // Atomic: a frontend's thread and the application's may both amend.
  def amend(lambda: value => value): Unit = synchronized(update(lambda(current)))

object Live:
  extension [element](live: Live[List[element]])
    def append(element: element): Unit = live.amend(_ :+ element)
    def prepend(element: element): Unit = live.amend(element :: _)
