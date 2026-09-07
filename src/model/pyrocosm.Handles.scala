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

import java.util.concurrent.atomic as juca

import anticipation.*

// The handles an application creates to name the things a user can act upon. Each carries a
// process-unique id, so that a handle is a plain value: it serialises as its id, an event
// naming it decodes to an equal handle, and `case Event.Pressed(`run`)` matches by equality.
// The label is for the application's own diagnostics; it plays no part in identity.
object Handles:
  // A random per-process prefix keeps ids from two processes (a server and a replayed
  // recording, say) from colliding; the counter keeps them unique within one.
  private val prefix: String =
    java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(Math.random()) & 0xffffffL).nn

  private val counter: juca.AtomicLong = juca.AtomicLong(0L)

  def fresh(kind: Char): Text = s"$kind$prefix-${counter.incrementAndGet()}".tt

// Something that can be triggered: a button, a selectable row, an internal link, a shortcut.
object Action:
  def apply(label: Text = ""): Action = new Action(Handles.fresh('a'), label)

case class Action private (id: Text, label: Text)

// A text field the user can edit.
object Input:
  def apply(label: Text = ""): Input = new Input(Handles.fresh('i'), label)

case class Input private (id: Text, label: Text)

// A choice among a fixed set of options.
object Choice:
  def apply(label: Text = ""): Choice = new Choice(Handles.fresh('c'), label)

case class Choice private (id: Text, label: Text)

// A two-state switch.
object Toggle:
  def apply(label: Text = ""): Toggle = new Toggle(Handles.fresh('t'), label)

case class Toggle private (id: Text, label: Text)
