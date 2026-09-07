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

import anticipation.*

// The opaque handles an application creates to name the things a user can act upon. Identity is
// by reference: two buttons carrying the same `Action` are the same command, and an event
// naming it is matched with `case Event.Pressed(`run`)`. The label is for the application's own
// diagnostics and for the web renderer's element ids; it plays no part in equality.

// Something that can be triggered: a button, a selectable row, an internal link, a shortcut.
final class Action(val label: Text = ""):
  override def toString: String = s"Action(${label.s})"

// A text field the user can edit.
final class Input(val label: Text = ""):
  override def toString: String = s"Input(${label.s})"

// A choice among a fixed set of options.
final class Choice(val label: Text = ""):
  override def toString: String = s"Choice(${label.s})"

// A two-state switch.
final class Toggle(val label: Text = ""):
  override def toString: String = s"Toggle(${label.s})"
