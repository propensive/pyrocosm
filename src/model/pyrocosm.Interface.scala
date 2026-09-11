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

import clavichord.*
import rudiments.*
import symbolism.*

// A keyboard shortcut the interface claims. Declared, so the web frontend intercepts only these
// chords and leaves the rest to the browser, and so a help panel can list them.
case class Shortcut(keypress: Keypress, action: Action, description: List[Inline] = Nil)

// The whole of what an application shows: its panels, its global controls, its shortcuts. A
// description, not a layout — see `Panel`.
case class Interface
  ( title:     List[Inline],
    panels:    List[Panel],
    controls:  List[Control]  = Nil,
    shortcuts: List[Shortcut] = Nil,
    hints:     Hints          = Hints.none ):

  // Every `Live` cell reachable from this interface, for a frontend to bind its wake to.
  def cells: List[Live[?]] =
    def ofControl(control: Control): List[Live[?]] = control match
      case Control.Button(_, _, enabled)          => List(enabled)
      case Control.Field(_, _, value, decoration, _, _, history) => List(value, decoration, history)
      case Control.Choice(_, _, current)          => List(current)
      case Control.Toggle(_, _, state)            => List(state)

    panels.bind { (panel: Panel) => panel.content :: panel.controls.bind(ofControl) }
    + controls.bind(ofControl)
