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
import vacuous.*

// A panel is a region of the interface with a purpose. The interface level says what each panel
// is *for*, how important it is, and how it relates to the others; where it goes is the
// renderer's decision, made for the space it has.
object Panel:
  final class Id(val label: Text = ""):
    override def toString: String = s"Panel.Id(${label.s})"

  // What the panel is for. Each renderer has an idiom for each role: a verso sidebar or a menu
  // strip for `Navigation`, a masthead or a one-row bar for `Status`, a docked editor for
  // `Prompt`, a following scroller or card for `Log`.
  enum Role:
    case Primary, Navigation, Detail, Inspector, Status, Log, Prompt

  // What gives way first when space is short. The terminal drops `Peripheral` panels, then
  // `Important` ones; the web collapses them into disclosure or a drawer instead.
  enum Priority:
    case Essential, Important, Peripheral

  // A relationship to another panel: a detail view of it, or a member of a group that the
  // renderer shows as tabs, an accordion, or side by side, as space allows.
  enum Relation:
    case DetailOf(panel: Id)
    case Grouped(group: Text)

case class Panel
  ( id:       Panel.Id,
    role:     Panel.Role,
    title:    Optional[List[Inline]],
    content:  Live[List[Block]],
    priority: Panel.Priority        = Panel.Priority.Important,
    relation: Optional[Panel.Relation] = Unset,
    controls: List[Control]         = Nil,
    hints:    Hints                 = Hints.none )
