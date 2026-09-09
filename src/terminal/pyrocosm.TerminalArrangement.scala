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
import denominative.*
import escapade.*
import gossamer.*
import rudiments.*
import symbolism.*
import denominative.dysasymptotics.{linearAccess, linearSize}
import vacuous.*

import ultimatum.{border, strip, stack, BorderStyle, Pane, Sizing}

// The terminal's arrangement solver: from the interface's panels, their roles and priorities,
// and the terminal's size, to an Ultimatum pane tree. A rule table rather than a constraint
// solver, so that it can be read, tested and revised.
object TerminalArrangement:
  // Which panels are shown where. `rectoBeside` says whether the detail panels sit to the right
  // of the centre or below it.
  case class Plan
    ( verso:       List[Panel],
      centre:      List[Panel],
      recto:       List[Panel],
      log:         List[Panel],
      prompt:      List[Panel],
      status:      List[Panel],
      dropped:     List[Panel],
      rectoBeside: Boolean )

  def plan(interface: Interface, columns: Int, rows: Int): Plan =
    // What fits: an essential panel always; an important one unless the terminal is very
    // narrow; a peripheral one only when there is room to spare. A `Minimum` hint below the
    // terminal's size drops any but an essential panel.
    def fits(panel: Panel): Boolean =
      val minimum = panel.hints[hints.Minimum]
      val wide = minimum.lay(true) { hint => hint.columns <= columns && hint.rows <= rows }

      panel.priority match
        case Panel.Priority.Essential  => true
        case Panel.Priority.Important  => wide && columns >= 50
        case Panel.Priority.Peripheral => wide && columns >= 100 && rows >= 24

    val (kept, dropped) = interface.panels.partition(fits)
    val rectoBeside = columns >= 90

    def role(role: Panel.Role): List[Panel] = kept.filter(_.role == role)

    Plan
      ( verso = role(Panel.Role.Navigation),
        centre = role(Panel.Role.Primary) + role(Panel.Role.Transcript),
        recto = role(Panel.Role.Detail) + role(Panel.Role.Inspector),
        log = role(Panel.Role.Log),
        prompt = role(Panel.Role.Prompt),
        status = role(Panel.Role.Status),
        dropped = dropped,
        rectoBeside = rectoBeside )

  // The pane tree for a plan. Every panel becomes a widget pane supplied by `widget`; the
  // title and the controls become fixed rows.
  def build
    ( interface: Interface,
      plan:      Plan,
      title:     Optional[Pane],
      toolbar:   Optional[Pane],
      widget:    Panel => Pane )
  :   Pane =

    def framed(panel: Panel): Pane =
      val pane = widget(panel)

      panel.hints[hints.terminal.Border] match
        case hints.terminal.Border.None    => pane
        case hints.terminal.Border.Heavy   => border(BorderStyle.heavy)(pane)
        case hints.terminal.Border.Rounded => border(BorderStyle.rounded)(pane)
        case hints.terminal.Border.Light   => border(BorderStyle.light)(pane)
        case _                             => border(BorderStyle.light)(pane)

    def column(panels: List[Panel]): Optional[Pane] =
      if panels.nil then Unset else stack(panels.map(framed)*)

    val versoWidth = plan.verso.stdlib.flatMap(_.hints[hints.Minimum].option).map(_.columns).maxOption.getOrElse(24)
    val rectoWidth = plan.recto.stdlib.flatMap(_.hints[hints.Minimum].option).map(_.columns).maxOption.getOrElse(32)

    val verso: Optional[Pane] = column(plan.verso).let(_.weight(0.0)).let: pane =>
      sized(pane, minWidth = versoWidth, maxWidth = versoWidth + 8)

    val centreBelow: List[Panel] = if plan.rectoBeside then Nil else plan.recto
    val centre: List[Pane] =
      column(plan.centre + centreBelow).lay(Nil: List[Pane])(List(_))
        + column(plan.log).lay(Nil: List[Pane])(List(_))
        + column(plan.prompt).lay(Nil: List[Pane]) { pane => List(pane.weight(0.0)) }

    val middle: Pane = if centre.nil then stack() else stack(centre*)

    val recto: Optional[Pane] =
      if plan.rectoBeside then column(plan.recto).let { pane => sized(pane, minWidth = rectoWidth, maxWidth = rectoWidth + 16) }
      else Unset

    val body: Pane =
      strip((verso.lay(Nil: List[Pane])(List(_)) + List(middle) + recto.lay(Nil: List[Pane])(List(_)))*)

    val status: Optional[Pane] = column(plan.status).let(_.weight(0.0))

    val rowsOf: List[Pane] =
      title.lay(Nil: List[Pane])(List(_)) + toolbar.lay(Nil: List[Pane])(List(_)) + List(body)
        + status.lay(Nil: List[Pane])(List(_))

    stack(rowsOf*)

  // A pane held to a column band: the sizing of every leaf beneath it is left alone, and the
  // branch itself is bounded.
  private def sized(pane: Pane, minWidth: Int, maxWidth: Int): Pane = pane match
    case Pane.Leaf(sizing, content)      => Pane.Leaf(sizing.copy(minWidth = minWidth, maxWidth = maxWidth), content)
    case Pane.Widget(sizing, fixture)    => Pane.Widget(sizing.copy(minWidth = minWidth, maxWidth = maxWidth), fixture)
    case Pane.Branch(sizing, arr, panes) => Pane.Branch(sizing.copy(minWidth = minWidth, maxWidth = maxWidth), arr, panes)
