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
      dropped:     List[Panel],
      rectoBeside: Boolean )

  def plan(interface: Interface, columns: Int, rows: Int): Plan =
    // What fits: an essential panel always; an important one unless the terminal is very
    // narrow; a peripheral one only when there is room to spare. A `Minimum` hint below the
    // terminal's size drops any but an essential panel.
    def fits(panel: Panel): Boolean =
      val minimum = panel.hints[hints.Minimum]
      val wide = minimum.lay(true): hint => hint.columns <= columns && hint.rows <= rows

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
        dropped = dropped,
        rectoBeside = rectoBeside )

  // The pane tree for a plan. Every panel becomes a widget pane supplied by `widget`; the
  // title and the controls become fixed rows, and the activities a band beneath the matter.
  // `hidden` says whether every fixture is painting nothing (an inline commit's last frame),
  // when a border must vanish with its content rather than stand as an empty box. `spaced` puts
  // a blank row above the prompts (an inline block's), as a transcript puts one between entries,
  // so a line keeps its place when it is submitted.
  def build
    ( interface:  Interface,
      plan:       Plan,
      title:      Optional[Tui.Pane],
      toolbar:    Optional[Tui.Pane],
      activities: Optional[Tui.Pane],
      widget:     Panel => Tui.Pane,
      hidden:     () -> Boolean = () => false,
      spaced:     Boolean       = false )
  :   Tui.Pane =

    def framed(panel: Panel): Tui.Pane =
      val pane = widget(panel)

      panel.hints[hints.terminal.Border] match
        case hints.terminal.Border.None    => pane
        case hints.terminal.Border.Heavy   => border(BorderStyle.heavy, hidden)(pane)
        case hints.terminal.Border.Rounded => border(BorderStyle.rounded, hidden)(pane)
        case hints.terminal.Border.Light   => border(BorderStyle.light, hidden)(pane)
        case hints.terminal.Border.Rules   => rules(hidden)(pane)
        case _                             => border(BorderStyle.light, hidden)(pane)

    def column(panels: List[Panel]): Optional[Tui.Pane] =
      if panels.nil then Unset else stack(panels.map(framed)*)

    def widest(panels: List[Panel]): Optional[Int] =
      panels.map(_.hints[hints.Minimum])
      . sweep { case minimum: hints.Minimum => minimum.columns }
      . maximum

    val versoWidth = widest(plan.verso).or(24)
    val rectoWidth = widest(plan.recto).or(32)

    val verso: Optional[Tui.Pane] = column(plan.verso).let(_.weight(0.0)).let: pane =>
      sized(pane, minWidth = versoWidth, maxWidth = versoWidth + 8)

    val centreBelow: List[Panel] = if plan.rectoBeside then Nil else plan.recto

    val centre: List[Tui.Pane] =
      column(plan.centre + centreBelow).lay(Nil: List[Tui.Pane])(List(_)) +
        column(plan.log).lay(Nil: List[Tui.Pane])(List(_)) +
        column(plan.prompt).lay(Nil: List[Tui.Pane]): pane =>
          if spaced then List(gap(hidden), pane.weight(0.0)) else List(pane.weight(0.0))

    val middle: Tui.Pane = if centre.nil then stack() else stack(centre*)

    val recto: Optional[Tui.Pane] =
      if plan.rectoBeside then
        column(plan.recto).let: pane =>
          sized(pane, minWidth = rectoWidth, maxWidth = rectoWidth + 16)
      else
        Unset

    val body: Tui.Pane =
      val panes: List[Tui.Pane] =
        verso.lay(Nil: List[Tui.Pane])(List(_)) + List(middle) +
          recto.lay(Nil: List[Tui.Pane])(List(_))

      strip(panes*)

    val rowsOf: List[Tui.Pane] =
      title.lay(Nil: List[Tui.Pane])(List(_)) + toolbar.lay(Nil: List[Tui.Pane])(List(_)) +
        List(body) + activities.lay(Nil: List[Tui.Pane])(List(_))

    stack(rowsOf*)

  // A piece of a border: a rule or a corner, of fixed size while shown and of none while
  // every fixture hides.
  private class Edge(hidden: () -> Boolean, columns: Int, rows: Int, draw: Board^ -> Unit)
  extends Fixture:
    def measure(width: Int): (Int, Int) = if hidden() then (0, 0) else (columns, rows)

    def render(canvas: Board^, focused: Boolean): Unit =
      if !hidden() then
        canvas.clear()
        draw(canvas)
        canvas.flush()

  // A blank row, hiding with the content beneath it, so an inline commit leaves no blank behind.
  private def gap(hidden: () -> Boolean): Tui.Pane =
    Tui.Pane.Widget(Tui.Sizing(1.0, maxHeight = 1), new Edge(hidden, 0, 1, { _ => () })).weight(0.0)

  // A low rule above and a high rule below, no sides: the frame a transcript keeps of its
  // entries, drawn here in the foreground's full strength, where the entries' are faint.
  private def rules(hidden: () -> Boolean)(child: Tui.Pane): Tui.Pane =
    def rule(glyph: Text): Tui.Pane =
      Tui.Pane.Widget(Tui.Sizing(1.0, maxHeight = 1), new Edge(hidden, 0, 1, { canvas =>
        canvas.move(Prim, Prim)
        canvas.put(Teletype(glyph*canvas.width)) })).weight(0.0)

    val share: Double = child match
      case Tui.Pane.Leaf(sizing, _)      => sizing.fraction
      case Tui.Pane.Widget(sizing, _)    => sizing.fraction
      case Tui.Pane.Branch(sizing, _, _) => sizing.fraction

    stack(rule(t"⎽"), child.weight(share), rule(t"⎺"))

  // Ultimatum's `border`, with edges that hide with the content they frame.
  private def border(style: BorderStyle, hidden: () -> Boolean)(child: Tui.Pane): Tui.Pane =
    def horizontalRule: Tui.Pane =
      Tui.Pane.Widget(Tui.Sizing(1.0, maxHeight = 1), new Edge(hidden, 0, 1, { canvas =>
        canvas.move(Prim, Prim)
        canvas.put(Teletype(style.horizontal*canvas.width)) }))

    def verticalRule: Tui.Pane =
      Tui.Pane.Widget(Tui.Sizing(0.0, maxWidth = 1), new Edge(hidden, 1, 0, { canvas =>
        var row = 0

        while row < canvas.height do
          canvas.move(Prim, row.z)
          canvas.put(Teletype(style.vertical))
          row += 1 }))

    def corner(glyph: Text): Tui.Pane =
      val sizing = Tui.Sizing(0.0, maxWidth = 1, maxHeight = 1)

      Tui.Pane.Widget(sizing, new Edge(hidden, 1, 1, { canvas =>
        canvas.move(Prim, Prim)
        canvas.put(Teletype(glyph)) }))

    // The bands take no share of the stack's height (their rule's weight is for the strip's
    // width); the middle takes the child's share.
    def band(left: Text, right: Text): Tui.Pane =
      strip(corner(left), horizontalRule, corner(right)).weight(0.0)

    val share: Double = child match
      case Tui.Pane.Leaf(sizing, _)      => sizing.fraction
      case Tui.Pane.Widget(sizing, _)    => sizing.fraction
      case Tui.Pane.Branch(sizing, _, _) => sizing.fraction

    stack
      ( band(style.topLeft, style.topRight),
        strip(verticalRule, child, verticalRule).weight(share),
        band(style.bottomLeft, style.bottomRight) )

  // A pane held to a column band: the sizing of every leaf beneath it is left alone, and the
  // branch itself is bounded.
  private def sized(pane: Tui.Pane, minWidth: Int, maxWidth: Int): Tui.Pane =
    def bound(sizing: Tui.Sizing): Tui.Sizing =
      sizing.copy(minWidth = minWidth, maxWidth = maxWidth)

    pane match
      case Tui.Pane.Leaf(sizing, content)      => Tui.Pane.Leaf(bound(sizing), content)
      case Tui.Pane.Widget(sizing, fixture)    => Tui.Pane.Widget(bound(sizing), fixture)
      case Tui.Pane.Branch(sizing, arr, panes) => Tui.Pane.Branch(bound(sizing), arr, panes)
