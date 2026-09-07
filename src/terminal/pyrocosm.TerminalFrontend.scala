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

import ambience.*
import anticipation.*
import denominative.*
import clavichord.*
import contingency.*
import escapade.*
import escritoire.*
import gigantism.Every
import gossamer.*
import hieroglyph.*
import parasite.*
import polysyllabic.*
import profanity.*
import rudiments.*
import symbolism.*
import denominative.dysasymptotics.{linearAccess, linearSize}
import vacuous.*

import ultimatum.{conduct, strip, Gaugeable, Gauging, Occupancy, Pane, Sizing}

// The terminal as a `Frontend`: the interface's panels are arranged into an Ultimatum form and
// conducted on the terminal until the user leaves with Escape, Ctrl+C or Ctrl+D. Every `Live`
// cell reachable from the interface is bound to the form's redraw wake for the duration, so an
// assignment from any thread repaints.
class TerminalFrontend(occupancy: Optional[Occupancy] = Unset, theme: TerminalTheme = TerminalTheme.default)
  ( using console:     Console,
          monitor:     Monitor,
          probate:     Probate,
          environment: Environment,
          features:    Every[Terminal.Feature],
          tactic:      Tactic[Terminal.Error],
          metric:      Text is Measurable,
          hyphenation: Hyphenation,
          tableStyle:  TableStyle,
          gauging:     Gauging,
          glyphs:      Gaugeable.Glyphs )
extends Frontend:

  def run(interface: Interface)(handle: Event => Unit): Unit =
    val renderer = TerminalRenderer(theme)

    val shortcuts: Map[Keypress, Action] =
      interface.shortcuts.map { (shortcut: Shortcut) => shortcut.keypress -> shortcut.action }.to[Map]

    // Declared shortcuts fire their action; everything else reaches the application as it is.
    def dispatch0(event: Event): Unit = event match
      case Event.Key(keypress) => shortcuts(keypress).lay(handle(event)) { action => handle(Event.Pressed(action)) }
      case other               => handle(other)

    // The fixtures live in a pane tree, which must stay pure; the handler they call is vouched
    // pure once here, as ultimatum does for the wakes its `Reading` cells hold.
    val dispatch: Event -> Unit = caps.unsafe.unsafeAssumePure(dispatch0)

    def control(control: Control): Pane = control match
      case button: Control.Button =>
        val fixture = ButtonFocus(button, renderer, dispatch)
        Pane.Widget(Sizing(0.0, minWidth = fixture.measure(0)(0), maxWidth = fixture.measure(0)(0), minHeight = 1, maxHeight = 1), fixture)

      case toggle: Control.Toggle =>
        val fixture = ToggleFocus(toggle, renderer, dispatch)
        Pane.Widget(Sizing(0.0, minWidth = fixture.measure(0)(0), maxWidth = fixture.measure(0)(0), minHeight = 1, maxHeight = 1), fixture)

      case choice: Control.Choice =>
        Pane.Widget(Sizing(), ChoiceFocus(choice, renderer, dispatch))

      case field: Control.Field =>
        Pane.Widget(Sizing(), CodeField(field, renderer, dispatch))

    // A panel is its content, with its own controls stacked beneath.
    def widget(panel: Panel): Pane =
      val maxRows: Optional[Int] = panel.hints[hints.terminal.MaxRows].let(_.rows)
      val content = Pane.Widget(Sizing(maxHeight = maxRows), PanelFixture(panel, renderer, dispatch))
      if panel.controls.nil then content
      else ultimatum.stack((content :: panel.controls.map(control).map(_.weight(0.0)))*)

    interactive: terminal ?=>
      val columns = terminal.columns.or(80)
      val rows = terminal.rows.or(24)
      val plan = TerminalArrangement.plan(interface, columns, rows)

      val title: Optional[Pane] =
        if interface.title.nil then Unset
        else
          val fixture = TextFixture(List(e"$Bold(${renderer.phrase(interface.title)})"))
          Pane.Widget(Sizing(0.0, minHeight = 1, maxHeight = 1), fixture)

      val toolbar: Optional[Pane] =
        if interface.controls.nil then Unset
        else
          val spacer = ultimatum.panel(0.0, minWidth = 1, maxWidth = 1) { () }
          strip(interface.controls.bind { (control0: Control) => List(control(control0), spacer) }*).weight(0.0)

      val pane = TerminalArrangement.build(interface, plan, title, toolbar, widget)

      val mode: Occupancy = interface.hints[hints.terminal.Occupancy] match
        case hints.terminal.Occupancy.Inline     => Occupancy.Inline
        case hints.terminal.Occupancy.Fullscreen => Occupancy.Fullscreen
        case _ => occupancy.or(if interface.panels.stdlib.length > 1 then Occupancy.Fullscreen else Occupancy.Inline)

      val wake: () => Unit = () => terminal.events.put(Terminal.Info.Redraw)
      interface.cells.each(_.bindWake(wake))

      try conduct(mode)(pane)
      finally interface.cells.each(_.unbindWakes())

    handle(Event.Closed)
