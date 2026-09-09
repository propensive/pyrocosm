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

import java.io as ji
import java.util.concurrent as juc
import java.util.concurrent.atomic as juca
import java.util.concurrent.locks as jucl

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
import quantitative.*
import rudiments.*
import symbolism.*
import turbulence.*
import denominative.dysasymptotics.{linearAccess, linearSize}
import vacuous.*

import ultimatum.{strip, Form, Gaugeable, Gauging, InlineRoot, Occupancy, Pane, ScreenRoot, Sizing}

// An input stream over the console's own, which profanity's `interactive` closes when a session
// ends. Under an Ethereal daemon the console's stdin is the client's socket, so closing it would
// end the connection, and nothing the application printed after the session (a report, say)
// would reach the terminal. Closing this stream instead *detaches* it: a reader blocked in it
// returns end-of-input, which is what the session's key pump needs to finish, and the stream
// beneath stays open for the rest of the invocation. Reads poll for available input so that a
// detach can interrupt them; the interval is short enough not to be felt.
private class Detachable(underlying: ji.InputStream) extends ji.InputStream:
  @volatile
  private var detached: Boolean = false

  def read(): Int =
    while !detached && underlying.available() == 0 do jucl.LockSupport.parkNanos(1000000L)
    if detached then -1 else underlying.read()

  override def read(array: scala.Array[Byte] | Null, offset: Int, length: Int): Int =
    if length == 0 then 0 else
      val first = read()
      if first < 0 then -1 else
        array.nn(offset) = first.toByte
        var count = 1
        while count < length && !detached && underlying.available() > 0 do
          array.nn(offset + count) = underlying.read().toByte
          count += 1
        count

  override def available(): Int = if detached then 0 else underlying.available()
  override def close(): Unit = detached = true

// The terminal as a `Frontend`: the interface's panels are arranged into an Ultimatum form and
// conducted on the terminal until the user leaves with Escape, Ctrl+C or Ctrl+D, or the
// application calls `stop`. The frontend drives Ultimatum's `Form` itself rather than through
// `conduct`, for three things `conduct` cannot do: it owns the event iterator, so Tab reaches a
// code field (as Ctrl+Tab) and Shift+Tab moves focus instead; it owns the animation timer, so
// at most one is ever armed (Ultimatum's form re-arms after every refresh and forgets the
// pending one on any application redraw, which would leak a timer per `Live` assignment); and
// an inline session runs in cycles, each ending by committing a transcript's settled entries
// to the scrollback and starting a fresh block with the rest.
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

  // The running session's event spool, for `stop` to end; and whether `stop` has been called,
  // for a `run` that has not yet started (or is between starting and binding the spool).
  @caps.unsafe.untrackedCaptures
  private var spool: Optional[Relay[Terminal.Event]] = Unset

  @volatile
  @caps.unsafe.untrackedCaptures
  private var stopped: Boolean = false

  def stop(): Unit = synchronized:
    stopped = true
    spool.let(_.stop())

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

    // Every fixture, with the cells whose assignment should repaint it, and the panel it shows.
    val session = Session()
    val fixtures = scala.collection.mutable.ListBuffer[Refreshable]()
    val bindings = scala.collection.mutable.ListBuffer[(Live[?], Refreshable, Optional[Panel])]()

    def register[fixture <: Refreshable](fixture: fixture, panel: Optional[Panel], cells: Live[?]*): fixture =
      fixtures += fixture
      cells.foreach { cell => bindings += ((cell, fixture, panel)) }
      fixture

    def control(control: Control): Pane = control match
      case button: Control.Button =>
        val fixture = register(ButtonFocus(button, renderer, dispatch, session), Unset, button.enabled)
        Pane.Widget(Sizing(0.0, minWidth = fixture.measure(0)(0), maxWidth = fixture.measure(0)(0), minHeight = 1, maxHeight = 1), fixture)

      case toggle: Control.Toggle =>
        val fixture = register(ToggleFocus(toggle, renderer, dispatch, session), Unset, toggle.state)
        Pane.Widget(Sizing(0.0, minWidth = fixture.measure(0)(0), maxWidth = fixture.measure(0)(0), minHeight = 1, maxHeight = 1), fixture)

      case choice: Control.Choice =>
        Pane.Widget(Sizing(), register(ChoiceFocus(choice, renderer, dispatch, session), Unset, choice.current))

      case field: Control.Field =>
        Pane.Widget(Sizing(), register(CodeField(field, renderer, dispatch, session), Unset, field.value, field.decoration))

    // The session reads the console through a detachable stdin (see `Detachable`).
    val stdio0: Stdio = Stdio(console.stdio.out, console.stdio.err, Detachable(console.stdio.in), console.stdio.termcap)

    given consoleSession: Console = new Console:
      val stdio: Stdio = stdio0
      override def trap(handler: PartialFunction[UnixSignal | WindowsSignal, SignalResponse]): Unit =
        console.trap(handler)

    if stopped then handle(Event.Closed) else interactive: terminal ?=>
      synchronized:
        spool = terminal.events
        if stopped then terminal.events.stop()

      val columns = terminal.columns.or(80)
      val rows = terminal.rows.or(24)
      val plan = TerminalArrangement.plan(interface, columns, rows)

      val mode: Occupancy = interface.hints[hints.terminal.Occupancy] match
        case hints.terminal.Occupancy.Inline     => Occupancy.Inline
        case hints.terminal.Occupancy.Fullscreen => Occupancy.Fullscreen
        case _ => occupancy.or(if interface.panels.stdlib.length > 1 && interface.panels.all(_.role != Panel.Role.Transcript) then Occupancy.Fullscreen else Occupancy.Inline)

      val inlined: Boolean = mode == Occupancy.Inline

      // A panel is its content, with its own controls stacked beneath. An inline transcript is
      // passive, windowed on what is committed.
      def widget(panel: Panel): Pane =
        val maxRows: Optional[Int] = panel.hints[hints.terminal.MaxRows].let(_.rows)

        // A prompt's or a status bar's content is never focused (its keys belong to its
        // controls); an inline transcript is passive too, and windowed on what is committed.
        val passive: Boolean = panel.role match
          case Panel.Role.Prompt | Panel.Role.Status => true
          case Panel.Role.Transcript                 => inlined
          case _                                     => false

        val fixture: Refreshable =
          if passive then register(PassiveFixture(panel, renderer, session, inlined && panel.role == Panel.Role.Transcript), panel, panel.content)
          else register(PanelFixture(panel, renderer, dispatch, session), panel, panel.content)

        val content = Pane.Widget(Sizing(maxHeight = maxRows), fixture)
        if panel.controls.nil then content
        else ultimatum.stack((content :: panel.controls.map(control).map(_.weight(0.0)))*)

      // The title row, in a fullscreen session only: an inline block sits in the scrollback,
      // where a title would be repeated by every commit.
      val title: Optional[Pane] =
        if inlined || interface.title.nil then Unset
        else
          val fixture = TextFixture(List(e"$Bold(${renderer.phrase(interface.title)})"))
          Pane.Widget(Sizing(0.0, minHeight = 1, maxHeight = 1), fixture)

      val toolbar: Optional[Pane] =
        if interface.controls.nil then Unset
        else
          val spacer = ultimatum.panel(0.0, minWidth = 1, maxWidth = 1) { () }
          strip(interface.controls.bind { (control0: Control) => List(control(control0), spacer) }*).weight(0.0)

      val pane = TerminalArrangement.build(interface, plan, title, toolbar, widget)

      // Whether the current inline cycle is ending to commit transcript entries. The cycle is
      // ended by this sentinel on the spool, which the frontend's iterator stops at: stopping
      // the spool itself would end the form without the redraw queued just before.
      val committing: juca.AtomicBoolean = juca.AtomicBoolean(false)
      val sentinel: Terminal.Event = Keypress.EscapeSeq('\u0000')

      // The animation timer: one at most, whatever the form asks, and none from a cycle that
      // has ended (a stale wake would arm a second timer in the next cycle).
      val generation: juca.AtomicInteger = juca.AtomicInteger(0)
      val armed: juca.AtomicBoolean = juca.AtomicBoolean(false)

      val scheduleWake: Long => Unit = (delay: Long) =>
        if armed.compareAndSet(false, true) then
          val current = generation.get

          async:
            snooze((delay + 16).toDouble*Milli(Second))
            armed.set(false)
            if generation.get == current then terminal.events.put(Terminal.Info.Redraw)

          ()

      // A cell's assignment marks its fixture, which is what makes the form paint it, and
      // wakes the form. For an inline transcript with newly settled entries, the wake ends the
      // cycle instead: the form's last frame is the entries to commit alone (every other
      // fixture hides), and the form finishes the block with them in the scrollback.
      def wake(fixture: Refreshable, panel: Optional[Panel]): () => Unit = () =>
        fixture.mark()

        val settled: Int = panel.lay(0): panel =>
          if inlined && panel.role == Panel.Role.Transcript then Actions.settled(panel.content()) else 0

        if settled > session.frozen && !session.hiding && committing.compareAndSet(false, true) then
          session.upto = settled
          session.hiding = true
          terminal.events.put(Terminal.Info.Redraw)
          terminal.events.put(sentinel)
        else terminal.events.put(Terminal.Info.Redraw)

      bindings.foreach { (cell, fixture, panel) => cell.bindWake(wake(fixture, panel)) }

      // The form's events, with Tab given to a focused fixture that claims it (as Ctrl+Tab, which
      // the form passes on), Shift+Tab moving focus in its place, and the commit sentinel ending
      // the iteration, so the form returns.
      def events(): scala.collection.Iterator[Terminal.Event] =
        val underlying = terminal.eventIterator()

        new scala.collection.Iterator[Terminal.Event]:
          private var peeked: Optional[Terminal.Event] = Unset
          private var ended: Boolean = false

          def hasNext: Boolean =
            if ended then false else
              if peeked.absent then
                if underlying.hasNext then peeked = underlying.next() else ended = true

              if ended then false
              else if peeked == sentinel then
                ended = true
                false
              else true

          def next(): Terminal.Event =
            val event = peeked.or(underlying.next())
            peeked = Unset

            event match
              case Keypress.Tab =>
                val focused = session.focused
                if focused != null && focused.claimsTab then Keypress.Ctrl(Keypress.Tab) else Keypress.Tab

              case Keypress.Shift(Keypress.Tab) => Keypress.Tab
              case other                        => other

      val formWake: () => Unit = () => terminal.events.put(Terminal.Info.Redraw)

      try
        mode match
          case Occupancy.Fullscreen =>
            caps.unsafe.unsafeAssumeSeparate:
              profanity.terminalFeatures.alternateScreenFeature:
                val root = ScreenRoot(terminal)
                root.cursor(false)

                try Form(root, mode, pane, formWake, 0, 0, scheduleWake).run(events())
                finally root.finish()

          case Occupancy.Inline =>
            // A cycle ends when the user leaves, `stop` is called, or a commit is due: the
            // form's last frame then holds exactly the entries to commit, the form's own
            // `finish` leaves them in the scrollback, and the next cycle starts a fresh block
            // below with the rest. Redraws are neither throttled nor debounced (`conduct`
            // coalesces resizes), so the commit's redraw, queued just before the form's end,
            // is painted rather than deferred past it.
            def cycle(): Unit =
              val root = InlineRoot(terminal)
              Form(root, mode, pane, formWake, 0, 0, scheduleWake).run(events())

              if committing.get && !stopped then
                generation.incrementAndGet()
                session.frozen = session.upto
                session.hiding = false
                committing.set(false)
                cycle()

            cycle()
      finally
        interface.cells.each(_.unbindWakes())
        synchronized { spool = Unset }

      handle(Event.Closed)
