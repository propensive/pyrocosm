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

import anticipation.*
import denominative.*
import clavichord.*
import denominative.*
import escapade.*
import gossamer.*
import hieroglyph.*
import profanity.*
import rudiments.*
import symbolism.*
import denominative.dysasymptotics.{linearAccess, linearSize}
import spectacular.*
import vacuous.*

import ultimatum.{Fixture, Focus, Tick}

// What one running session shares with its fixtures: which fixture was last painted focused
// (so the frontend can give it Tab), and the state of an inline transcript: how many entries
// are committed to the scrollback already, so the live block shows only the rest; and, while
// a commit is being painted as the block's last frame, that every other fixture is to paint
// nothing and the transcript only the entries being committed.
class Session():
  @volatile
  @caps.unsafe.untrackedCaptures
  var focused: Refreshable | Null = null

  @volatile
  @caps.unsafe.untrackedCaptures
  var frozen: Int = 0

  @volatile
  @caps.unsafe.untrackedCaptures
  var hiding: Boolean = false

  @volatile
  @caps.unsafe.untrackedCaptures
  var upto: Int = 0

  def focus(fixture: Refreshable, focused: Boolean): Unit = if focused then this.focused = fixture

// A fixture whose appearance an application changes from another thread, through a `Live` cell.
// A fullscreen form repaints only what it knows has changed: the entry that handled a key, and
// any entry with a period. So the frontend marks the fixture when one of its cells is assigned,
// the fixture reports a period while marked (any period: it is only a flag to the form), and
// clears the mark once it has painted. `pulse` is a genuine animation period, when the content
// animates, and is what keeps the form's timer armed. Reporting a period at all other times
// costs a full re-render of every panel on every tick, and worse: the form re-arms its timer
// after every refresh and forgets the pending one whenever an application redraw arrives, so a
// wake per assignment leaks a permanent timer each time (see `TerminalFrontend`).
trait Refreshable extends Fixture:
  @caps.unsafe.untrackedCaptures
  private var marked: Boolean = true

  def mark(): Unit = marked = true
  protected def painted(): Unit = marked = false
  def pulse: Optional[Int] = Unset

  // Whether Tab is this fixture's to handle when it is focused (a code field completes with
  // it); otherwise Tab is the form's, and moves focus.
  def claimsTab: Boolean = false

  // Painting nothing, while a transcript's commit is painted: not even a clear, since a
  // zero-height extent still clears the row it sits on, which is the commit's last.
  protected def hidden(canvas: Board^): Unit = painted()
  override def period: Optional[Int] = if pulse.present then pulse else if marked then 1 else Unset

// A panel's content, painted from its live cell. Focusable: when the content offers actions
// (selectable rows, items, tree nodes, vertices), Up and Down move the selection and Enter
// presses it; otherwise they scroll. Any other key is reported as `Event.Key`.
class PanelFixture(panel: Panel, renderer: TerminalRenderer, dispatch: Event -> Unit, session: Session)
extends Focus, Refreshable:

  @caps.unsafe.untrackedCaptures
  private val started: Long = System.nanoTime

  @caps.unsafe.untrackedCaptures
  private var selection: Int = 0

  @caps.unsafe.untrackedCaptures
  private var offset: Optional[Int] = Unset

  private def follow: Boolean = panel.hints.has[hints.Follow.type]

  private def tick: Tick = Tick.at((System.nanoTime - started)/1000000L, 80)
  private def actions: List[Action] = Actions.of(panel.content())
  private def selected: Optional[Action] = actions.at(selection.z)

  override def pulse: Optional[Int] = if Actions.animated(panel.content()) then 80 else Unset

  // One rendering serves the measure and the paint of a refresh, and every refresh after it
  // until something it depends on changes: the content (by identity, since it is immutable),
  // the width, focus and selection, and the animation frame when the content animates.
  private class Rendering
    ( val content: List[Block], val width: Int, val focused: Boolean, val selection: Int, val frame: Long,
      val lines: List[Teletype] ):

    def matches(content0: List[Block], width0: Int, focused0: Boolean, selection0: Int, frame0: Long): Boolean =
      (content.stdlib eq content0.stdlib) && width == width0 && focused == focused0
        && selection == selection0 && frame == frame0

  @caps.unsafe.untrackedCaptures
  private var rendering: Optional[Rendering] = Unset

  private def lines(width: Int, focused: Boolean): List[Teletype] =
    val content = panel.content()
    val frame: Long = if Actions.animated(content) then (System.nanoTime - started)/80000000L else 0L
    val selection0: Int = if focused then selection else -1

    rendering.let { current => if current.matches(content, width, focused, selection0, frame) then current.lines else Unset }.or:
      val heading: List[Teletype] = panel.title.lay(Nil: List[Teletype]): title =>
        val text = renderer.phrase(title)
        List(if focused then e"$Bold(${Fg(renderer.theme.tone(Tone.Accent))}($text))" else e"$Bold($text)")

      val result = heading + renderer.blocks(content, width.max(1), tick, if focused then selected else Unset)
      rendering = Rendering(content, width, focused, selection0, frame, result)
      result

  // The minimum height: the content, for the panels which are short by nature; a few rows for
  // the rest, which scroll, so that a long primary panel does not starve the others.
  def measure(width: Int): (Int, Int) = if session.hiding then (0, 0) else measure0(width)

  private def measure0(width: Int): (Int, Int) =
    val rows = lines(width, false).stdlib.length.max(1)
    val bounded = panel.hints[hints.terminal.MaxRows].lay(rows) { hint => rows.min(hint.rows) }

    panel.role match
      case Panel.Role.Status | Panel.Role.Prompt | Panel.Role.Navigation => (0, bounded)
      case _                                                             => (0, bounded.min(4))

  def render(canvas: Board^, focused: Boolean): Unit =
    session.focus(this, focused)
    if session.hiding then hidden(canvas) else paint(canvas, focused)

  private def paint(canvas: Board^, focused: Boolean): Unit =
    val all = lines(canvas.width, focused)
    val total = all.stdlib.length
    val height = canvas.height.max(1)
    val start = offset.or(if follow then (total - height).max(0) else 0).min((total - height).max(0))

    canvas.clear()
    var row = 0
    val iterator = all.stdlib.drop(start).iterator

    while row < height && iterator.hasNext do
      canvas.move(Prim, row.z)
      canvas.put(iterator.next())
      row += 1

    canvas.cursor(false)
    canvas.flush()
    painted()

  def handle(event: Terminal.Event): Unit = event match
    case Keypress.Up =>
      if !actions.nil then selection = (selection - 1).max(0)
      else offset = (offset.or(0) - 1).max(0)

    case Keypress.Down =>
      if !actions.nil then selection = (selection + 1).min(actions.stdlib.length - 1)
      else offset = offset.or(0) + 1

    case Keypress.Enter =>
      selected.let { action => dispatch(Event.Pressed(action)) }

    case keypress: Keypress =>
      dispatch(Event.Key(keypress))

    case _ =>
      ()

// A panel painted from its live cell but never focused: a prompt's or a status bar's content,
// whose keys belong elsewhere, and an inline transcript, which is `windowed`: it shows the
// entries not yet committed to the scrollback (the session says how many are), or, while a
// commit is being painted, exactly the entries being committed.
class PassiveFixture(panel: Panel, renderer: TerminalRenderer, session: Session, windowed: Boolean)
extends Refreshable:

  @caps.unsafe.untrackedCaptures
  private val started: Long = System.nanoTime

  private class Rendering(val content: List[Block], val from: Int, val until: Int, val width: Int, val frame: Long, val lines: List[Teletype])

  @caps.unsafe.untrackedCaptures
  private var rendering: Optional[Rendering] = Unset

  override def pulse: Optional[Int] = if Actions.animated(panel.content()) then 80 else Unset

  private def lines(width: Int): List[Teletype] =
    val content = panel.content()
    val from = if windowed then session.frozen else 0
    val until = if windowed && session.hiding then session.upto else content.stdlib.length
    val frame: Long = if Actions.animated(content) then (System.nanoTime - started)/80000000L else 0L

    rendering.let { current =>
      if (current.content.stdlib eq content.stdlib) && current.from == from && current.until == until
          && current.width == width && current.frame == frame
      then current.lines else Unset }
    . or:
      val window: List[Block] = List.from(content.stdlib.slice(from, until))
      val tick = Tick.at((System.nanoTime - started)/1000000L, 80)
      val result = renderer.blocks(window, width.max(1), tick, Unset)
      rendering = Rendering(content, from, until, width, frame, result)
      result

  def measure(width: Int): (Int, Int) =
    if session.hiding && !windowed then (0, 0) else (0, lines(width).stdlib.length)

  def render(canvas: Board^, focused: Boolean): Unit =
    if session.hiding && !windowed then hidden(canvas) else paint(canvas)

  private def paint(canvas: Board^): Unit =
    canvas.clear()
    lines(canvas.width).indexed.each: (line, index) =>
      canvas.move(Prim, index.n0.z)
      canvas.put(line)
    canvas.cursor(false)
    canvas.flush()
    painted()

// A fixed run of styled lines: the title bar.
class TextFixture(lines: List[Teletype]) extends Fixture:
  def measure(width: Int): (Int, Int) = (0, lines.stdlib.length.max(1))

  def render(canvas: Board^, focused: Boolean): Unit =
    canvas.clear()
    lines.indexed.each: (line, index) =>
      canvas.move(Prim, index.n0.z)
      canvas.put(line)
    canvas.flush()

class ButtonFocus(button: Control.Button, renderer: TerminalRenderer, dispatch: Event -> Unit, session: Session)
extends Focus, Refreshable:

  private def label: Teletype = e"[ ${renderer.phrase(button.label)} ]"

  def measure(width: Int): (Int, Int) = if session.hiding then (0, 0) else (label.length, 1)

  def render(canvas: Board^, focused: Boolean): Unit =
    session.focus(this, focused)
    if session.hiding then hidden(canvas) else paint(canvas, focused)

  private def paint(canvas: Board^, focused: Boolean): Unit =
    canvas.clear()
    canvas.move(Prim, Prim)
    val text =
      if !button.enabled() then e"${Fg(renderer.theme.muted)}($label)"
      else if focused then e"$Reverse($label)"
      else e"$Bold($label)"
    canvas.put(text)
    canvas.cursor(false)
    canvas.flush()
    painted()

  def handle(event: Terminal.Event): Unit = event match
    case Keypress.Enter | Keypress.CharKey(' ') => if button.enabled() then dispatch(Event.Pressed(button.action))
    case keypress: Keypress                     => dispatch(Event.Key(keypress))
    case _                                      => ()

class ToggleFocus(toggle: Control.Toggle, renderer: TerminalRenderer, dispatch: Event -> Unit, session: Session)
extends Focus, Refreshable:

  private def line: Teletype =
    val box = if toggle.state() then t"[x] " else t"[ ] "
    e"$box${renderer.phrase(toggle.label)}"

  def measure(width: Int): (Int, Int) = if session.hiding then (0, 0) else (line.length, 1)

  def render(canvas: Board^, focused: Boolean): Unit =
    session.focus(this, focused)
    if session.hiding then hidden(canvas) else paint(canvas, focused)

  private def paint(canvas: Board^, focused: Boolean): Unit =
    canvas.clear()
    canvas.move(Prim, Prim)
    canvas.put(if focused then e"$Reverse($line)" else line)
    canvas.cursor(false)
    canvas.flush()
    painted()

  def handle(event: Terminal.Event): Unit = event match
    case Keypress.Enter | Keypress.CharKey(' ') =>
      toggle.state() = !toggle.state()
      dispatch(Event.Toggled(toggle.toggle, toggle.state()))

    case keypress: Keypress => dispatch(Event.Key(keypress))
    case _                  => ()

class ChoiceFocus(choice: Control.Choice, renderer: TerminalRenderer, dispatch: Event -> Unit, session: Session)
extends Focus, Refreshable:

  private val count: Int = choice.options.stdlib.length

  def measure(width: Int): (Int, Int) = if session.hiding then (0, 0) else (0, count.max(1))

  def render(canvas: Board^, focused: Boolean): Unit =
    session.focus(this, focused)
    if session.hiding then hidden(canvas) else paint(canvas, focused)

  private def paint(canvas: Board^, focused: Boolean): Unit =
    canvas.clear()
    choice.options.indexed.each: (option, index) =>
      canvas.move(Prim, index.n0.z)
      val current = index.n0 == choice.current()
      val marker = if !current then t"   " else if focused then t" > " else t" · "
      val line = e"$marker${renderer.phrase(option)}"
      canvas.put(if current && focused then e"$Reverse($line)" else line)
    canvas.cursor(false)
    canvas.flush()
    painted()

  private def move(delta: Int): Unit =
    val next = (choice.current() + delta).max(0).min(count - 1)
    if next != choice.current() then
      choice.current() = next
      dispatch(Event.Chosen(choice.choice, next))

  def handle(event: Terminal.Event): Unit = event match
    case Keypress.Up        => move(-1)
    case Keypress.Down      => move(1)
    case keypress: Keypress => dispatch(Event.Key(keypress))
    case _                  => ()

// A text or code field on a line editor. The application decorates it through the field's
// `Live[Decoration]`: highlighting tokens are laid over the text when they cover it exactly,
// completions are listed beneath with the first shown as ghost text after the caret, and
// `incomplete` decides whether Enter submits or inserts a newline. Up and Down move through the
// completions when there are any, else through the submission history on a single-line value.
// Tab is the form's focus key, so Right at the end of the text accepts the ghost.
object CodeField:
  def commonPrefix(left: String, right: String): String =
    var n = 0
    while n < left.length && n < right.length && left.charAt(n) == right.charAt(n) do n += 1
    left.substring(0, n).nn

class CodeField(field: Control.Field, renderer: TerminalRenderer, dispatch: Event -> Unit, session: Session)
extends Focus, Refreshable:

  @caps.unsafe.untrackedCaptures
  private var editor: LineEditor =
    LineEditor(field.value(), mode = LineEditor.Mode.Multiline(_ => false))

  @caps.unsafe.untrackedCaptures
  private var published: Text = field.value()

  @caps.unsafe.untrackedCaptures
  private var history: List[Text] = Nil

  @caps.unsafe.untrackedCaptures
  private var recall: Int = 0

  @caps.unsafe.untrackedCaptures
  private var completion: Int = 0

  private val maxCompletions: Int = 6

  private def decoration: Control.Field.Decoration = field.decoration()
  private def completions: List[Control.Field.Completion] = decoration.completions

  // The application may replace the value (clearing it after a submission, say); adopt it.
  private def sync(): Unit =
    if field.value() != published then
      editor = LineEditor(field.value(), mode = LineEditor.Mode.Multiline(_ => false))
      published = field.value()

  private def publish(): Unit =
    published = editor.value
    field.value() = editor.value

    field.notification match
      case Control.Field.Notify.Keystrokes => dispatch(Event.Edited(field.input, editor.value, editor.position))
      case _                               => ()

  private def isIdentifier(char: Char): Boolean = char.isLetterOrDigit || char == '_'

  // The identifier being typed, immediately before the caret.
  private def stem: Text =
    val before = editor.value.s.substring(0, editor.position).nn
    before.reverse.takeWhile(isIdentifier).reverse.tt

  private def atEnd: Boolean = editor.position == editor.value.length

  // What a candidate replaces: the whole text, or the identifier before the caret.
  private def replaced(candidate: Control.Field.Completion): Text = if candidate.whole then editor.value else stem

  private def ghost: Optional[Text] =
    completions.at(completion.z).let: candidate =>
      val current = replaced(candidate)
      if atEnd && candidate.name.starts(current) && candidate.name.length > current.length
      then candidate.name.s.substring(current.length).nn.tt
      else Unset

  // Replace what the candidate covers with it.
  private def insert(candidate: Control.Field.Completion, name: Text): Unit =
    val current = replaced(candidate)
    val start = if candidate.whole then 0 else editor.position - current.length
    val end = if candidate.whole then editor.value.length else editor.position
    val text = t"${editor.value.s.substring(0, start).nn}$name${editor.value.s.substring(end).nn}"
    editor = LineEditor(text, start + name.length, LineEditor.Mode.Multiline(_ => false))
    completion = 0
    publish()

  private def accept(): Unit =
    completions.at(completion.z).let { candidate => insert(candidate, candidate.name) }

  // Tab: a lone candidate is accepted; several first extend the text to their longest common
  // prefix, when that is longer than what they cover, and otherwise cycle. So `pri` becomes
  // `print`, then Tab walks println, print, printf.
  private def complete(): Unit =
    val all: scala.List[Control.Field.Completion] = completions.stdlib

    if all.length == 1 then insert(all.head, all.head.name)
    else if all.length > 1 then
      val prefix: String = all.map(_.name.s).reduce(CodeField.commonPrefix)
      val covered = replaced(all.head)
      if prefix.length > covered.length && prefix.startsWith(covered.s) then insert(all.head, prefix.tt)
      else completion = (completion + 1)%all.length

  private def valueLines: List[Teletype] =
    val value = editor.value
    val tokens = decoration.tokens
    val covered = !tokens.nil && tokens.map(_.text).join == value

    if !covered then value.cut(t"\n").map(Teletype(_)) else
      // Split the token run at newlines into lines, so each token is coloured on its own line.
      val lines = scala.collection.mutable.ListBuffer[Teletype](Teletype(t""))
      tokens.each: token =>
        val parts = token.text.cut(t"\n")
        parts.indexed.each: (part, index) =>
          if index.n0 > 0 then lines += Teletype(t"")
          lines(lines.length - 1) = lines(lines.length - 1).append(renderer.token(token.copy(text = part)))
      lines.to(List)

  override def claimsTab: Boolean = true

  // The detail beneath the text, as blocks wrapped to the width: what the input is being read
  // as, what it has brought into scope.
  private def noteLines: List[Teletype] = noteLines(lastWidth)

  private def noteLines(width: Int): List[Teletype] =
    if decoration.detail.nil then Nil else renderer.blocks(decoration.detail, width.max(1))

  @caps.unsafe.untrackedCaptures
  private var lastWidth: Int = 80

  private def completionLines: List[Teletype] =
    completions.indexed.map: (candidate, index) =>
      val line: Teletype = e"  ${candidate.name}  ${Fg(renderer.theme.muted)}(${candidate.signature})"
      if index.n0 == completion then e"$Reverse($line)" else line
    . stdlib.take(maxCompletions).to(List)

  def measure(width: Int): (Int, Int) =
    if session.hiding then (0, 0) else
      sync()
      lastWidth = width
      (0, valueLines.stdlib.length.max(1) + noteLines(width).stdlib.length + completionLines.stdlib.length)

  def render(canvas: Board^, focused: Boolean): Unit =
    session.focus(this, focused)
    if session.hiding then hidden(canvas) else paint(canvas, focused)

  private def paint(canvas: Board^, focused: Boolean): Unit =
    sync()
    canvas.clear()

    val lines = valueLines
    val placeholder = field.placeholder.let(Teletype(_))

    lines.indexed.each: (line, index) =>
      canvas.move(Prim, index.n0.z)
      canvas.put(line)

    // The ghost after the caret, and the placeholder when there is nothing at all.
    if editor.value == t"" then
      canvas.move(Prim, Prim)
      placeholder.let { text => canvas.put(e"${Fg(renderer.theme.muted)}($text)") }

    val (row, column) = caret
    ghost.let: text =>
      canvas.move(column.z, row.z)
      canvas.put(e"${Fg(renderer.theme.muted)}($text)")

    val notes = noteLines(canvas.width)

    notes.indexed.each: (line, index) =>
      canvas.move(Prim, (lines.stdlib.length + index.n0).z)
      canvas.put(line)

    completionLines.indexed.each: (line, index) =>
      canvas.move(Prim, (lines.stdlib.length + notes.stdlib.length + index.n0).z)
      canvas.put(line)

    canvas.showCaret(column.z, row.z)
    canvas.cursor(focused)
    canvas.flush()
    painted()

  private def caret: (Int, Int) =
    val before = editor.value.s.substring(0, editor.position).nn
    val row = before.count(_ == '\n')
    val column = before.length - before.lastIndexOf('\n') - 1
    (row, column)

  private def submit(): Unit =
    val value = editor.value
    if value != t"" then history = history :+ value
    recall = history.stdlib.length
    completion = 0
    editor = LineEditor(t"", mode = LineEditor.Mode.Multiline(_ => false))
    publish()
    dispatch(Event.Submitted(field.input, value))

  private def multiline: Boolean = editor.value.contains(t"\n")

  // A continued line: when the caret ends its line, the new line starts with the same leading
  // whitespace, as an editor would; a newline in the middle of text splits it plainly.
  private def newline(): Unit =
    val value = editor.value.s
    val position = editor.position
    val rest = value.substring(position).nn
    val indent: String =
      if rest.isEmpty || rest.startsWith("\n") then
        val before = value.substring(0, position).nn
        val line = before.substring(before.lastIndexOf('\n') + 1).nn
        line.takeWhile(_ == ' ')
      else ""
    val text = t"${value.substring(0, position).nn}\n$indent$rest"
    editor = LineEditor(text, position + 1 + indent.length, LineEditor.Mode.Multiline(_ => false))
    completion = 0
    publish()

  def handle(event: Terminal.Event): Unit =
    sync()

    event match
      case Keypress.Enter =>
        if !completions.nil && ghost.present then accept()
        else if decoration.incomplete then newline()
        else submit()

      case Keypress.Shift(Keypress.Enter) =>
        newline()

      // Tab, delivered by the frontend as Ctrl+Tab so that the form does not take it for focus.
      case Keypress.Tab | Keypress.Ctrl(Keypress.Tab) =>
        complete()

      case Keypress.Up =>
        if !completions.nil then completion = (completion - 1).max(0)
        else if !multiline && recall > 0 then
          recall -= 1
          editor = LineEditor(history.at(recall.z).or(t""), mode = LineEditor.Mode.Multiline(_ => false))
          publish()
        else
          editor = editor(event)

      case Keypress.Down =>
        if !completions.nil then completion = (completion + 1).min(completions.stdlib.length - 1)
        else if !multiline && recall < history.stdlib.length then
          recall += 1
          editor = LineEditor(history.at(recall.z).or(t""), mode = LineEditor.Mode.Multiline(_ => false))
          publish()
        else
          editor = editor(event)

      case Keypress.Right if atEnd && ghost.present =>
        accept()

      case keypress: Keypress =>
        editor = editor(keypress)
        completion = 0
        publish()

      case _ =>
        ()
