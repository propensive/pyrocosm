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

// Excluded from the umbrella: `Token` (harlequin), which would outrank this package's own
// definitions, since a wildcard import beats a package member declared in another file.
import soundness.{Token as _, *}

import clavichord.Keypress

import dysasymptotics.{linearAccess, linearSize}

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

  @volatile
  @caps.unsafe.untrackedCaptures
  var resetting: Boolean = false

  // Whether the next inline cycle is to replay: the terminal was resized, so what was committed
  // has reflowed unpredictably; the screen and scrollback are cleared and the settled entries
  // committed afresh at the new width before the block resumes.
  @volatile
  @caps.unsafe.untrackedCaptures
  var replaying: Boolean = false

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
  // it); otherwise Tab is the form's, and moves focus. Likewise Escape, which a code field
  // showing candidates takes to dismiss them; otherwise Escape is the form's, and leaves.
  def claimsTab: Boolean = false
  def claimsEscape: Boolean = false

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
  private val started: Long = java.lang.System.nanoTime

  @caps.unsafe.untrackedCaptures
  private var selection: Int = 0

  @caps.unsafe.untrackedCaptures
  private var offset: Optional[Int] = Unset

  private def follow: Boolean = panel.hints.has[hints.Follow.type]

  private def tick: Tick = Tick.at((java.lang.System.nanoTime - started)/1000000L, 80)

  // The content's actions and whether it animates, found once per content (by identity, since
  // it is immutable): a table of thousands of rows offers as many actions, and the questions
  // are asked on every keypress and every period query.
  @caps.unsafe.untrackedCaptures
  private var known: AnyRef | Null = null

  @caps.unsafe.untrackedCaptures
  private var actions0: List[Action] = Nil

  @caps.unsafe.untrackedCaptures
  private var actionCount: Int = 0

  @caps.unsafe.untrackedCaptures
  private var animated0: Boolean = false

  private def examine(content: List[Block]): Unit =
    if !content.asInstanceOf[AnyRef].eq(known.asInstanceOf[AnyRef]) then
      known = content.asInstanceOf[AnyRef]
      actions0 = Actions.of(content)
      actionCount = actions0.size
      animated0 = Actions.animated(content)

  private def actions: List[Action] = { examine(panel.content()); actions0 }
  private def selected: Optional[Action] = actions.at(selection.z)

  override def pulse: Optional[Int] = { examine(panel.content()); if animated0 then 80 else Unset }

  // The content as a run of segments, each a top-level block (a group's members spliced in,
  // sharing its separator) rendered whole, or a table kept incrementally in a `TableCache`.
  // Segments persist between refreshes and are matched to the new content by position and
  // identity: a table whose rows changed keeps its cache and updates it.
  private enum Segment:
    case Whole(block: Block, separator: Boolean, lines: List[Teletype], frame: Long, selected: Optional[Action], actionable: Boolean)
    case Tabular(block: Block.Table, separator: Boolean, cache: TableCache)

    def separator: Boolean
    def height: Int = this match
      case Whole(_, _, lines, _, _, _) => lines.size
      case Tabular(_, _, cache)         => cache.height

  @caps.unsafe.untrackedCaptures
  private var segments: List[Segment] = Nil

  @caps.unsafe.untrackedCaptures
  private var segmented: AnyRef | Null = null

  @caps.unsafe.untrackedCaptures
  private var segmentWidth: Int = -1

  @caps.unsafe.untrackedCaptures
  private var segmentFrame: Long = 0L

  @caps.unsafe.untrackedCaptures
  private var segmentSelected: Optional[Action] = Unset

  // The blocks a content list paints, with whether each follows a separator: exactly the
  // arrangement `TerminalRenderer.blocks` makes, a group's members joined without one.
  private def flatten(content: List[Block]): List[(Block, Boolean)] =
    content.indexed.bind: (block, index) =>
      val separator = index.n0 > 0
      block match
        case Block.Group(members) => members.indexed.map { (member, position) => (member, separator && position.n0 == 0) }
        case other                => List((other, separator))

  private def refresh(width0: Int, focused: Boolean): Unit =
    val content = panel.content()
    val width = width0.max(1)
    examine(content)
    val frame: Long = if animated0 then (java.lang.System.nanoTime - started)/80000000L else 0L
    val selected0: Optional[Action] = if focused then selected else Unset
    val fresh = !content.asInstanceOf[AnyRef].eq(segmented.asInstanceOf[AnyRef]) || width != segmentWidth
    val moved = frame != segmentFrame || selected0 != segmentSelected

    if fresh || moved then
      val previous: List[Segment] = segments

      segments =
        flatten(content).indexed.map: (pair, index) =>
          val (block, separator) = pair
          val old: Optional[Segment] = previous.at(index)

          block match
            case table: Block.Table =>
              val cache: TableCache = old match
                case Segment.Tabular(previousTable, _, cache) if previousTable.columns == table.columns && previousTable.caption == table.caption => cache
                case _ => TableCache(renderer, table.columns, table.caption)

              cache.update(table.rows, width)
              Segment.Tabular(table, separator, cache)

            case other =>
              old match
                case Segment.Whole(previousBlock, _, lines, oldFrame, oldSelected, actionable)
                    if previousBlock.eq(other) && width == segmentWidth
                    && (oldFrame == frame || !Actions.animated(List(other)))
                    && (oldSelected == selected0 || !actionable) =>
                  Segment.Whole(other, separator, lines, frame, selected0, actionable)

                case _ =>
                  val actionable = !Actions.of(List(other)).nil
                  Segment.Whole(other, separator, renderer.block(other, width, tick, selected0), frame, selected0, actionable)

      segmented = content.asInstanceOf[AnyRef]
      segmentWidth = width
      segmentFrame = frame
      segmentSelected = selected0

  private def heading(focused: Boolean): List[Teletype] =
    panel.title.lay(Nil: List[Teletype]): title =>
      val text = renderer.phrase(title)
      List(if focused then e"$Bold(${Fg(renderer.theme.tone(Tone.Accent))}($text))" else e"$Bold($text)")

  private def total: Int =
    segments.fold(0) { (sum, segment) => sum + (if segment.separator then 1 else 0) + segment.height }

  // The minimum height: the content, for the panels which are short by nature; a few rows for
  // the rest, which scroll, so that a long primary panel does not starve the others.
  def measure(width: Int): (Int, Int) = if session.hiding then (0, 0) else measure0(width)

  private def measure0(width: Int): (Int, Int) =
    refresh(width, false)
    val rows = (panel.title.lay(0) { _ => 1 } + total).max(1)
    val bounded = panel.hints[hints.terminal.MaxRows].lay(rows) { hint => rows.min(hint.rows) }

    panel.role match
      case Panel.Role.Status | Panel.Role.Prompt | Panel.Role.Navigation => (0, bounded)
      case _                                                             => (0, bounded.min(4))

  def render(canvas: Board^, focused: Boolean): Unit =
    session.focus(this, focused)
    if session.hiding then hidden(canvas) else paint(canvas, focused)

  // The visible window: the heading, then each segment's slice of the rows it spans; a table
  // renders only the rows within the window.
  private def paint(canvas: Board^, focused: Boolean): Unit =
    refresh(canvas.width, focused)
    val head: List[Teletype] = heading(focused)
    val all = head.size + total
    val height = canvas.height.max(1)
    val start = offset.or(if follow then (all - height).max(0) else 0).min((all - height).max(0))
    val until = start + height
    val selected0: Optional[Action] = if focused then selected else Unset

    canvas.clear()

    var row = 0

    def emit(lines: List[Teletype]): Unit =
      lines.each: line =>
        canvas.move(Prim, row.z)
        canvas.put(line)
        row += 1

    def slice(lines: List[Teletype], position: Int): Unit =
      val keepFrom = (start - position).max(0)
      val keepUntil = until - position
      if keepUntil > 0 && keepFrom < lines.size then emit(lines.skip(keepFrom).keep(keepUntil - keepFrom))

    slice(head, 0)
    var position = head.size

    segments.each: segment =>
      if segment.separator then
        if position >= start && position < until then emit(List(blank))
        position += 1

      val size = segment.height

      if position < until && position + size > start then segment match
        case Segment.Whole(_, _, lines, _, _, _) => slice(lines, position)
        case Segment.Tabular(_, _, cache)         => emit(cache.window((start - position).max(0), (until - position).min(size), selected0))

      position += size

    canvas.cursor(false)
    canvas.flush()
    painted()

  private def blank: Teletype = Teletype(t"")

  def handle(event: Terminal.Event): Unit = event match
    case Keypress.Up =>
      if !actions.nil then selection = (selection - 1).max(0)
      else offset = (offset.or(0) - 1).max(0)

    case Keypress.Down =>
      if !actions.nil then selection = (selection + 1).min(actionCount - 1)
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
  private val started: Long = java.lang.System.nanoTime

  private class Rendering(val content: List[Block], val from: Int, val until: Int, val width: Int, val frame: Long, val lines: List[Teletype])

  @caps.unsafe.untrackedCaptures
  private var rendering: Optional[Rendering] = Unset

  override def pulse: Optional[Int] = if Actions.animated(panel.content()) then 80 else Unset

  private def lines(width: Int): List[Teletype] =
    val content = panel.content()
    val from = if windowed then session.frozen else 0
    val until = if windowed && session.hiding then session.upto else content.size
    val frame: Long = if Actions.animated(content) then (java.lang.System.nanoTime - started)/80000000L else 0L

    rendering.let { current =>
      if current.content == content && current.from == from && current.until == until
          && current.width == width && current.frame == frame
      then current.lines else Unset }
    . or:
      val window: List[Block] = content.excerpt(from, until)
      val tick = Tick.at((java.lang.System.nanoTime - started)/1000000L, 80)
      // A window that continues committed entries is separated from them by a blank line, as
      // the entries are from each other.
      val rendered = renderer.blocks(window, width.max(1), tick, Unset)
      val result = if from > 0 && !window.nil then renderer.blank :: rendered else rendered
      rendering = Rendering(content, from, until, width, frame, result)
      result

  def measure(width: Int): (Int, Int) =
    if session.hiding && !windowed then (0, 0) else (0, lines(width).size)

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
  def measure(width: Int): (Int, Int) = (0, lines.size.max(1))

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

  private val count: Int = choice.options.size

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

  // Where in the field's history (oldest first) recall stands: past the end is the draft.
  @caps.unsafe.untrackedCaptures
  private var recall: Int = field.history().size

  private def history: List[Text] = field.history()

  // The candidates are shown as they arrive, but none is taken until the user picks one with
  // Tab or Down: only then does Enter accept it, and the ghost preview it. Escape dismisses
  // them until the next keystroke.
  @caps.unsafe.untrackedCaptures
  private var completion: Int = 0

  @caps.unsafe.untrackedCaptures
  private var selected: Boolean = false

  @caps.unsafe.untrackedCaptures
  private var dismissed: Boolean = false

  private val maxCompletions: Int = 6

  private def decoration: Control.Field.Decoration = field.decoration()
  private def completions: List[Control.Field.Completion] = if dismissed then Nil else decoration.completions

  // The application's prompt, drawn before the first row of the text. Every row of the text
  // starts past it, so a wrapped row or a later line hangs beneath the first, as a REPL's
  // continuation lines do. A prompt that leaves no column for the text beside it is not drawn
  // at all: the extent wraps rather than clips, so an overlong row would spill onto the row
  // beneath.
  private def prompt: Teletype = renderer.phrase(decoration.prompt)

  private def indent(width: Int): Int =
    val prefix = prompt.length
    if prefix < width then prefix else 0

  // The columns left for the text beside the prompt.
  private def inner(width: Int): Int = (width - indent(width)).max(1)

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
    if !selected then Unset else completions.at(completion.z).let: candidate =>
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
    selected = false
    publish()

  private def accept(): Unit =
    completions.at(completion.z).let { candidate => insert(candidate, candidate.name) }

  // Tab: a lone candidate is accepted; several first extend the text to their longest common
  // prefix, when that is longer than what they cover, then select the first, then cycle. So
  // `pri` becomes `print`, then Tab walks println, print, printf.
  private def complete(): Unit =
    completions match
      // No candidates yet: the application hears the Tab and may supply some.
      case Nil => dispatch(Event.Key(Keypress.Tab))
      case single :: Nil => insert(single, single.name)
      case first :: rest =>
        val prefix: String = rest.fold(first.name.s) { (prefix: String, candidate: Control.Field.Completion) => CodeField.commonPrefix(prefix, candidate.name.s) }
        val covered = replaced(first)
        if prefix.length > covered.length && prefix.startsWith(covered.s) then insert(first, prefix.tt)
        else if !selected then
          selected = true
          completion = 0
        else completion = (completion + 1)%completions.size

  override def claimsEscape: Boolean = !completions.nil

  private def valueLines: List[Teletype] =
    val value = editor.value
    val tokens = decoration.tokens
    val covered = !tokens.nil && tokens.map(_.text).join == value

    if !covered then value.cut(t"\n").map(Teletype(_)) else
      // The token run as lines, so each token is coloured on its own line, with the
      // decoration's marks (error and warning spans) laid over each as a code block's notes.
      Block.Line.split(tokens).indexed.map { (line: Block.Line, index: Ordinal) =>
        renderer.codeLine(line, decoration.marks.filter(_.line == index.n0)) }

  override def claimsTab: Boolean = true

  // The detail beneath the text, as blocks wrapped to the width: what the input is being read
  // as, what it has brought into scope.
  private def noteLines: List[Teletype] = noteLines(lastWidth)

  private def noteLines(width: Int): List[Teletype] =
    if decoration.detail.nil then Nil else renderer.blocks(decoration.detail, width.max(1))

  @caps.unsafe.untrackedCaptures
  private var lastWidth: Int = 80

  // One row per candidate, cut to the width: a signature can run long, and a wrapped row
  // would push the rows beneath it out of the field.
  private def completionLines: List[Teletype] = completionLines(lastWidth)

  private def completionLines(width: Int): List[Teletype] =
    completions.indexed.map: (candidate, index) =>
      val line: Teletype = e"  ${candidate.name}  ${Fg(renderer.theme.muted)}(${candidate.signature})"
      val cut: Teletype = if line.length > width.max(1) then line.takeChars(width.max(1)) else line
      if selected && index.n0 == completion then e"$Reverse($cut)" else cut
    . keep(maxCompletions)

  def measure(width: Int): (Int, Int) =
    if session.hiding then (0, 0) else
      sync()
      lastWidth = width
      (0, visualLines(width).size.max(1) + noteLines(width).size + completionLines(width).size)

  // The value's lines as rows of the width left beside the prompt: a line longer than that
  // wraps hard, and the caret's column beyond it falls onto the row below. A caret at the very
  // end of a line exactly that long sits at the start of the next row, which is where a
  // wrapped line's next character would go. The rows carry no indent; `paint` positions them.
  private def visualLines(width: Int): List[Teletype] =
    val columns = inner(width)
    valueLines.bind { (line: Teletype) => renderer.hardWrap(line, columns) }

  private def visualCaret(width: Int): (Int, Int) =
    val columns = inner(width)
    val (row, column) = caret
    val above: Int = valueLines.keep(row).map { (line: Teletype) => (line.length/columns).max(0) + (if line.length % columns == 0 && line.length > 0 then 0 else 1) }.total
    (above + column/columns, indent(width) + column % columns)

  def render(canvas: Board^, focused: Boolean): Unit =
    session.focus(this, focused)
    if session.hiding then hidden(canvas) else paint(canvas, focused)

  private def paint(canvas: Board^, focused: Boolean): Unit =
    sync()
    canvas.clear()

    val width = canvas.width
    val offset = indent(width)
    val lines = visualLines(width)
    val placeholder = field.placeholder.let(Teletype(_))

    // The prompt before the first row, and every row of the text past it; the clear has
    // already blanked the columns beneath the prompt on the rows below.
    if offset > 0 then
      canvas.move(Prim, Prim)
      canvas.put(prompt)

    lines.indexed.each: (line, index) =>
      canvas.move(offset.z, index.n0.z)
      canvas.put(line)

    // The ghost after the caret, and the placeholder when there is nothing at all.
    if editor.value == t"" then
      canvas.move(offset.z, Prim)
      placeholder.let { text => canvas.put(e"${Fg(renderer.theme.muted)}($text)") }

    val (row, column) = visualCaret(width)
    ghost.let: text =>
      canvas.move(column.z, row.z)
      canvas.put(e"${Fg(renderer.theme.muted)}($text)")

    val notes = noteLines(width)

    notes.indexed.each: (line, index) =>
      canvas.move(Prim, (lines.size + index.n0).z)
      canvas.put(line)

    completionLines(width).indexed.each: (line, index) =>
      canvas.move(Prim, (lines.size + notes.size + index.n0).z)
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
    recall = history.size + (if value == t"" then 0 else 1)
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
        if selected && ghost.present then accept()
        else if decoration.incomplete then newline()
        else submit()

      case Keypress.Shift(Keypress.Enter) =>
        newline()

      // Tab, delivered by the frontend as Ctrl+Tab so that the form does not take it for focus.
      case Keypress.Tab | Keypress.Ctrl(Keypress.Tab) =>
        complete()

      // Escape, delivered by the frontend as Ctrl+Escape so that the form does not take it for
      // leaving: the candidates are dismissed until the next keystroke.
      case Keypress.Ctrl(Keypress.Escape) =>
        dismissed = true
        selected = false

      case Keypress.Up =>
        if !completions.nil && selected then
          if completion == 0 then selected = false else completion -= 1
        else if !multiline && recall > 0 then
          recall -= 1
          editor = LineEditor(history.at(recall.z).or(t""), mode = LineEditor.Mode.Multiline(_ => false))
          publish()
        else
          editor = editor(event)

      case Keypress.Down =>
        if !completions.nil then
          if !selected then
            selected = true
            completion = 0
          else completion = (completion + 1).min(completions.size - 1)
        else if !multiline && recall < history.size then
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
        selected = false
        dismissed = false
        publish()

      case _ =>
        ()
