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

// Excluded from the umbrella: `Standing` (ultimatum), `Step` (ultimatum), `Token` (harlequin),
// which would outrank this package's own definitions, since a wildcard import beats a package
// member declared in another file.
import soundness.{Standing as _, Step as _, Token as _, *}

import dysasymptotics.{linearAccess, linearSize}

import murmuration.zip
import sortingAlgorithms.timsort
import columnAttenuation.ignoreAttenuation
import laneDagStyles.boxDrawingLaneDagStyle
import treeStyles.roundedTreeStyle
import processions.checklistProcession
import sparklines.blockSparkline
import timers.compactElapsed

// The static half of the terminal frontend: phrasing to a styled line, and a block to styled
// lines at a width. Everything effectful (repainting, focus, scrolling) is in the fixtures; this
// is a pure function of the model, the width, the animation tick and the current selection, so
// it is testable by calling it and reusable by anything that prints.
object TerminalRenderer:
  // The braille ring: six dots with a two-dot gap chasing round the cell, eight frames. The
  // same design as Ultimatum's `brailleRingSpinner`, added there alongside this and defined
  // here until the release carrying it; the ASCII line is its fallback.
  private[pyrocosm] def ring(using Gauging): Fraction is Gaugeable =
    ultimatum.Spinner.each(t"⣸⢹⠻⠟⡏⣇⣦⣴", 80, Gaugeable.Glyphs.Unicode, ultimatum.Spinner.each(t"-\\|/", 130, Gaugeable.Glyphs.Ascii)).gaugeable

  // The columnars fume's tables use: `Stretch` absorbs the spare width so a table spans the
  // line; `Rigid` never shrinks, so figures never wrap.
  object Stretch extends Columnar:
    def flex(metrics: Metrics, maxWidth: Int): Flex = Flex(metrics, 1.0, Unset)

    def fit[text: Textual { type Result = Char }]
      ( lines: Array[text]^{}, width: Int, textAlign: TextAlignment )
      ( using Text is Measurable, Hyphenation )
    :   Sequence[text] =

      val all: List[text] = List.from(lines.readable)
      all.bind { (line: text) => Flow.wrap(line, width) }.to[Sequence]

  object Rigid extends Columnar:
    def flex(metrics: Metrics, maxWidth: Int): Flex =
      Flex(Metrics(metrics.natural, metrics.natural), 0.0, metrics.natural)

    def fit[text: Textual { type Result = Char }]
      ( lines: Array[text]^{}, width: Int, textAlign: TextAlignment )
      ( using Text is Measurable, Hyphenation )
    :   Sequence[text] =

      val all: List[text] = List.from(lines.readable)
      all.bind { (line: text) => Flow.wrap(line, width) }.to[Sequence]

  def glyph(glyph: Glyph)(using glyphs: Gaugeable.Glyphs): Text =
    val unicode = glyphs != Gaugeable.Glyphs.Ascii

    glyph match
      case Glyph.Check      => if unicode then t"✓" else t"+"
      case Glyph.Cross      => if unicode then t"✗" else t"x"
      case Glyph.Warning    => if unicode then t"⚠" else t"!"
      case Glyph.Running    => if unicode then t"▶" else t">"
      case Glyph.Pending    => if unicode then t"·" else t"."
      case Glyph.Skipped    => if unicode then t"‑" else t"-"
      case Glyph.Bullet     => if unicode then t"•" else t"*"
      case Glyph.ArrowRight => if unicode then t"→" else t"->"
      case Glyph.ArrowUp    => if unicode then t"↑" else t"^"
      case Glyph.ArrowDown  => if unicode then t"↓" else t"v"
      case Glyph.Star       => if unicode then t"★" else t"*"
      case Glyph.Ellipsis   => if unicode then t"…" else t"..."
      case Glyph.Winner     => if unicode then t"★" else t"*"

  private def glyphTone(glyph: Glyph): Optional[Tone] = glyph match
    case Glyph.Check   => Tone.Success
    case Glyph.Cross   => Tone.Failure
    case Glyph.Warning => Tone.Warning
    case Glyph.Running => Tone.Accent
    case Glyph.Pending => Tone.Muted
    case Glyph.Skipped => Tone.Muted
    case Glyph.Winner  => Tone.Warning
    case _             => Unset

class TerminalRenderer(val theme: TerminalTheme = TerminalTheme.default)
  ( using metric:      Text is Measurable,
          hyphenation: Hyphenation,
          tableStyle:  TableStyle,
          gauging:     Gauging,
          glyphs:      Gaugeable.Glyphs ):

  import TerminalRenderer.{glyph, glyphTone, Stretch, Rigid}
  import Amounts.{figure, scaled}

  // The table machinery's evidence, for a `TableCache` rendering through this renderer.
  val measurable: Text is Measurable = metric
  val hyphenating: Hyphenation = hyphenation
  val tabling: TableStyle = tableStyle

  private def tint(chroma: Chroma)(text: Teletype): Teletype = e"${Fg(chroma)}($text)"
  private def toned(tone: Tone)(text: Teletype): Teletype = tint(theme.tone(tone))(text)
  private def bold(text: Teletype): Teletype = e"$Bold($text)"
  private def faint(text: Teletype): Teletype = tint(theme.muted)(text)

  private def joined(parts: List[Teletype]): Teletype =
    parts.fold(e"") { (joined: Teletype, part: Teletype) => joined.append(part) }

  def blank: Teletype = e""

  // ── Phrasing ──────────────────────────────────────────────────────────────────────────────

  def phrase(content: List[Inline]): Teletype = joined(content.map(inline1))

  def token(token: Token): Teletype =
    val coloured = tint(theme.accent(token.accent))(Teletype(token.text))
    if token.role == Token.Role.Binding then e"$Italic($coloured)" else coloured

  def tokens(tokens: List[Token]): Teletype = joined(tokens.map(token))

  private def inline1(node: Inline): Teletype = node match
    case Inline.Textual(text)         => Teletype(text)
    case Inline.Phrase(content)       => phrase(content)
    case Inline.Emphasis(content)     => e"$Italic(${phrase(content)})"
    case Inline.Strong(content)       => bold(phrase(content))
    case Inline.Toned(tone, content)  => toned(tone)(phrase(content))
    case Inline.Code(_, tokens0)      => tokens(tokens0)
    case Inline.Keystroke(keypress)   => bold(tint(theme.key)(Teletype(keypress.show)))
    case Inline.Reference(id)         => tint(theme.reference)(Teletype(id))
    case Inline.Break()               => Teletype(t"\n")
    case Inline.Math(math)            => Teletype(safely(Ergo.serialize(math)).or(t"…"))

    case Inline.Link(destination, content) =>
      val text = e"$Underline(${phrase(content)})"
      destination match
        case Inline.Destination.External(_)  => tint(theme.link)(text)
        case Inline.Destination.Internal(_)  => tint(theme.tone(Tone.Accent))(text)

    case Inline.Symbol(symbol) =>
      val text = Teletype(glyph(symbol))
      glyphTone(symbol).lay(text)(toned(_)(text))

    case Inline.Amount(value, units) =>
      val (number, unit) = scaled(value, units)
      e"${tint(theme.figure)(Teletype(number))} ${tint(theme.units)(Teletype(unit))}"

    case Inline.Figure(value, precision) =>
      tint(theme.figure)(Teletype(figure(value, precision)))

  // ── Blocks ────────────────────────────────────────────────────────────────────────────────

  // The lines of a run of blocks, one blank line between blocks.
  def blocks
    ( blocks: List[Block], width: Int, tick: Tick = Tick.zero, selected: Optional[Action] = Unset )
  :   List[Teletype] =

    val rendered: List[List[Teletype]] = blocks.map { (block0: Block) => block(block0, width, tick, selected) }

    rendered.indexed.bind: (lines: List[Teletype], index: Ordinal) =>
      val separator: List[Teletype] = if index.n0 == 0 then Nil else List(blank)
      separator + lines

  // A paragraph's phrasing, split at hard breaks and wrapped to the width.
  private def wrapped(content: List[Inline], width: Int): List[Teletype] =
    var done: List[List[Inline]] = Nil      // the lines already broken off, most recent first
    var current: List[Inline] = Nil         // the line being gathered, in reverse

    content.each: (node: Inline) =>
      node match
        case Inline.Break() =>
          done = current.reverse :: done
          current = Nil

        case other =>
          current = other :: current

    (current.reverse :: done).reverse.bind { (line: List[Inline]) => Flow.wrap(phrase(line), width.max(1)) }

  private def indented(lines: List[Teletype], prefix: Teletype, continuation: Teletype): List[Teletype] =
    lines.indexed.map { (line: Teletype, index: Ordinal) => (if index.n0 == 0 then prefix else continuation).append(line) }

  def block(block: Block, width: Int, tick: Tick = Tick.zero, selected: Optional[Action] = Unset)
  :   List[Teletype] =

    block match
      case Block.Paragraph(content) =>
        wrapped(content, width)

      case Block.Heading(level, content) =>
        val text = phrase(content)
        if level <= 1 then List(bold(e"$Underline($text)"))
        else if level == 2 then List(bold(text))
        else List(bold(faint(text)))

      case Block.Listing(ordered, items) =>
        val marks: List[Text] =
          if ordered then items.indexed.map { (_, index) => t"${index.n1}. " }
          else items.map { (_: Block.Item) => t"${glyph(Glyph.Bullet)} " }

        val indent = marks.map(_.length).maximum.or(2)

        items.zip(marks).bind: (item: Block.Item, mark: Text) =>
          val chosen = item.action.present && item.action == selected
          val body = blocks(item.content, width - indent, tick, selected)
          val prefix0 = Teletype(mark.pad(indent))
          val prefix = if chosen then e"$Reverse($prefix0)" else prefix0
          indented(body, prefix, Teletype(t" "*indent))

      case Block.Quotation(content) =>
        val bar = faint(Teletype(if glyphs == Gaugeable.Glyphs.Ascii then t"| " else t"▎ "))
        blocks(content, width - 2, tick, selected).map { (line: Teletype) => bar.append(line) }

      case Block.Rule(side) =>
        val ascii = glyphs == Gaugeable.Glyphs.Ascii
        val rule = side match
          case Block.Side.Above   => if ascii then t"_" else t"⎽"
          case Block.Side.Below   => if ascii then t"-" else t"‾"
          case Block.Side.Between => if ascii then t"-" else t"─"
        List(faint(Teletype(rule*width)))

      // A code line longer than the width is wrapped hard, as captured output is: code is not
      // prose, so a line is broken exactly at the width rather than clipped or reflowed.
      case Block.Code(_, lines, notes) =>
        lines.indexed.bind { (line, index) => hardWrap(codeLine(line, notes.filter(_.line == index.n0)), width) }

      case Block.Table(columns, rows, caption) =>
        table(columns, rows, caption, width, selected)

      case Block.Record(entries, title) =>
        val keyWidth = entries.map(_.key).map(phrase).map(_.length).maximum.or(0)
        val heading = title.lay(Nil: List[Teletype]) { title => List(bold(phrase(title))) }

        val body = entries.bind: (entry: Block.Entry) =>
          val key = bold(phrase(entry.key).pad(keyWidth))
          val value = blocks(entry.value, (width - keyWidth - 2).max(10), tick, selected)
          indented(value, e"$key  ", Teletype(t" "*(keyWidth + 2)))

        heading + body

      case Block.Notice(tone, title, content) =>
        val bar = toned(tone)(Teletype(if glyphs == Gaugeable.Glyphs.Ascii then t"| " else t"▌ "))
        val heading = title.lay(Nil: List[Teletype]) { title => List(bar.append(bold(toned(tone)(phrase(title))))) }
        heading + blocks(content, width - 2, tick, selected).map { (line: Teletype) => bar.append(line) }

      case Block.Disclosure(summary, content, open) =>
        val marker =
          if glyphs == Gaugeable.Glyphs.Ascii then (if open then t"v " else t"> ")
          else (if open then t"▾ " else t"▸ ")

        val head = faint(Teletype(marker)).append(phrase(summary))
        if open then head :: blocks(content, width - 2, tick, selected).map { (line: Teletype) => Teletype(t"  ").append(line) }
        else List(head)

      case Block.Image(source, alt) =>
        List(faint(e"$Italic([$alt])").append(faint(Teletype(t" $source"))))

      // A drawing has no terminal form; its description stands in for it.
      case Block.Figure(figure) =>
        List(faint(e"$Italic([${Inline.plain(figure.alt)}])"))

      case Block.Tree(roots) =>
        val diagram = TreeDiagram.by[Block.TreeNode](_.children)(roots*)
        val style = roundedTreeStyle[Teletype]
        diagram.render(treeLabel(selected))(using style).to[List]

      case Block.Graph(vertices, edges) =>
        safely(LaneDagDiagram(Block.Graph(vertices, edges).dag)).lay(vertices.map { (vertex: Block.Vertex) => vertexLabel(selected)(vertex) }): diagram =>
          val style = boxDrawingLaneDagStyle[Teletype]
          List.from(diagram.render(vertexLabel(selected))(using style))

      case Block.Chart(kind, series) =>
        chart(kind, series, width)

      case Block.Gauge(status, caption) =>
        gauge(status, caption, width, tick)

      // A group's members belong together: no blank line between them.
      case Block.Group(content) =>
        content.bind { (member: Block) => this.block(member, width, tick, selected) }

      // Captured output, verbatim: every visual row of every line behind a gutter naming the
      // stream (blue for standard output, red for standard error), and wrapped hard at the
      // width less the gutter, spaces and all, since a program's output is not prose.
      case Block.Output(text, error) =>
        val gutter: Teletype = e"${Fg(theme.tone(if error then Tone.Failure else Tone.Info))}(░) "
        val inner = (width - 2).max(1)
        val lines: List[Text] = text.cut(t"\n")
        val trimmed: List[Text] = if lines.last == t"" then lines.keep(lines.size - 1) else lines
        trimmed.bind { (line: Text) => hardWrap(Teletype(line), inner).map { (row: Teletype) => gutter.append(row) } }

  private def treeLabel(selected: Optional[Action])(node: Block.TreeNode): Teletype =
    val label = node.tone.lay(phrase(node.label))(toned(_)(phrase(node.label)))
    if node.action.present && node.action == selected then e"$Reverse($label)" else label

  private def vertexLabel(selected: Optional[Action])(vertex: Block.Vertex): Teletype =
    val label = vertex.tone.lay(phrase(vertex.label))(toned(_)(phrase(vertex.label)))
    if vertex.action.present && vertex.action == selected then e"$Reverse($label)" else label

  // One line of code: each token coloured by its accent, then the note ranges laid over it.
  // A styled line cut into rows of at most `width` columns, with an empty line kept as one row.
  def hardWrap(line: Teletype, width: Int): List[Teletype] =
    val columns = width.max(1)

    def recur(rest: Teletype, rows: List[Teletype]): List[Teletype] =
      if rest.length > columns then recur(rest.dropChars(columns), rest.takeChars(columns) :: rows)
      else (rest :: rows).reverse

    recur(line, Nil)

  def codeLine(line: Block.Line, notes: List[Block.Note]): Teletype =
    // Each token, cut at the boundaries where the styling may change within it (the ends of
    // the notes that fall inside it), with a running offset into the line.
    var offset: Int = 0
    var pieces: List[Teletype] = Nil

    line.tokens.each: (token: Token) =>
      val end = offset + token.text.length
      val ends: List[Int] = notes.bind { (note: Block.Note) => List(note.start, note.end) }
      val cuts: List[Int] = (offset :: end :: ends).filter { (cut: Int) => cut >= offset && cut <= end }.distinct.sort

      cuts.zip(cuts.tail).each: (from: Int, to: Int) =>
        val text = token.text.s.substring(from - offset, to - offset).nn.tt
        val base = this.token(token.copy(text = text))

        val styled = notes.seek { (note: Block.Note) => note.start <= from && note.end >= to }.let(_.style) match
          case Block.Note.Style.Erroneous => e"$Underline(${toned(Tone.Failure)(base)})"
          case Block.Note.Style.Caution   => e"$Underline(${toned(Tone.Warning)(base)})"
          case Block.Note.Style.Highlight => e"${Bg(theme.selection)}($base)"
          case Block.Note.Style.Param     => e"$Italic($base)"
          case _                          => base

        pieces = styled :: pieces

      offset = end

    joined(pieces.reverse)

  // The escritoire columns of a table's columns. A column's own decoration is the row's tone
  // alone: the selection highlight is a render-time decoration (`rowDecorations`), so that a
  // layout — and every row rendered against it — is independent of which row is selected.
  def scaffold(columns: List[Block.Column]): Scaffold[Block.Row, Teletype] =
    def sizing(column: Block.Column): Columnar = column.sizing match
      case Block.Sizing.Stretch               => Stretch
      case Block.Sizing.Rigid                 => Rigid
      case Block.Sizing.Paragraph             => columnar.Paragraph
      case Block.Sizing.Collapsible(priority) => columnar.Collapsible(priority)

    def alignment(column: Block.Column): TextAlignment = column.alignment match
      case Block.Alignment.Start   => TextAlignment.Left
      case Block.Alignment.End     => TextAlignment.Right
      case Block.Alignment.Center  => TextAlignment.Center
      case Block.Alignment.Justify => TextAlignment.Justify

    val definitions: List[Column[Block.Row, Teletype]] =
      columns.indexed.map: (column, index) =>
        Column[Block.Row, Teletype, Teletype]
          ( bold(phrase(column.title)),
            alignment(column),
            sizing = sizing(column),
            decorate = { (row: Block.Row) => row.tone.let { tone => (line: Teletype) => toned(tone)(line) } } ):
          row => row.cells.at(index).lay(blank) { cell => phrase(cell.content) }

    Scaffold[Block.Row](definitions*)

  // A row's cell decorations, one per column: the selection's background when the row is the
  // selected one, else its tone.
  def rowDecorations(row: Block.Row, columns: Int, selected: Boolean): List[Optional[Teletype -> Teletype]] =
    val decoration: Optional[Teletype -> Teletype] =
      if selected then ((line: Teletype) => e"${Bg(theme.selection)}($line)"): Optional[Teletype -> Teletype]
      else row.tone.let { tone => (line: Teletype) => toned(tone)(line) }

    List.fill(columns)(decoration)

  def caption(caption: Optional[List[Inline]]): List[Teletype] =
    caption.lay(Nil: List[Teletype]) { caption => List(faint(e"$Italic(${phrase(caption)})")) }

  // The whole table: a layout admitting every row, then each row rendered against it — the
  // reference a `TableCache` window must match line for line.
  private def table
    ( columns: List[Block.Column], rows: List[Block.Row], caption: Optional[List[Inline]],
      width: Int, selected: Optional[Action] )
  :   List[Teletype] =

    val scaffold0 = scaffold(columns)
    val phrased: List[(Block.Row, escritoire.Cells[Teletype])] = rows.map { row => (row, scaffold0.cells(row)) }
    val layout = phrased.fold(scaffold0.layout(width.max(4))) { (layout, pair) => layout.extend(pair(1)) }
    val count: Int = columns.size

    val body: List[Teletype] =
      phrased.bind: (row, cells) =>
        layout.lines(cells, rowDecorations(row, count, row.action.present && row.action == selected))

    layout.topRule.let(List(_)).or(Nil) + layout.titleLines + List(layout.titleRule) + body
      + layout.bottomRule.let(List(_)).or(Nil) + this.caption(caption)

  private def chart(kind: Block.Chart.Kind, series: List[Block.Series], width: Int): List[Teletype] =
    val labelWidth = series.map(_.label).map(phrase).map(_.length).maximum.or(0)
    val room = (width - labelWidth - 2).max(4)
    val most = series.bind(_.values).maximum.or(1.0).max(1e-9)

    def label(series0: Block.Series): Teletype =
      val text = phrase(series0.label).pad(labelWidth)
      series0.tone.lay(text)(toned(_)(text)).append(Teletype(t"  "))

    kind match
      case Block.Chart.Kind.Sparkline =>
        series.map { (series0: Block.Series) =>
          label(series0).append(gaugeLine(series0.values.to[Sequence], room))
        }

      case Block.Chart.Kind.Bars | Block.Chart.Kind.Histogram =>
        val fill = if glyphs == Gaugeable.Glyphs.Ascii then t"#" else t"█"

        series.bind: (series0: Block.Series) =>
          series0.values.indexed.map: (value: Double, index: Ordinal) =>
            val cells = ((value/most)*(room - 8)).toInt.max(if value > 0 then 1 else 0)
            val bar = series0.tone.lay(toned(Tone.Accent)(Teletype(fill*cells)))(toned(_)(Teletype(fill*cells)))
            val prefix = if index.n0 == 0 then label(series0) else Teletype(t" "*(labelWidth + 2))
            prefix.append(bar).append(faint(Teletype(t" ${figure(value)}")))

  private def gauge(status: Status, caption: Optional[List[Inline]], width: Int, tick: Tick)
  :   List[Teletype] =

    val text: Optional[Text] = caption.let(Inline.plain(_))

    inline def line[status: Gaugeable](value: status): List[Teletype] =
      text.lay(List(gaugeLine(value, width, tick))) { caption => List(gaugeLine(Captioned(value, caption), width, tick)) }

    // Progress of unknown extent is a one-character spinner beside its caption, not a bar
    // sweeping the width: there is no extent to show. Ultimatum's spinners are designs for a
    // `Fraction` that animate on the tick's frame.
    def spinner: List[Teletype] =
      val mark = gaugeLine(Fraction(0.0), 1, tick)(using TerminalRenderer.ring)
      List(text.lay(mark) { caption => e"$mark ${caption}" })

    status match
      case Status.Fraction(value)        => line(Fraction(value))
      case Status.Indeterminate()        => spinner
      case Status.Reckoning(done, total) => line(Reckoning(done, total))
      case Status.Elapsed(seconds)       => line(Duration((seconds*1000).toLong))
      case Status.Remaining(seconds)     => line(Countdown(Duration((seconds*1000).toLong)))

      case Status.Standing(standing) =>
        val mapped = standing match
          case Standing.Pending   => ultimatum.Standing.Pending
          case Standing.Running   => ultimatum.Standing.Running
          case Standing.Succeeded => ultimatum.Standing.Succeeded
          case Standing.Failed    => ultimatum.Standing.Failed
          case Standing.Warned    => ultimatum.Standing.Warned
          case Standing.Skipped   => ultimatum.Standing.Skipped

        val mark = gaugeLine(mapped, 1, tick)
        List(text.lay(mark) { caption => e"$mark ${caption}" })

      case Status.Steps(steps) =>
        val mapped: Sequence[ultimatum.Step] =
          steps.map { (step: Step) =>
            val standing = step.standing match
              case Standing.Pending   => ultimatum.Standing.Pending
              case Standing.Running   => ultimatum.Standing.Running
              case Standing.Succeeded => ultimatum.Standing.Succeeded
              case Standing.Failed    => ultimatum.Standing.Failed
              case Standing.Warned    => ultimatum.Standing.Warned
              case Standing.Skipped   => ultimatum.Standing.Skipped

            ultimatum.Step(Inline.plain(step.name), standing, step.detail.let(Inline.plain(_)))
          } .to[Sequence]

        val rows = gaugeRows(mapped, width, tick)
        text.lay(rows) { caption => bold(Teletype(caption)) :: rows }

  // Plain text, for logs and machine-adjacent output.
  def plain(blocks0: List[Block], width: Int): Text =
    blocks(blocks0, width).map(_.plain).join(t"\n")
