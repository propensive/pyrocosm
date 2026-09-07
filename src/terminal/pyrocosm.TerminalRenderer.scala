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

import scala.collection.immutable as sci

import anticipation.*
import archimedes.*
import contingency.*
import dendrology.*
import denominative.*
import escapade.*
import escritoire.*
import gossamer.*
import hieroglyph.*
import polysyllabic.*
import rudiments.*
import symbolism.*
import denominative.dysasymptotics.{linearAccess, linearSize}
import spectacular.*
import tessellate.*
import vacuous.*

import ultimatum.{gaugeLine, gaugeRows, Captioned, Countdown, Fraction, Gaugeable, Gauging,
    Reckoning, Tick}

import aviation.Duration
import columnAttenuation.ignoreAttenuation
import dendrology.laneDagStyles.boxDrawingLaneDagStyle
import dendrology.treeStyles.roundedTreeStyle
import ultimatum.processions.checklistProcession
import ultimatum.sparklines.blockSparkline
import ultimatum.timers.compactElapsed

// The static half of the terminal frontend: phrasing to a styled line, and a block to styled
// lines at a width. Everything effectful (repainting, focus, scrolling) is in the fixtures; this
// is a pure function of the model, the width, the animation tick and the current selection, so
// it is testable by calling it and reusable by anything that prints.
object TerminalRenderer:
  // The columnars fume's tables use: `Stretch` absorbs the spare width so a table spans the
  // line; `Rigid` never shrinks, so figures never wrap.
  object Stretch extends Columnar:
    def flex[text: Textual { type Result = Char }](lines: Array[text]^{}, maxWidth: Int)
      ( using Text is Measurable )
    :   Flex =

      var metrics = Metrics(0, 0)
      lines.each { line => metrics = metrics.max(Flow.metrics(line)) }
      Flex(metrics, 1.0, Unset)

    def fit[text: Textual { type Result = Char }]
      ( lines: Array[text]^{}, width: Int, textAlign: TextAlignment )
      ( using Text is Measurable, Hyphenation )
    :   Sequence[text] =

      Sequence.from:
        lines.readable.to(sci.IndexedSeq).flatMap { line => Flow.wrap(line, width).stdlib }.toVector

  object Rigid extends Columnar:
    def flex[text: Textual { type Result = Char }](lines: Array[text]^{}, maxWidth: Int)
      ( using Text is Measurable )
    :   Flex =

      var metrics = Metrics(0, 0)
      lines.each { line => metrics = metrics.max(Flow.metrics(line)) }
      Flex(Metrics(metrics.natural, metrics.natural), 0.0, metrics.natural)

    def fit[text: Textual { type Result = Char }]
      ( lines: Array[text]^{}, width: Int, textAlign: TextAlignment )
      ( using Text is Measurable, Hyphenation )
    :   Sequence[text] =

      Sequence.from:
        lines.readable.to(sci.IndexedSeq).flatMap { line => Flow.wrap(line, width).stdlib }.toVector

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

  private def tint(chroma: Chroma)(text: Teletype): Teletype = e"${Fg(chroma)}($text)"
  private def toned(tone: Tone)(text: Teletype): Teletype = tint(theme.tone(tone))(text)
  private def bold(text: Teletype): Teletype = e"$Bold($text)"
  private def faint(text: Teletype): Teletype = tint(theme.muted)(text)

  private def joined(parts: List[Teletype]): Teletype =
    parts.stdlib.foldLeft(e"")(_.append(_))

  private def blank: Teletype = e""

  // ── Phrasing ──────────────────────────────────────────────────────────────────────────────

  def phrase(content: List[Inline]): Teletype = joined(content.map(inline1))

  def token(token: Token): Teletype =
    val coloured = tint(theme.accent(token.accent))(Teletype(token.text))
    if token.role == Token.Role.Binding then e"$Italic($coloured)" else coloured

  def tokens(tokens: List[Token]): Teletype = joined(tokens.map(token))

  private def inline1(node: Inline): Teletype = node match
    case Inline.Textual(text)         => Teletype(text)
    case Inline.Phrase(content)       => phrase(content)
    case Inline.Emphasis(content)     => bold(phrase(content))
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

    rendered.stdlib.zipWithIndex.flatMap { (lines, index) =>
      (if index == 0 then scala.Nil else scala.List(blank)) ++ lines.stdlib
    } .to(List)

  // A paragraph's phrasing, split at hard breaks and wrapped to the width.
  private def wrapped(content: List[Inline], width: Int): List[Teletype] =
    val hard: scala.List[scala.List[Inline]] =
      content.stdlib.foldLeft(scala.List(scala.List.empty[Inline])): (acc, node) =>
        node match
          case Inline.Break() => scala.Nil :: acc
          case other          => (other :: acc.head) :: acc.tail
      . map(_.reverse).reverse

    hard.flatMap { line => Flow.wrap(phrase(line.to(List)), width.max(1)).stdlib }.to(List)

  private def indented(lines: List[Teletype], prefix: Teletype, continuation: Teletype): List[Teletype] =
    lines.stdlib.zipWithIndex.map { (line, index) =>
      (if index == 0 then prefix else continuation).append(line)
    } .to(List)

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

        val indent = marks.map(_.length).stdlib.maxOption.getOrElse(2)

        items.stdlib.zip(marks.stdlib).flatMap { (item, mark) =>
          val chosen = item.action.present && item.action == selected
          val body = blocks(item.content, width - indent, tick, selected)
          val prefix0 = Teletype(mark.pad(indent))
          val prefix = if chosen then e"$Reverse($prefix0)" else prefix0
          indented(body, prefix, Teletype(t" "*indent)).stdlib
        } .to(List)

      case Block.Quotation(content) =>
        val bar = faint(Teletype(if glyphs == Gaugeable.Glyphs.Ascii then t"| " else t"▎ "))
        blocks(content, width - 2, tick, selected).map { (line: Teletype) => bar.append(line) }

      case Block.Rule() =>
        val rule = if glyphs == Gaugeable.Glyphs.Ascii then t"-" else t"─"
        List(faint(Teletype(rule*width)))

      case Block.Code(_, lines, notes) =>
        lines.indexed.map { (line, index) => codeLine(line, notes.filter(_.line == index.n0)) }

      case Block.Table(columns, rows, caption) =>
        table(columns, rows, caption, width, selected)

      case Block.Record(entries, title) =>
        val keyWidth = entries.map(_.key).map(phrase).map(_.length).stdlib.maxOption.getOrElse(0)
        val heading = title.lay(Nil: List[Teletype]) { title => List(bold(phrase(title))) }

        val body = entries.stdlib.flatMap { entry =>
          val key = bold(phrase(entry.key).pad(keyWidth))
          val value = blocks(entry.value, (width - keyWidth - 2).max(10), tick, selected)
          indented(value, e"$key  ", Teletype(t" "*(keyWidth + 2))).stdlib
        } .to(List)

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

      case Block.Tree(roots) =>
        val diagram = TreeDiagram.by[Block.TreeNode](_.children)(roots*)
        val style = roundedTreeStyle[Teletype]
        List.from(diagram.render(treeLabel(selected))(using style).stdlib)

      case Block.Graph(vertices, edges) =>
        safely(LaneDagDiagram(Block.Graph(vertices, edges).dag)).lay(vertices.map { (vertex: Block.Vertex) => vertexLabel(selected)(vertex) }): diagram =>
          val style = boxDrawingLaneDagStyle[Teletype]
          List.from(diagram.render(vertexLabel(selected))(using style))

      case Block.Chart(kind, series) =>
        chart(kind, series, width)

      case Block.Gauge(status, caption) =>
        gauge(status, caption, width, tick)

      case Block.Group(content) =>
        blocks(content, width, tick, selected)

  private def treeLabel(selected: Optional[Action])(node: Block.TreeNode): Teletype =
    val label = node.tone.lay(phrase(node.label))(toned(_)(phrase(node.label)))
    if node.action.present && node.action == selected then e"$Reverse($label)" else label

  private def vertexLabel(selected: Optional[Action])(vertex: Block.Vertex): Teletype =
    val label = vertex.tone.lay(phrase(vertex.label))(toned(_)(phrase(vertex.label)))
    if vertex.action.present && vertex.action == selected then e"$Reverse($label)" else label

  // One line of code: each token coloured by its accent, then the note ranges laid over it.
  private def codeLine(line: Block.Line, notes: List[Block.Note]): Teletype =
    var offset = 0
    val pieces = scala.collection.mutable.ListBuffer[Teletype]()

    line.tokens.each: token =>
      val end = offset + token.text.length
      // The boundaries at which the styling may change within this token.
      val cuts: scala.List[Int] =
        (offset :: end :: notes.stdlib.flatMap { note => scala.List(note.start, note.end) })
        . filter { cut => cut >= offset && cut <= end }.distinct.sorted

      cuts.zip(cuts.tail).foreach { (from, to) =>
        val text = token.text.s.substring(from - offset, to - offset).nn.tt
        val base = this.token(token.copy(text = text))
        val styled = notes.stdlib.find { note => note.start <= from && note.end >= to }.map(_.style) match
          case Some(Block.Note.Style.Erroneous) => e"$Underline(${toned(Tone.Failure)(base)})"
          case Some(Block.Note.Style.Caution)   => e"$Underline(${toned(Tone.Warning)(base)})"
          case Some(Block.Note.Style.Highlight) => e"${Bg(theme.selection)}($base)"
          case Some(Block.Note.Style.Param)     => e"$Italic($base)"
          case None                             => base
        pieces += styled
      }

      offset = end

    joined(pieces.to(List))

  private def table
    ( columns: List[Block.Column], rows: List[Block.Row], caption: Optional[List[Inline]],
      width: Int, selected: Optional[Action] )
  :   List[Teletype] =

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
            decorate = { (row: Block.Row) =>
              if row.action.present && row.action == selected
              then ((line: Teletype) => e"${Bg(theme.selection)}($line)"): Optional[Teletype -> Teletype]
              else row.tone.let { tone => (line: Teletype) => toned(tone)(line) } } ):
          row => row.cells.at(index).lay(blank) { cell => phrase(cell.content) }

    val grid = Scaffold[Block.Row](definitions*).tabulate(rows).grid(width.max(4))
    val lines = List.from(grid.render.stdlib)
    caption.lay(lines) { caption => lines :+ faint(e"$Italic(${phrase(caption)})") }

  private def chart(kind: Block.Chart.Kind, series: List[Block.Series], width: Int): List[Teletype] =
    val labelWidth = series.map(_.label).map(phrase).map(_.length).stdlib.maxOption.getOrElse(0)
    val room = (width - labelWidth - 2).max(4)
    val most = series.stdlib.flatMap(_.values.stdlib).maxOption.getOrElse(1.0).max(1e-9)

    def label(series0: Block.Series): Teletype =
      val text = phrase(series0.label).pad(labelWidth)
      series0.tone.lay(text)(toned(_)(text)).append(Teletype(t"  "))

    kind match
      case Block.Chart.Kind.Sparkline =>
        series.map { (series0: Block.Series) =>
          label(series0).append(gaugeLine(Sequence.from(series0.values.stdlib.toVector), room))
        }

      case Block.Chart.Kind.Bars | Block.Chart.Kind.Histogram =>
        val fill = if glyphs == Gaugeable.Glyphs.Ascii then t"#" else t"█"

        series.stdlib.flatMap { series0 =>
          series0.values.stdlib.zipWithIndex.map { (value, index) =>
            val cells = ((value/most)*(room - 8)).toInt.max(if value > 0 then 1 else 0)
            val bar = series0.tone.lay(toned(Tone.Accent)(Teletype(fill*cells)))(toned(_)(Teletype(fill*cells)))
            val prefix = if index == 0 then label(series0) else Teletype(t" "*(labelWidth + 2))
            prefix.append(bar).append(faint(Teletype(t" ${figure(value)}")))
          }
        } .to(List)

  private def gauge(status: Status, caption: Optional[List[Inline]], width: Int, tick: Tick)
  :   List[Teletype] =

    val text: Optional[Text] = caption.let(Inline.plain(_))

    inline def line[status: Gaugeable](value: status): List[Teletype] =
      text.lay(List(gaugeLine(value, width, tick))) { caption => List(gaugeLine(Captioned(value, caption), width, tick)) }

    status match
      case Status.Fraction(value)        => line(Fraction(value))
      case Status.Indeterminate()        => line(Fraction.indeterminate)
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
          Sequence.from:
            steps.stdlib.map { step =>
              val standing = step.standing match
                case Standing.Pending   => ultimatum.Standing.Pending
                case Standing.Running   => ultimatum.Standing.Running
                case Standing.Succeeded => ultimatum.Standing.Succeeded
                case Standing.Failed    => ultimatum.Standing.Failed
                case Standing.Warned    => ultimatum.Standing.Warned
                case Standing.Skipped   => ultimatum.Standing.Skipped

              ultimatum.Step(Inline.plain(step.name), standing, step.detail.let(Inline.plain(_)))
            } .toVector

        val rows = gaugeRows(mapped, width, tick)
        text.lay(rows) { caption => bold(Teletype(caption)) :: rows }

  // Plain text, for logs and machine-adjacent output.
  def plain(blocks0: List[Block], width: Int): Text =
    blocks(blocks0, width).map(_.plain).join(t"\n")
