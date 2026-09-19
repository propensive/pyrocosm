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

// Excluded from the umbrella: `Glyph` (phoenicia), which would outrank this package's own
// definitions, since a wildcard import beats a package member declared in another file. Excluded
// too: `Em` (cataclysm), `Span` (denominative), so the name is the HTML element `htmlDoms`
// supplies.
import soundness.{Em as _, Glyph as _, Span as _, *}

import murmuration.zip
import sortingAlgorithms.timsort
import dysasymptotics.{linearAccess, linearSize}
import attributives.textAttributive
import htmlDoms.whatwg
import htmlDoms.whatwg.*
import nomenclature.CssClass.nominative

// The static half of the web frontend: phrasing to phrasing content, a block to flow content.
// Everything is semantic HTML carrying `pyro-*` classes for the stylesheet, each naming what an
// element is or a state it is in, and nothing carries a `style` attribute: appearance is the
// stylesheet's, so a theme is a `:root` block and a restyling is another sheet. Elements a
// user can act upon carry the handle's id, which is what the browser sends back.
object HtmlRenderer:
  // The class of a tone, `pyro-tone-success` and so on.
  def toneClass(tone: Tone): Name[CssClass] = unsafely(Name[CssClass](t"pyro-tone-${tone.toString.tt.lower}"))
  def accentClass(accent: Token.Accent): Name[CssClass] = unsafely(Name[CssClass](t"pyro-accent-${accent.toString.tt.lower}"))
  def cls(name: Text): Name[CssClass] = unsafely(Name[CssClass](name))

  // The DOM id of a panel or a handle: what a patch replaces and what an event names.
  def panelId(panel: Panel): Text = t"pyro-panel-${panel.id.label}"

  // A class list, omitted when it is empty: honeycomb's own writes `class=""`.
  given classes: List[Name[CssClass]] is Attributive to Whatwg.CssClassList =
    (key, value) => if value.nil then Unset else (key, value.join(t" "))

class HtmlRenderer():
  import HtmlRenderer.{toneClass, accentClass, cls, classes}

  def phrase(content: List[Inline]): Html of Phrasing = Fragment(content.map(inline1)*)

  def token(token: Token): Html of Phrasing =
    val classes: List[Name[CssClass]] =
      val extra: List[Name[CssClass]] = if token.role == Token.Role.Binding then List(cls(t"pyro-binding")) else Nil
      accentClass(token.accent) :: extra
    Span(`class` = classes)(token.text)

  def tokens(tokens: List[Token]): Html of Phrasing = Fragment(tokens.map(token)*)

  private def inline1(node: Inline): Html of Phrasing = node match
    case Inline.Textual(text)         => text
    case Inline.Phrase(content)       => phrase(content)
    case Inline.Emphasis(content)     => Em(phrase(content))
    case Inline.Strong(content)       => Strong(phrase(content))
    case Inline.Toned(tone, content)  => Span(`class` = toneClass(tone))(phrase(content))
    case Inline.Code(_, tokens0)      => Code(`class` = cls(t"pyro-code"))(tokens(tokens0))
    case Inline.Keystroke(keypress)   => Kbd(`class` = cls(t"pyro-key"))(keyText(keypress))
    case Inline.Reference(id)         => Code(`class` = cls(t"pyro-reference"))(id)
    case Inline.Break()               => Br
    case Inline.Symbol(glyph)         => Span(`class` = List(cls(t"pyro-glyph"), cls(t"pyro-glyph-${glyph.toString.tt.lower}")))(glyphText(glyph))
    case Inline.Math(math)            => Span(`class` = cls(t"pyro-math"))(safely(Ergo.serialize(math)).or(t"…"))

    case Inline.Link(destination, content) => destination match
      case Inline.Destination.External(url)   => A(href = url, `class` = cls(t"pyro-link"))(phrase(content))
      case Inline.Destination.Internal(action) => A(href = t"#", id = action.id, `class` = List(cls(t"pyro-link"), cls(t"pyro-action")))(phrase(content))

    case Inline.Amount(value, units) =>
      val (number, unit) = Amounts.scaled(value, units)
      Span(`class` = cls(t"pyro-amount"), title = t"${value.toString} $units")
        ( Span(`class` = cls(t"pyro-figure"))(number), t" ", Span(`class` = cls(t"pyro-units"))(unit) )

    case Inline.Figure(value, precision) =>
      Span(`class` = cls(t"pyro-figure"))(Amounts.figure(value, precision))

  // A keypress without the brackets clavichord's rendering puts around each key: a keycap is
  // already a box, so `[⌃]+[C]` reads as `⌃+C`.
  private def keyText(keypress: Keypress): Text = keypress.show.s.replace("[", "").nn.replace("]", "").nn.tt

  private def glyphText(glyph: Glyph): Text = glyph match
    case Glyph.Check      => t"✓"
    case Glyph.Cross      => t"✗"
    case Glyph.Warning    => t"⚠"
    case Glyph.Running    => t"▶"
    case Glyph.Pending    => t"·"
    case Glyph.Skipped    => t"‑"
    case Glyph.Bullet     => t"•"
    case Glyph.ArrowRight => t"→"
    case Glyph.ArrowUp    => t"↑"
    case Glyph.ArrowDown  => t"↓"
    case Glyph.Star       => t"★"
    case Glyph.Ellipsis   => t"…"
    case Glyph.Winner     => t"★"

  // ── Blocks ────────────────────────────────────────────────────────────────────────────────

  def blocks(blocks: List[Block]): Html of Flow = Fragment(blocks.map(block)*)

  def block(block: Block): Html of Flow = block match
    case Block.Paragraph(content) => P(phrase(content))

    case Block.Heading(level, content) => level match
      case 1 => H1(phrase(content))
      case 2 => H2(phrase(content))
      case 3 => H3(phrase(content))
      case _ => H4(phrase(content))

    case Block.Listing(ordered, items) =>
      val entries = items.map(item)
      if ordered then Ol(`class` = cls(t"pyro-listing"))(entries*) else Ul(`class` = cls(t"pyro-listing"))(entries*)

    case Block.Quotation(content) => Blockquote(`class` = cls(t"pyro-quotation"))(blocks(content))
    case Block.Rule(side)         => Hr(`class` = cls(t"pyro-rule-${side.toString.tt.lower}"))

    case Block.Code(language, lines, notes) =>
      Pre(`class` = List(cls(t"pyro-codeblock"), cls(t"pyro-language-${language.name}")))
        (Code(lines.indexed.map { (line: Block.Line, index: Ordinal) => codeLine(line, notes.filter(_.line == index.n0), index.n0 == lines.size - 1) }*))

    case Block.Table(columns, rows, caption) => table(columns, rows, caption)

    case Block.Record(entries, title) =>
      val list = Dl(`class` = cls(t"pyro-record"))(entries.bind { (entry: Block.Entry) => List[Html of "dt" | "dd"](Dt(phrase(entry.key)), Dd(blocks(entry.value))) }*)
      title.lay(list: Html of Flow) { title => Section(`class` = cls(t"pyro-record-section"))(H4(phrase(title)), list) }

    case Block.Notice(tone, title, content) =>
      Aside(`class` = List(cls(t"pyro-notice"), toneClass(tone)))
        (Fragment[Flow](title.lay(Fragment[Flow]()) { title => Header(Strong(phrase(title))) }, blocks(content)))

    case Block.Disclosure(summary, content, open) =>
      if open then Details(`class` = cls(t"pyro-disclosure"), open = true)(Summary(phrase(summary)), blocks(content))
      else Details(`class` = cls(t"pyro-disclosure"))(Summary(phrase(summary)), blocks(content))

    case Block.Image(source, alt) => Figure(`class` = cls(t"pyro-image"))(Img(src = source, alt = alt), Figcaption(alt))

    // A figure's drawing, its ids qualified by the figure's so several drawings share a page,
    // is read as foreign (SVG) content into a holder, `<figure id>-svg`, which a redraw replaces
    // whole and a part revision patches inside. Markup that does not parse shows its description.
    case Block.Figure(figure) =>
      val markup: Text = pyrocosm.Figure.namespace(figure.id, figure.svg)
      val drawing: Optional[Html of "svg"] = safely(markup.read[Html of "svg"])

      val holder: Html of Flow =
        drawing.lay(Div(id = t"${figure.id}-svg", `class` = cls(t"pyro-drawing-holder"))(P(phrase(figure.alt)))):
          svg => Div(id = t"${figure.id}-svg", `class` = cls(t"pyro-drawing-holder"))(svg)

      Figure(id = figure.id, `class` = cls(t"pyro-drawing"))(holder, Figcaption(phrase(figure.alt)))

    case Block.Tree(roots) => Ul(`class` = cls(t"pyro-tree"))(roots.map(treeNode)*)

    // A graph as its vertices, each with what it depends on; a drawn layout is later work.
    case Block.Graph(vertices, edges) =>
      val labels: Map[Text, List[Inline]] = vertices.map { (vertex: Block.Vertex) => vertex.id -> vertex.label }.to[Map]

      Ul(`class` = cls(t"pyro-graph"))(vertices.map { (vertex: Block.Vertex) =>
          val dependencies = edges.filter(_.from == vertex.id).map { (edge: Block.Edge) => labels(edge.to).or(Inline.text(edge.to)) }
          val label = vertexLabel(vertex)
          if dependencies.nil then Li(label)
          else Li(label, Span(`class` = cls(t"pyro-arrow"))(t" → "), Fragment(dependencies.indexed.map { (target, index) =>
            Fragment[Phrasing](if index.n0 == 0 then Fragment[Phrasing]() else t", ", phrase(target)) }*))
        }*)

    case Block.Chart(kind, series) => chart(kind, series)
    case Block.Gauge(status, caption) => gauge(status, caption)
    case Block.Group(content) => Div(`class` = cls(t"pyro-group"))(blocks(content))

    // Captured output, verbatim, each line behind a gutter naming its stream.
    case Block.Output(text, error) =>
      val gutter = cls(t"pyro-gutter")
      val lines: List[Text] = text.cut(t"\n")
      val trimmed: List[Text] = if lines.last == t"" then lines.keep(lines.size - 1) else lines
      val count = trimmed.size
      val rows: List[Html of Phrasing] = trimmed.indexed.map: (line: Text, index: Ordinal) =>
        val text: Text = if index.n0 == count - 1 then line else t"$line\n"
        Fragment[Phrasing](Span(`class` = gutter)(t"░ "), text)

      Pre(`class` = List(cls(t"pyro-output"), if error then cls(t"pyro-output-stderr") else cls(t"pyro-output-stdout")))(rows*)

  private def item(item: Block.Item): Html of "li" =
    item.action.lay(Li(blocks(item.content))) { action => Li(id = action.id, `class` = cls(t"pyro-action"))(blocks(item.content)) }

  private def treeNode(node: Block.TreeNode): Html of "li" =
    val label: Html of Phrasing = node.tone.lay(phrase(node.label)) { tone => Span(`class` = toneClass(tone))(phrase(node.label)) }
    val children: Html of Flow = if node.children.nil then Fragment[Flow]() else Ul(node.children.map(treeNode)*)
    node.action.lay(Li(label, children)) { action => Li(id = action.id, `class` = cls(t"pyro-action"))(label, children) }

  private def vertexLabel(vertex: Block.Vertex): Html of Phrasing =
    val label: Html of Phrasing = vertex.tone.lay(phrase(vertex.label)) { tone => Span(`class` = toneClass(tone))(phrase(vertex.label)) }
    vertex.action.lay(label) { action => A(href = t"#", id = action.id, `class` = cls(t"pyro-action"))(label) }

  def codeLine(line: Block.Line, notes: List[Block.Note], last: Boolean): Html of Phrasing =
    // Each token, cut at the boundaries where a note begins or ends inside it, with a running
    // offset into the line.
    var offset: Int = 0
    var pieces: List[Html of Phrasing] = Nil

    line.tokens.each: (token: Token) =>
      val end = offset + token.text.length
      val ends: List[Int] = notes.bind { (note: Block.Note) => List(note.start, note.end) }
      val cuts: List[Int] = (offset :: end :: ends).filter { (cut: Int) => cut >= offset && cut <= end }.distinct.sort

      cuts.zip(cuts.tail).each: (from: Int, to: Int) =>
        val text = token.text.s.substring(from - offset, to - offset).nn.tt
        val base = this.token(token.copy(text = text))

        val styled: Html of Phrasing =
          notes.seek { (note: Block.Note) => note.start <= from && note.end >= to }.lay(base): (note: Block.Note) =>
            Span(`class` = cls(t"pyro-note-${note.style.toString.tt.lower}"), title = note.caption.or(t""))(base)

        pieces = styled :: pieces

      offset = end

    val ordered: List[Html of Phrasing] = pieces.reverse

    Span(`class` = cls(t"pyro-line"))(Fragment(ordered*), if last then Fragment[Phrasing]() else t"\n")

  private def table(columns: List[Block.Column], rows: List[Block.Row], caption: Optional[List[Inline]]): Html of Flow =
    def columnClasses(column: Block.Column): List[Name[CssClass]] =
      val alignment: List[Name[CssClass]] = if column.alignment == Block.Alignment.Start then Nil else List(cls(t"pyro-align-${column.alignment.toString.tt.lower}"))
      val numeric: List[Name[CssClass]] = if column.numeric then List(cls(t"pyro-numeric")) else Nil

      val sizing: List[Name[CssClass]] = column.sizing match
        case Block.Sizing.Stretch        => List(cls(t"pyro-stretch"))
        case Block.Sizing.Rigid          => List(cls(t"pyro-rigid"))
        case Block.Sizing.Paragraph      => Nil
        case Block.Sizing.Collapsible(_) => List(cls(t"pyro-collapsible"))

      alignment + numeric + sizing

    val head = Thead(Tr(columns.map { (column: Block.Column) => Th(`class` = columnClasses(column))(phrase(column.title)) }*))

    val body = Tbody(rows.map { (row: Block.Row) =>
        val cells = columns.indexed.map { (column, index) =>
          Td(`class` = columnClasses(column))(row.cells.at(index).lay(Fragment[Phrasing]()) { cell => phrase(cell.content) })
        }
        val classes: List[Name[CssClass]] = row.tone.lay(Nil: List[Name[CssClass]]) { tone => List(toneClass(tone)) }
        row.action.lay(Tr(`class` = classes)(cells*)) { action => Tr(id = action.id, `class` = cls(t"pyro-action") :: classes)(cells*) }
      }*)

    val captioned: List[Html of "caption"] = caption.lay(Nil: List[Html of "caption"]) { caption => List(Caption(phrase(caption))) }
    Table(`class` = cls(t"pyro-table"))(Fragment(captioned*), head, body)

  // A chart is a figure holding a description list: each series is a term, and each of its
  // values a description. A bar or a histogram column is a `meter` of the value against the
  // largest, so its length is the browser's to draw; a sparkline is a run of block glyphs.
  private def chart(kind: Block.Chart.Kind, series: List[Block.Series]): Html of Flow =
    val most = series.bind(_.values).maximum.or(1.0).max(1e-9)
    val classes = List(cls(t"pyro-chart"), cls(t"pyro-chart-${kind.toString.tt.lower}"))

    type Entry = Html of "dt" | "dd"

    def values(series0: Block.Series): List[Entry] = kind match
      case Block.Chart.Kind.Sparkline =>
        val glyphs = t"▁▂▃▄▅▆▇█"
        val cells = series0.values.map { (value: Double) => glyphs.s.charAt(((value/most)*7).toInt.max(0).min(7)).toString.tt }.join
        List[Entry](Dd(`class` = cls(t"pyro-series-value"))(Span(`class` = cls(t"pyro-sparkline"))(cells)))

      case Block.Chart.Kind.Bars | Block.Chart.Kind.Histogram =>
        series0.values.map { (value: Double) =>
          val figure = Amounts.figure(value)
          val entry: Entry =
            Dd(`class` = cls(t"pyro-series-value"))
              ( Meter(`class` = cls(t"pyro-meter"), value = value, max = most)(figure), t" ",
                Span(`class` = cls(t"pyro-figure"))(figure) )
          entry
        }

    val entries: List[Entry] = series.bind { (series0: Block.Series) =>
      val label: Entry = Dt(`class` = cls(t"pyro-series-label"))(phrase(series0.label))
      label :: values(series0) }

    Figure(`class` = classes)(Dl(`class` = cls(t"pyro-series"))(entries*))

  // A gauge is a figure, its caption the figure's; a duration is a `time` with its
  // machine-readable form; a count without a total is a `data` element carrying it.
  private def gauge(status: Status, caption: Optional[List[Inline]]): Html of Flow =
    def duration(seconds: Double): Text = t"PT${Amounts.figure(seconds, 3)}S"

    // A scalar status (a standing, a duration, a count) names its kind on the figure, so the
    // stylesheet can set it on one line with its caption; a bar or a list stays a block.
    val classes: List[Name[CssClass]] = status match
      case Status.Standing(_)          => List(cls(t"pyro-gauge"), cls(t"pyro-gauge-standing"))
      case Status.Elapsed(_)           => List(cls(t"pyro-gauge"), cls(t"pyro-gauge-elapsed"))
      case Status.Remaining(_)         => List(cls(t"pyro-gauge"), cls(t"pyro-gauge-remaining"))
      case Status.Reckoning(_, Unset)  => List(cls(t"pyro-gauge"), cls(t"pyro-gauge-count"))
      case _                           => List(cls(t"pyro-gauge"))

    val body: Html of Flow = status match
      case Status.Fraction(value)        => Progress(`class` = cls(t"pyro-progress"), value = t"${(value*1000).toInt}", max = t"1000")(t"")
      case Status.Indeterminate()        => Progress(`class` = cls(t"pyro-progress"))(t"")
      case Status.Reckoning(done, total) =>
        total.lay[Html of Flow](whatwg.Data(`class` = cls(t"pyro-reckoning"), value = done.toString.tt)(done.toString.tt)): total =>
          Div(`class` = cls(t"pyro-reckoning"))(Progress(value = t"${done.toString}", max = t"${total.toString}")(t""), Span(t" ${done.toString}/${total.toString}"))
      case Status.Standing(standing)     => Span(`class` = List(cls(t"pyro-standing"), cls(t"pyro-standing-${standing.toString.tt.lower}")))(standingGlyph(standing))
      case Status.Elapsed(seconds)       => Time(`class` = cls(t"pyro-elapsed"), datetime = duration(seconds))(Amounts.scaled(seconds, t"s") match { case (n, u) => t"$n $u" })
      case Status.Remaining(seconds)     => Time(`class` = cls(t"pyro-remaining"), datetime = duration(seconds))(Amounts.scaled(seconds, t"s") match { case (n, u) => t"$n $u left" })
      case Status.Steps(steps) =>
        Ol(`class` = cls(t"pyro-steps"))(steps.map { (step: Step) =>
          Li(`class` = cls(t"pyro-standing-${step.standing.toString.tt.lower}"))(Span(`class` = cls(t"pyro-standing"))(standingGlyph(step.standing)), t" ", phrase(step.name))
        }*)

    caption.lay(Figure(`class` = classes)(body)): caption =>
      Figure(`class` = classes)(Figcaption(phrase(caption)), body)

  private def standingGlyph(standing: Standing): Text = standing match
    case Standing.Pending   => t"·"
    case Standing.Running   => t"▶"
    case Standing.Succeeded => t"✓"
    case Standing.Failed    => t"✗"
    case Standing.Warned    => t"⚠"
    case Standing.Skipped   => t"‑"
