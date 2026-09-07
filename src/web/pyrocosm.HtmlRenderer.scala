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
import archimedes.*
import contingency.*
import denominative.{Span as _, *}
import gossamer.*
import honeycomb.*
import nomenclature.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

import denominative.dysasymptotics.{linearAccess, linearSize}
import honeycomb.attributives.textAttributive
import htmlDoms.whatwg.*
import nomenclature.CssClass.nominative

// The static half of the web frontend: phrasing to phrasing content, a block to flow content.
// Everything is semantic HTML carrying `pyro-*` classes for the stylesheet, and nothing carries
// an inline colour: appearance is the stylesheet's, so a theme is a `:root` block. Elements a
// user can act upon carry the handle's id, which is what the browser sends back.
object HtmlRenderer:
  // The class of a tone, `pyro-tone-success` and so on.
  def toneClass(tone: Tone): Name[CssClass] = unsafely(Name[CssClass](t"pyro-tone-${tone.toString.tt.lower}"))
  def accentClass(accent: Token.Accent): Name[CssClass] = unsafely(Name[CssClass](t"pyro-accent-${accent.toString.tt.lower}"))
  def cls(name: Text): Name[CssClass] = unsafely(Name[CssClass](name))

  // The DOM id of a panel or a handle: what a patch replaces and what an event names.
  def panelId(panel: Panel): Text = t"pyro-panel-${panel.id.label}"

class HtmlRenderer():
  import HtmlRenderer.{toneClass, accentClass, cls}

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
    case Inline.Toned(tone, content)  => Span(`class` = List(cls(t"pyro-toned"), toneClass(tone)))(phrase(content))
    case Inline.Code(_, tokens0)      => Code(`class` = cls(t"pyro-code"))(tokens(tokens0))
    case Inline.Keystroke(keypress)   => Kbd(`class` = cls(t"pyro-key"))(keypress.show)
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
    case Block.Rule()             => Hr

    case Block.Code(language, lines, notes) =>
      Pre(`class` = List(cls(t"pyro-codeblock"), cls(t"pyro-language-${language.name}")))
        (Code(lines.indexed.map { (line, index) => codeLine(line, notes.filter(_.line == index.n0), index.n0 == lines.stdlib.length - 1) }*))

    case Block.Table(columns, rows, caption) => table(columns, rows, caption)

    case Block.Record(entries, title) =>
      val list = Dl(`class` = cls(t"pyro-record"))(entries.bind { (entry: Block.Entry) => List[Html of "dt" | "dd"](Dt(phrase(entry.key)), Dd(blocks(entry.value))) }*)
      title.lay(list: Html of Flow) { title => Section(`class` = cls(t"pyro-record-section"))(H4(phrase(title)), list) }

    case Block.Notice(tone, title, content) =>
      Aside(`class` = List(cls(t"pyro-notice"), toneClass(tone)))
        (Fragment[Flow](title.lay(Fragment[Flow]()) { title => Header(Strong(phrase(title))) }, blocks(content)))

    case Block.Disclosure(summary, content, open) =>
      if open then Details(`class` = cls(t"pyro-disclosure"), open = t"")(Summary(phrase(summary)), blocks(content))
      else Details(`class` = cls(t"pyro-disclosure"))(Summary(phrase(summary)), blocks(content))

    case Block.Image(source, alt) => Figure(`class` = cls(t"pyro-image"))(Img(src = source, alt = alt), Figcaption(alt))

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

  private def item(item: Block.Item): Html of "li" =
    item.action.lay(Li(blocks(item.content))) { action => Li(id = action.id, `class` = cls(t"pyro-action"))(blocks(item.content)) }

  private def treeNode(node: Block.TreeNode): Html of "li" =
    val label: Html of Phrasing = node.tone.lay(phrase(node.label)) { tone => Span(`class` = toneClass(tone))(phrase(node.label)) }
    val children: Html of Flow = if node.children.nil then Fragment[Flow]() else Ul(node.children.map(treeNode)*)
    node.action.lay(Li(label, children)) { action => Li(id = action.id, `class` = cls(t"pyro-action"))(label, children) }

  private def vertexLabel(vertex: Block.Vertex): Html of Phrasing =
    val label: Html of Phrasing = vertex.tone.lay(phrase(vertex.label)) { tone => Span(`class` = toneClass(tone))(phrase(vertex.label)) }
    vertex.action.lay(label) { action => A(href = t"#", id = action.id, `class` = cls(t"pyro-action"))(label) }

  private def codeLine(line: Block.Line, notes: List[Block.Note], last: Boolean): Html of Phrasing =
    var offset = 0
    val pieces = scala.collection.mutable.ListBuffer[Html of Phrasing]()

    line.tokens.each: token =>
      val end = offset + token.text.length
      val cuts: scala.List[Int] =
        (offset :: end :: notes.stdlib.flatMap { note => scala.List(note.start, note.end) })
        . filter { cut => cut >= offset && cut <= end }.distinct.sorted

      cuts.zip(cuts.tail).foreach { (from, to) =>
        val text = token.text.s.substring(from - offset, to - offset).nn.tt
        val base = this.token(token.copy(text = text))
        val note = notes.stdlib.find { note => note.start <= from && note.end >= to }
        pieces += note.map { note => Span(`class` = cls(t"pyro-note-${note.style.toString.tt.lower}"), title = note.caption.or(t""))(base): Html of Phrasing }.getOrElse(base)
      }

      offset = end

    Span(`class` = cls(t"pyro-line"))(Fragment(pieces.to(List)*), if last then Fragment[Phrasing]() else t"\n")

  private def table(columns: List[Block.Column], rows: List[Block.Row], caption: Optional[List[Inline]]): Html of Flow =
    def columnClasses(column: Block.Column): List[Name[CssClass]] =
      List(cls(t"pyro-align-${column.alignment.toString.tt.lower}"))
        + (if column.numeric then List(cls(t"pyro-numeric")) else Nil)
        + (column.sizing match
            case Block.Sizing.Stretch        => List(cls(t"pyro-stretch"))
            case Block.Sizing.Rigid          => List(cls(t"pyro-rigid"))
            case Block.Sizing.Paragraph      => Nil
            case Block.Sizing.Collapsible(_) => List(cls(t"pyro-collapsible")))

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

  // Bars and histograms as proportionally sized divs; a sparkline as a run of block glyphs.
  private def chart(kind: Block.Chart.Kind, series: List[Block.Series]): Html of Flow =
    val most = series.stdlib.flatMap(_.values.stdlib).maxOption.getOrElse(1.0).max(1e-9)

    kind match
      case Block.Chart.Kind.Sparkline =>
        val glyphs = t"▁▂▃▄▅▆▇█"
        Div(`class` = cls(t"pyro-chart"))(series.map { (series0: Block.Series) =>
          val cells = series0.values.map { (value: Double) => glyphs.s.charAt(((value/most)*7).toInt.max(0).min(7)).toString.tt }.join
          Div(`class` = cls(t"pyro-series"))(Span(`class` = cls(t"pyro-series-label"))(phrase(series0.label)), Span(`class` = cls(t"pyro-sparkline"))(cells))
        }*)

      case Block.Chart.Kind.Bars | Block.Chart.Kind.Histogram =>
        Div(`class` = cls(t"pyro-chart"))(series.map { (series0: Block.Series) =>
          Div(`class` = cls(t"pyro-series"))
            ( Span(`class` = cls(t"pyro-series-label"))(phrase(series0.label)),
              Fragment(series0.values.map { (value: Double) =>
                val percent = ((value/most)*100).toInt
                Div(`class` = cls(t"pyro-bar-row"))(Div(`class` = cls(t"pyro-bar"), style = t"width: $percent%")(t""), Span(`class` = cls(t"pyro-figure"))(Amounts.figure(value)))
              }*) )
        }*)

  private def gauge(status: Status, caption: Optional[List[Inline]]): Html of Flow =
    val label: Html of Flow = caption.lay(Fragment[Flow]()) { caption => Div(`class` = cls(t"pyro-caption"))(phrase(caption)) }

    val body: Html of Flow = status match
      case Status.Fraction(value)        => Progress(`class` = cls(t"pyro-progress"), value = t"${(value*1000).toInt}", max = t"1000")(t"")
      case Status.Indeterminate()        => Progress(`class` = cls(t"pyro-progress"))(t"")
      case Status.Reckoning(done, total) =>
        total.lay[Html of Flow](Span(`class` = cls(t"pyro-reckoning"))(t"${done.toString}")): total =>
          Div(`class` = cls(t"pyro-reckoning"))(Progress(value = t"${done.toString}", max = t"${total.toString}")(t""), Span(t" ${done.toString}/${total.toString}"))
      case Status.Standing(standing)     => Span(`class` = List(cls(t"pyro-standing"), cls(t"pyro-standing-${standing.toString.tt.lower}")))(standingGlyph(standing))
      case Status.Elapsed(seconds)       => Span(`class` = cls(t"pyro-elapsed"))(Amounts.scaled(seconds, t"s") match { case (n, u) => t"$n $u" })
      case Status.Remaining(seconds)     => Span(`class` = cls(t"pyro-remaining"))(Amounts.scaled(seconds, t"s") match { case (n, u) => t"$n $u left" })
      case Status.Steps(steps) =>
        Ol(`class` = cls(t"pyro-steps"))(steps.map { (step: Step) =>
          Li(`class` = cls(t"pyro-standing-${step.standing.toString.tt.lower}"))(Span(`class` = cls(t"pyro-standing"))(standingGlyph(step.standing)), t" ", phrase(step.name))
        }*)

    Div(`class` = cls(t"pyro-gauge"))(label, body)

  private def standingGlyph(standing: Standing): Text = standing match
    case Standing.Pending   => t"·"
    case Standing.Running   => t"▶"
    case Standing.Succeeded => t"✓"
    case Standing.Failed    => t"✗"
    case Standing.Warned    => t"⚠"
    case Standing.Skipped   => t"‑"
