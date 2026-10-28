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
import soundness.{Em as _, Origin as _, Span as _, *}

import sortingAlgorithms.timsort
import dysasymptotics.{linearAccess, linearSize}
import attributives.textAttributive
import htmlDoms.whatwg
import htmlDoms.whatwg.*
import nomenclature.CssClass.nominative
import textMetrics.uniformMetric

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

  // `data-node="0.2.1"` on a collapsible tree node: its path within its tree, by which the
  // script keeps the open state a reader gave it across repaints of its panel.
  given nodePath: ("data-node" is Attribute of Whatwg.Textual in Whatwg) = Whatwg.globalAttribute()

  // A class list, omitted when it is empty: honeycomb's own writes `class=""`.
  given classes: List[Name[CssClass]] is Attributive to Whatwg.CssClassList =
    (key, value) => if value.nil then Unset else (key, value.join(t" "))

  // `data-action="…"` on an activity's card: the action its press sends. The card has an id of
  // its own, as the same action may belong to a navigation item already on the page.
  given actionId: ("data-action" is Attribute of Whatwg.Textual in Whatwg) = Whatwg.globalAttribute()

  // `data-atom="a12"` on a token of an animated code block: its identity across the frames,
  // by which the script moves it from where it was to where it is.
  given atomId: ("data-atom" is Attribute of Whatwg.Textual in Whatwg) = Whatwg.globalAttribute()

  // `tabindex` on an animated block, so that it takes the arrow keys when focused.
  given tabIndex: ("tabindex" is Attribute of Whatwg.Textual in Whatwg) = Whatwg.globalAttribute()

  // A text as a JSON string literal, for the animation the renderer embeds.
  def jsonText(text: Text): Text =
    val builder = StringBuilder()
    builder.append('"')

    var index: Int = 0

    while index < text.s.length do
      val char: Char = text.s.charAt(index)

      char match
        case '"'  => builder.append("\\\"")
        case '\\' => builder.append("\\\\")
        case '\n' => builder.append("\\n")
        case '\r' => builder.append("\\r")
        case '\t' => builder.append("\\t")
        case char if char < ' ' => builder.append(f"\\u${char.toInt}%04x")
        case char => builder.append(char)

      index += 1

    builder.append('"')
    builder.toString.tt

  // `data-local="true"` on a menu link to a tool of this machine, whose `localhost` the script
  // replaces with the page's own host when the page is viewed from another.
  given localLink: ("data-local" is Attribute of Whatwg.Textual in Whatwg) = Whatwg.globalAttribute()

class HtmlRenderer():
  import HtmlRenderer.{toneClass, accentClass, cls, classes, nodePath, actionId, atomId, tabIndex, localLink, jsonText}

  def phrase(content: List[Inline]): Html of Phrasing = Html.Fragment(content.map(inline1)*)

  // A linked token is an anchor carrying the token's own classes, so that it stays one element
  // and the stylesheet and the editor see it as any other token.
  def token(token: Token): Html of Phrasing =
    val classes: List[Name[CssClass]] =
      val extra: List[Name[CssClass]] = if token.role == Token.Role.Binding then List(cls(t"pyro-binding")) else Nil
      accentClass(token.accent) :: extra

    token.link.lay[Html of Phrasing](Span(`class` = classes)(token.text)): (link: Inline.Destination) =>
      link match
        case Inline.Destination.External(url)    => A(href = url, `class` = classes)(token.text)
        case Inline.Destination.Internal(action) => A(href = t"#", id = action.id, `class` = classes :+ cls(t"pyro-action"))(token.text)
        case local: Inline.Destination.Local     => A(href = href(local), `class` = classes :+ cls(t"pyro-locate"))(token.text)

  // The `href` of a local destination: its path, and its fragment. The one place a prefix would
  // be put before every page of a tool, were several tools to share an origin.
  private def href(local: Inline.Destination.Local): Text =
    local.fragment.lay(local.path) { fragment => t"${local.path}#$fragment" }

  // A label which leads somewhere: an anchor around it for a local or external destination, an
  // action's anchor for an internal one.
  private def lead(destination: Inline.Destination, label: Html of Phrasing): Html of Phrasing = destination match
    case Inline.Destination.External(url)    => A(href = url, `class` = cls(t"pyro-link"))(label)
    case Inline.Destination.Internal(action) => A(href = t"#", id = action.id, `class` = List(cls(t"pyro-link"), cls(t"pyro-action")))(label)
    case local: Inline.Destination.Local     => A(href = href(local), `class` = List(cls(t"pyro-link"), cls(t"pyro-locate")))(label)

  def tokens(tokens: List[Token]): Html of Phrasing =
    Html.Fragment(qualified(tokens.map { (token: Token) => (token, this.token(token)) })*)

  // Each run of chained tokens as Amok rendered a qualified name: every linked segment after the
  // first wraps everything before it, its separators and itself in a `pyro-qualified` span, and
  // the first is wrapped alone, so that each link is a direct child of exactly the span which
  // holds the name up to it — which the stylesheet underlines while that link is pointed at:
  // `com`, then `com.example`, then `com.example.Foo`. A run of one token is the token alone.
  private def qualified(items: List[(Token, Html of Phrasing)]): List[Html of Phrasing] =
    val qualifiedClass: Name[CssClass] = cls(t"pyro-qualified")
    var done: List[Html of Phrasing] = Nil
    var current: Optional[Html of Phrasing] = Unset
    var first: Optional[(Token, Html of Phrasing)] = Unset
    var pending: List[Html of Phrasing] = Nil

    def flush(): Unit =
      first.let: (_, html) => done = html :: done
      current.let: (html: Html of Phrasing) => done = html :: done
      done = pending + done
      first = Unset
      current = Unset
      pending = Nil

    items.each: (token: Token, html: Html of Phrasing) =>
      if !token.chained || (first.absent && current.absent) then
        flush()
        first = (token, html)
      else if token.link.absent then
        pending = html :: pending
      else
        val before: Html of Phrasing = current.or:
          first.lay(Html.Fragment[Phrasing]()): (token0: Token, html0: Html of Phrasing) =>
            if token0.link.present then Span(`class` = qualifiedClass)(html0) else html0

        first = Unset
        current = Span(`class` = qualifiedClass)(before, Html.Fragment(pending.reverse*), html)
        pending = Nil

    flush()
    done.reverse

  private def inline1(node: Inline): Html of Phrasing = node match
    case Inline.Textual(text)         => text
    case Inline.Phrase(content)       => phrase(content)
    case Inline.Emphasis(content)     => Em(phrase(content))
    case Inline.Strong(content)       => Strong(phrase(content))
    case Inline.Toned(tone, content)  => Span(`class` = toneClass(tone))(phrase(content))
    case Inline.Snippet(_, tokens0)      => Code(`class` = cls(t"pyro-code"))(tokens(tokens0))
    case Inline.Keystroke(keypress)   => Kbd(`class` = cls(t"pyro-key"))(keyText(keypress))
    case Inline.Reference(id)         => Code(`class` = cls(t"pyro-reference"))(id)
    case Inline.Break()               => Br
    case Inline.Symbol(glyph)         => Span(`class` = List(cls(t"pyro-glyph"), cls(t"pyro-glyph-${glyph.toString.tt.lower}")))(glyphText(glyph))
    case Inline.Math(math)            => Span(`class` = cls(t"pyro-math"))(safely(Ergo.serialize(math)).or(t"…"))
    case Inline.Icon(source, alt) =>
      Img(src = source, alt = alt, title = alt, `class` = cls(t"pyro-icon"))

    case Inline.Link(destination, content) => lead(destination, phrase(content))

    // The note is in the element, after the content, so that it is read by what has no hover
    // (a screen reader, a text browser); the stylesheet shows it only when pointed at or
    // focused, which the tabindex allows from the keyboard.
    case Inline.Annotated(content, note) =>
      Span(`class` = cls(t"pyro-annotated"), tabindex = t"0")
        ( phrase(content), Span(`class` = cls(t"pyro-annotation"))(phrase(note)) )

    case Inline.Amount(value, units) =>
      val (number, unit) = Amounts.scaled(value, units)
      Span(`class` = cls(t"pyro-amount"), title = t"${value.toString} $units")
        ( Span(`class` = cls(t"pyro-figure"))(number), t" ", Span(`class` = cls(t"pyro-units"))(unit) )

    case Inline.Numeral(value, precision) =>
      Span(`class` = cls(t"pyro-figure"))(Amounts.figure(value, precision))

  // A keypress without the brackets clavichord's rendering puts around each key: a keycap is
  // already a box, so `[⌃]+[C]` reads as `⌃+C`.
  private def keyText(keypress: Keypress): Text = keypress.show.s.replace("[", "").nn.replace("]", "").nn.tt

  private def glyphText(glyph: Glyph): Text = glyph match
    case Glyph.Check      => t"✓"
    case Glyph.Cross      => t"✗"
    case Glyph.Warning    => t"△"
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

  def blocks(blocks: List[Block]): Html of Flow = Html.Fragment(blocks.map(block)*)

  def block(block: Block): Html of Flow = block match
    case Block.Paragraph(content) => P(phrase(content))

    case Block.Heading(level, content, id) => id.lay[Html of Flow](level match
      case 1 => H1(phrase(content))
      case 2 => H2(phrase(content))
      case 3 => H3(phrase(content))
      case _ => H4(phrase(content))):
      id => level match
        case 1 => H1(id = id)(phrase(content))
        case 2 => H2(id = id)(phrase(content))
        case 3 => H3(id = id)(phrase(content))
        case _ => H4(id = id)(phrase(content))

    case Block.Listing(ordered, items) =>
      val entries = items.map(item)
      if ordered then Ol(`class` = cls(t"pyro-listing"))(entries*) else Ul(`class` = cls(t"pyro-listing"))(entries*)

    case Block.Quotation(content) => Blockquote(`class` = cls(t"pyro-quotation"))(blocks(content))
    case Block.Rule(side)         => Hr(`class` = cls(t"pyro-rule-${side.toString.tt.lower}"))

    case Block.Code(language, lines, notes, start, animation, id) =>
      animation.lay(plainCode(language, lines, notes, start, id)): (animation: Block.Animation) =>
        animatedCode(language, animation, id)

    case Block.Table(columns, rows, caption) => table(columns, rows, caption)

    case Block.Record(entries, title) =>
      val list = Dl(`class` = cls(t"pyro-record"))(entries.bind { (entry: Block.Entry) => List[Html of "dt" | "dd"](Dt(phrase(entry.key)), Dd(blocks(entry.value))) }*)
      title.lay(list: Html of Flow) { title => Section(`class` = cls(t"pyro-record-section"))(H4(phrase(title)), list) }

    case Block.Notice(tone, title, content) =>
      Aside(`class` = List(cls(t"pyro-notice"), toneClass(tone)))
        (Html.Fragment[Flow](title.lay(Html.Fragment[Flow]()) { title => Header(Strong(phrase(title))) }, blocks(content)))

    // A diagnostic is an article in its tone: a header naming the place and the severity, the
    // message, the excerpt and whatever follows.
    case Block.Diagnostic(tone, location, message, excerpt, detail) =>
      val glyph: Glyph = tone match
        case Tone.Failure => Glyph.Cross
        case Tone.Warning => Glyph.Warning
        case Tone.Success => Glyph.Check
        case _            => Glyph.Bullet

      val severity: Text = tone match
        case Tone.Failure => t"error"
        case Tone.Warning => t"warning"
        case Tone.Success => t"ok"
        case Tone.Info    => t"info"
        case _            => t"note"

      val where: Html of Phrasing =
        location.lay(Html.Fragment[Phrasing]()) { (location: Block.Location) => Code(`class` = cls(t"pyro-location"))(location.text) }

      val rest: Html of Flow =
        if detail.nil then Html.Fragment[Flow]() else Div(`class` = cls(t"pyro-detail"))(blocks(detail))

      Article(`class` = List(cls(t"pyro-diagnostic"), toneClass(tone)))
        ( Header(Span(`class` = cls(t"pyro-glyph"))(glyphText(glyph)), where, Span(`class` = cls(t"pyro-severity"))(severity)),
          Div(`class` = cls(t"pyro-message"))(blocks(message)),
          blocks(excerpt),
          rest )

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

    case Block.Deck(leaves) =>
      deck(leaves)

    case Block.Tree(roots) =>
      Ul(`class` = cls(t"pyro-tree"))(roots.indexed.map { (root: Block.TreeNode, index: Ordinal) => treeNode(root, t"${index.n0}") }*)

    // A graph as its vertices, each with what it depends on; a drawn layout is later work.
    case Block.Graph(vertices, edges) =>
      val labels: Map[Text, List[Inline]] = vertices.map { (vertex: Block.Vertex) => vertex.id -> vertex.label }.to[Map]

      Ul(`class` = cls(t"pyro-graph"))(vertices.map { (vertex: Block.Vertex) =>
          val dependencies = edges.filter(_.from == vertex.id).map { (edge: Block.Edge) => labels(edge.to).or(Inline.text(edge.to)) }
          val label = vertexLabel(vertex)
          if dependencies.nil then Li(label)
          else Li(label, Span(`class` = cls(t"pyro-arrow"))(t" → "), Html.Fragment(dependencies.indexed.map { (target, index) =>
            Html.Fragment[Phrasing](if index.n0 == 0 then Html.Fragment[Phrasing]() else t", ", phrase(target)) }*))
        }*)

    case Block.Chart(kind, series) => chart(kind, series)
    case Block.Gauge(status, caption) => gauge(status, caption)
    case Block.Trace(stacks) => trace(stacks)
    case Block.Group(content) => Div(`class` = cls(t"pyro-group"))(blocks(content))

    // Captured output, verbatim, each line behind a gutter naming its stream.
    case Block.Output(text, error) =>
      val gutter = cls(t"pyro-gutter")
      val lines: List[Text] = text.cut(t"\n")
      val trimmed: List[Text] = if lines.last == t"" then lines.keep(lines.size - 1) else lines
      val count = trimmed.size
      val rows: List[Html of Phrasing] = trimmed.indexed.map: (line: Text, index: Ordinal) =>
        val text: Text = if index.n0 == count - 1 then line else t"$line\n"
        Html.Fragment[Phrasing](Span(`class` = gutter)(t"░ "), text)

      Pre(`class` = List(cls(t"pyro-output"), if error then cls(t"pyro-output-stderr") else cls(t"pyro-output-stdout")))(rows*)

  // An item with a destination and no action is a real link (which opens in a new tab as a
  // link does): the anchor wraps its one paragraph's phrasing, since an anchor cannot hold
  // blocks; an item of several blocks leads from its first paragraph.
  private def item(item: Block.Item): Html of "li" =
    item.action.lay(item.destination.lay(Li(blocks(item.content))) { destination => Li(`class` = cls(t"pyro-lead"))(leading(destination, item.content)*) }):
      action => Li(id = action.id, `class` = cls(t"pyro-action"))(blocks(item.content))

  private def leading(destination: Inline.Destination, content: List[Block]): List[Html of Flow] = content match
    case Block.Paragraph(phrasing) :: rest => P(lead(destination, phrase(phrasing))) :: rest.map(block)
    case other                             => other.map(block)

  // A collapsible node (one with `open` and children) is a `<details>` whose summary is its
  // label, named by its path for the script; any other node is its label and its children.
  private def treeNode(node: Block.TreeNode, path: Text): Html of "li" =
    val toned: Html of Phrasing = node.tone.lay(phrase(node.label)) { tone => Span(`class` = toneClass(tone))(phrase(node.label)) }
    val label: Html of Phrasing =
      if node.action.present then toned else node.destination.lay(toned) { destination => lead(destination, toned) }
    val children: Html of Flow =
      if node.children.nil then Html.Fragment[Flow]()
      else Ul(node.children.indexed.map { (child: Block.TreeNode, index: Ordinal) => treeNode(child, t"$path.${index.n0}") }*)

    val plain: List[Html of Flow] = List(label, children)

    val content: List[Html of Flow] =
      if node.children.nil then plain
      else node.open.lay(plain): (open: Boolean) =>
        val details: Html of Flow =
          if open then Details(`class` = cls(t"pyro-node"), open = true, `data-node` = path)(Summary(label), children)
          else Details(`class` = cls(t"pyro-node"), `data-node` = path)(Summary(label), children)

        List(details)

    node.action.lay(Li(content*)) { action => Li(id = action.id, `class` = cls(t"pyro-action"))(content*) }

  private def vertexLabel(vertex: Block.Vertex): Html of Phrasing =
    val label: Html of Phrasing = vertex.tone.lay(phrase(vertex.label)) { tone => Span(`class` = toneClass(tone))(phrase(vertex.label)) }
    vertex.action.lay(label) { action => A(href = t"#", id = action.id, `class` = cls(t"pyro-action"))(label) }

  // The rows of a code block, each ended by a newline but the last: every line, behind its
  // number when `start` says which line of its source the first is; and beneath a line, a row
  // for each caret or captioned note ending on it — the caret `╱╲` beneath its position, or `‾`
  // under each marked column, then the caption — spaced into place with the line's own
  // columns, as the terminal draws it, since a `pre` keeps them.
  private def codeRows(lines: List[Block.Line], notes: List[Block.Note], start: Optional[Int]): List[Html of Phrasing] =
    val digits: Int = start.lay(0) { (first: Int) => (first + lines.size - 1).show.length }

    def number(value: Optional[Int]): Html of Phrasing =
      if start.absent then Html.Fragment[Phrasing]()
      else Span(`class` = cls(t"pyro-lineno"))(value.lay(t" "*digits) { (value: Int) => value.show.pad(digits, Rtl) })

    val rows: List[Html of Phrasing] =
      lines.indexed.bind: (line: Block.Line, index: Ordinal) =>
        val length: Int = line.tokens.fold(0) { (total: Int, token: Token) => total + token.text.length }
        Html.Fragment[Phrasing](number(start.let(_ + index.n0)), codeLine(line, notes, true, index.n0))
          :: markers(notes, index.n0, length).map { (note: Block.Note) => marker(note, index.n0, length, number(Unset)) }

    val count: Int = rows.size

    rows.indexed.map: (row: Html of Phrasing, index: Ordinal) =>
      if index.n0 == count - 1 then row else Html.Fragment[Phrasing](row, t"\n")

  // The notes which end on line `index` and are drawn beneath it: a caret, or a captioned note.
  private def markers(notes: List[Block.Note], index: Int, length: Int): List[Block.Note] =
    notes.filter { (note: Block.Note) => note.lastLine == index && (note.caret || !note.caption.nil) }

  // The row beneath a line for one of its notes: the caret `╱╲` beneath its position, or `‾`
  // under each marked column, then the caption, spaced into place with the line's own columns;
  // `margin` is what stands where the line's number does, so the columns align.
  private def marker(note: Block.Note, index: Int, length: Int, margin: Html of Phrasing = Html.Fragment[Phrasing]())
  :   Html of Phrasing =

    val range: (Int, Int) = note.range(index, length).or((0, 0))
    val pad: Int = if note.caret then (range(0) - 1).max(0) else range(0)
    val marks: Text = if note.caret then t"╱╲" else t"‾"*(range(1) - range(0)).max(1)
    val caption: Html of Phrasing = if note.caption.nil then Html.Fragment[Phrasing]() else Html.Fragment[Phrasing](t" ", phrase(note.caption))

    Span(`class` = List(cls(t"pyro-marker"), cls(t"pyro-marker-${note.style.toString.tt.lower}")))
      (margin, t" "*pad, Span(`class` = cls(t"pyro-marks"))(marks), caption)

  def plainCode
    ( language: pyrocosm.Language,
      lines:    List[Block.Line],
      notes:    List[Block.Note],
      start:    Optional[Int],
      id:       Optional[Text] = Unset )
  :   Html of Flow =

    val numbered: List[Name[CssClass]] = if start.present then List(cls(t"pyro-numbered")) else Nil
    val classes: List[Name[CssClass]] = cls(t"pyro-codeblock") :: cls(t"pyro-language-${language.name}") :: numbered
    val code: Html of Phrasing = Code(codeRows(lines, notes, start)*)

    id.lay(Pre(`class` = classes)(code)) { (id: Text) => Pre(id = id, `class` = classes)(code) }

  // An animated block: its first slide as lines of atoms, each carrying its id, with that
  // slide's annotations and label and in its language; its controls; and the animation as
  // JSON in a hidden element the script reads. Slide 0 is what a page without the script
  // shows.
  // A deck: every slide a section, the first current, which is what a page without the script
  // shows; and its controls. The script shows one slide at a time and steps through them.
  def deck(leaves: List[Block.Leaf]): Html of Flow =
    val slides: List[Html of Flow] =
      leaves.indexed.map: (leaf: Block.Leaf, index: Ordinal) =>
        val current: List[Name[CssClass]] =
          if index.n0 == 0 then List(cls(t"pyro-current")) else Nil

        Section(`class` = cls(t"pyro-leaf") :: current)(blocks(leaf.content))

    val controls: Html of Flow =
      Div(`class` = cls(t"pyro-deck-controls"))
        ( Button(`class` = cls(t"pyro-deck-prev"), `type` = t"button")(t"‹"),
          Span(`class` = cls(t"pyro-deck-counter"))(t"1 / ${leaves.size.toString.tt}"),
          Button(`class` = cls(t"pyro-deck-next"), `type` = t"button")(t"›") )

    Div(`class` = cls(t"pyro-deck"), tabindex = t"0")(Html.Fragment(slides*), controls)

  def animatedCode(language: pyrocosm.Language, animation: Block.Animation, id: Optional[Text] = Unset): Html of Flow =
    val first: Optional[Block.Slide] = animation.slides.head
    val shown: pyrocosm.Language = first.let(_.language).or(language)
    val classes: List[Name[CssClass]] = List(cls(t"pyro-codeblock"), cls(t"pyro-language-${shown.name}"))
    val rows: List[Html of Phrasing] = animatedRows(animation.lines(0), first.lay(Nil)(_.notes))
    val frames: Int = animation.slides.size

    val controls: Html of Flow =
      Div(`class` = cls(t"pyro-animation-controls"))
        ( Button(`class` = cls(t"pyro-animation-prev"), `type` = t"button")(t"‹"),
          Span(`class` = cls(t"pyro-animation-counter"))(t"1 / ${frames.toString.tt}"),
          Button(`class` = cls(t"pyro-animation-next"), `type` = t"button")(t"›"),
          Button(`class` = cls(t"pyro-animation-play"), `type` = t"button")(t"▶"),
          Span(`class` = cls(t"pyro-animation-label"))(phrase(first.lay(Nil)(_.label))) )

    val parts: List[Html of Flow] =
      List
        ( Pre(`class` = classes)(Code(rows*)),
          controls,
          Pre(`class` = cls(t"pyro-animation-data"))(animationJson(language, animation)) )

    id.lay(Div(`class` = cls(t"pyro-animated"), tabindex = t"0")(parts*)): (id: Text) =>
      Div(id = id, `class` = cls(t"pyro-animated"), tabindex = t"0")(parts*)

  // One slide's lines as rows of atoms — block-level lines (the stylesheet's rule), so no
  // newline stands between them — with the frame's notes laid over them: an atom any part of
  // which a note covers is marked whole, since an atom is never split (its identity is what
  // the animation moves), and a caret or captioned note has its row beneath its line, as a
  // plain block draws it.
  private def animatedRows(lines: List[List[Block.Atom]], notes: List[Block.Note]): List[Html of Phrasing] =
    lines.indexed.bind: (atoms: List[Block.Atom], index: Ordinal) =>
      val length: Int = atoms.fold(0) { (total: Int, atom: Block.Atom) => total + atom.text.length }

      val ranges: List[(Int, Int, Block.Note)] =
        notes.bind: (note: Block.Note) =>
          note.range(index.n0, length).lay(Nil) { (range: (Int, Int)) => List((range(0), range(1), note)) }

      var offset: Int = 0

      val spans: List[Html of Phrasing] =
        atoms.map: (atom: Block.Atom) =>
          val end: Int = offset + atom.text.length

          val covering: Optional[Block.Note] =
            ranges.seek { (range: (Int, Int, Block.Note)) => range(0) < range(1) && range(0) < end && range(1) > offset }.let(_(2))

          offset = end
          atomSpan(atom, covering)

      val row: Html of Phrasing = Span(`class` = cls(t"pyro-line"))(spans*)
      row :: markers(notes, index.n0, length).map { (note: Block.Note) => marker(note, index.n0, length) }

  def atomSpan(atom: Block.Atom, note: Optional[Block.Note] = Unset): Html of Phrasing =
    val binding: List[Name[CssClass]] = if atom.role == Token.Role.Binding then List(cls(t"pyro-binding")) else Nil

    val marked: List[Name[CssClass]] =
      note.lay(Nil) { (note: Block.Note) => List(cls(t"pyro-marked"), cls(t"pyro-marked-${note.style.toString.tt.lower}")) }

    val classes: List[Name[CssClass]] = cls(t"pyro-atom") :: accentClass(atom.accent) :: (binding + marked)

    note.lay(Span(`class` = classes, `data-atom` = atom.id)(atom.text)): (note: Block.Note) =>
      Span(`class` = classes, `data-atom` = atom.id, title = Inline.plain(note.caption))(atom.text)

  // The animation as compact JSON: the block's language, atoms by id, slides, placements as
  // `[slide, line, id]`, and transitions with their moves as `[id, from, to]`. A slide's
  // label and its notes' captions are phrasing, rendered here as HTML, so the script shows
  // them without a renderer of its own.
  def animationJson(language: pyrocosm.Language, animation: Block.Animation): Text =
    def optional(value: Optional[Double]): Text = value.lay(t"null")(_.toString.tt)
    def ids(ids: List[Text]): Text = ids.map(jsonText).join(t",")

    val atoms: Text =
      animation.atoms.map: (atom: Block.Atom) =>
        val binding: Text = if atom.role == Token.Role.Binding then t"true" else t"false"
        t"""{"id":${jsonText(atom.id)},"text":${jsonText(atom.text)},"accent":${jsonText(atom.accent.toString.tt.lower)},"binding":$binding}"""
      . join(t",")

    def notes(frame: Block.Slide): Text =
      frame.notes.map: (note: Block.Note) =>
        val last: Text = note.last.lay(t"null")(_.toString.tt)
        val caption: Text = if note.caption.nil then t"\"\"" else jsonText(phrase(note.caption).show)
        t"""{"line":${note.line.toString.tt},"start":${note.start.toString.tt},"end":${note.end.toString.tt},"last":$last,"style":${jsonText(note.style.toString.tt.lower)},"caption":$caption}"""
      . join(t",")

    val slides: Text =
      animation.slides.map: (frame: Block.Slide) =>
        val lines: Text = ids(frame.lines)
        val label: Text = if frame.label.nil then t"\"\"" else jsonText(phrase(frame.label).show)
        val frameLanguage: Text = frame.language.lay(t"null") { (language: pyrocosm.Language) => jsonText(language.name) }
        t"""{"index":${frame.index.toString.tt},"label":$label,"hold":${optional(frame.hold)},"at":${optional(frame.at)},"language":$frameLanguage,"lines":[$lines],"notes":[${notes(frame)}]}"""
      . join(t",")

    val placements: Text =
      animation.placements.map: (placement: Block.Placement) =>
        t"[${placement.slide.toString.tt},${placement.line.toString.tt},${jsonText(placement.atom)}]"
      . join(t",")

    val transitions: Text =
      animation.transitions.map: (transition: Block.Transition) =>
        val moved: Text =
          transition.moved.map { (move: Block.Transition.Move) => t"[${jsonText(move.atom)},${jsonText(move.from)},${jsonText(move.to)}]" }.join(t",")

        t"""{"from":${transition.from.toString.tt},"to":${transition.to.toString.tt},"kind":${jsonText(transition.kind.toString.tt.lower)},"duration":${optional(transition.duration)},"departing":[${ids(transition.departing)}],"arriving":[${ids(transition.arriving)}],"moved":[$moved],"removed":[${ids(transition.removed)}],"inserted":[${ids(transition.inserted)}]}"""
      . join(t",")

    t"""{"language":${jsonText(language.name)},"atoms":[$atoms],"slides":[$slides],"placements":[$placements],"transitions":[$transitions],"autoplay":${animation.autoplay.toString.tt},"loop":${animation.loop.toString.tt}}"""

  // One line of code, the `index`th of its block, with the segments of the notes which touch
  // it laid over its tokens; a caret marks no segment. The line ends in a newline unless it is
  // the `last`.
  def codeLine(line: Block.Line, notes: List[Block.Note], last: Boolean, index: Int = 0): Html of Phrasing =
    val length: Int = line.tokens.fold(0) { (total: Int, token: Token) => total + token.text.length }

    val ranges: List[(Int, Int, Block.Note)] =
      notes.bind: (note: Block.Note) =>
        note.range(index, length).lay(Nil) { (range: (Int, Int)) => List((range(0), range(1), note)) }

    // Each token, cut at the boundaries where a segment begins or ends inside it, with a
    // running offset into the line.
    var offset: Int = 0
    var rendered: List[(Token, Html of Phrasing)] = Nil

    line.tokens.each: (token: Token) =>
      val end = offset + token.text.length
      val ends: List[Int] = ranges.bind { (range: (Int, Int, Block.Note)) => List(range(0), range(1)) }
      val cuts: List[Int] = (offset :: end :: ends).filter { (cut: Int) => cut >= offset && cut <= end }.distinct.sort
      var pieces: List[Html of Phrasing] = Nil

      cuts.zip(cuts.tail.or(Nil)).each: (from: Int, to: Int) =>
        val text = token.text.s.substring(from - offset, to - offset).nn.tt
        val base = this.token(token.copy(text = text))

        val covering: Optional[Block.Note] =
          ranges.seek { (range: (Int, Int, Block.Note)) => range(0) <= from && range(1) >= to && range(0) < range(1) }.let(_(2))

        val styled: Html of Phrasing =
          covering.lay(base): (note: Block.Note) =>
            Span(`class` = cls(t"pyro-note-${note.style.toString.tt.lower}"), title = Inline.plain(note.caption))(base)

        pieces = styled :: pieces

      // The token's pieces as one element, so that a qualified name chains whole tokens.
      rendered = (token, Html.Fragment(pieces.reverse*)) :: rendered
      offset = end

    val ordered: List[Html of Phrasing] = qualified(rendered.reverse)

    Span(`class` = cls(t"pyro-line"))(Html.Fragment(ordered*), if last then Html.Fragment[Phrasing]() else t"\n")

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
          Td(`class` = columnClasses(column))(row.cells.at(index).lay(Html.Fragment[Phrasing]()) { cell => phrase(cell.content) })
        }
        val classes: List[Name[CssClass]] = row.tone.lay(Nil: List[Name[CssClass]]) { tone => List(toneClass(tone)) }
        row.action.lay(row.id.lay(Tr(`class` = classes)(cells*)) { id => Tr(id = id, `class` = classes)(cells*) }):
          action => Tr(id = action.id, `class` = cls(t"pyro-action") :: classes)(cells*)
      }*)

    val captioned: List[Html of "caption"] = caption.lay(Nil: List[Html of "caption"]) { caption => List(Caption(phrase(caption))) }
    Table(`class` = cls(t"pyro-table"))(Html.Fragment(captioned*), head, body)

  // A stack trace: a section per exception in the chain, each a heading naming the exception
  // and its message, and a table of its frames laid out as the terminal lays them out—`at`, the
  // class with its last segment in the package's accent (`pyro-trace-accent-N`, by order of first
  // appearance), the method, the file, the line and the quoted source—with a `↳` row beneath a
  // frame for each level of inlining. The location is contiguous, `File.scala:42`: the colon
  // opens the line cell, and the stylesheet leaves no gap between it and the file cell, so the
  // colons stack in a column. A repeated class or file name (`pyro-repeat`), a compiler-synthesised
  // frame (`pyro-plumbing`) and an inlining row (`pyro-inlined`) are marked for the sheet to dim.
  private def trace(stacks: List[Block.Trace.Stack]): Html of Flow =
    def stack(stack: Block.Trace.Stack): Html of Flow =
      val accents: Map[Text, Int] = Block.Trace.accents(stack)

      def accentClass(namespace: Text): Name[CssClass] =
        cls(t"pyro-trace-accent-${accents(namespace).or(0)%5 + 1}")

      val heading =
        Header(`class` = cls(t"pyro-trace-heading"))
          ( Code(`class` = cls(t"pyro-trace-class"))(Em(t"${stack.component}."), Strong(stack.className)),
            t": ",
            Span(`class` = cls(t"pyro-trace-message"))(phrase(stack.message)) )

      def frame(frame: Block.Trace.Frame, sameClass: Boolean, sameFile: Boolean): Html of "tr" =
        val (prefix, segment) = Block.Trace.split(frame.owner)
        val repeat: List[Name[CssClass]] = if sameClass then List(cls(t"pyro-repeat")) else Nil
        val fileRepeat: List[Name[CssClass]] = if sameFile then List(cls(t"pyro-repeat")) else Nil
        val plumbing: List[Name[CssClass]] = if frame.plumbing then List(cls(t"pyro-plumbing")) else Nil
        val rowClasses: List[Name[CssClass]] = cls(t"pyro-frame") :: plumbing
        val joined = Block.Trace.joined(frame)

        Tr(`class` = rowClasses)
          ( Td(`class` = cls(t"pyro-at"))(t"at"),
            Td(`class` = cls(t"pyro-owner") :: accentClass(frame.namespace) :: repeat)
              (Span(`class` = cls(t"pyro-prefix"))(prefix), Strong(segment)),
            Td(`class` = cls(t"pyro-dot"))(if joined then t"." else t"⌗"),
            Td(`class` = cls(t"pyro-method"))(frame.method),
            Td(`class` = cls(t"pyro-file") :: fileRepeat)(frame.file),
            Td(`class` = cls(t"pyro-line"))(Span(`class` = cls(t"pyro-colon"))(t":"), frame.line.let(_.show).or(t"")),
            Td(`class` = List(cls(t"pyro-source"), cls(t"pyro-collapsible")))(Code(frame.code.or(t""))) )

      def origin(origin: Block.Trace.Origin): Html of "tr" =
        Tr(`class` = List(cls(t"pyro-frame"), cls(t"pyro-inlined")))
          ( Td(`class` = cls(t"pyro-at"))(t"↳"),
            Td(`class` = cls(t"pyro-owner"))(origin.owner.or(t"")),
            Td(`class` = cls(t"pyro-dot"))(origin.owner.lay(t"")(_ => t".")),
            Td(`class` = cls(t"pyro-method"))(origin.name.or(t"inlined from")),
            Td(`class` = cls(t"pyro-file"))(origin.file),
            Td(`class` = cls(t"pyro-line"))(Span(`class` = cls(t"pyro-colon"))(t":"), origin.line.show),
            Td(`class` = List(cls(t"pyro-source"), cls(t"pyro-collapsible")))(Code(origin.code.or(t""))) )

      val rows: List[Html of "tr"] =
        stack.frames.fold((Nil: List[Html of "tr"], t"", t"")):
          case ((acc, lastClass, lastFile), frame0) =>
            val row = frame(frame0, frame0.owner == lastClass, frame0.file == lastFile)
            val subRows = frame0.inlined.map(origin).reverse
            (subRows + (row :: acc), frame0.owner, frame0.file)
        . _1.reverse

      Section(`class` = cls(t"pyro-trace"))(heading, Table(`class` = cls(t"pyro-frames"))(Tbody(rows*)))

    Html.Fragment[Flow](stacks.indexed.map { (stack0: Block.Trace.Stack, index: Ordinal) =>
      if index.n0 == 0 then stack(stack0)
      else Html.Fragment[Flow](P(`class` = cls(t"pyro-caused-by"))(t"caused by:"), stack(stack0))
    }*)

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

  // An activity is a card: its title, its gauge and its state, all of it the link to its view
  // when it has one. The card's own id keeps it from the action's, which a navigation item may
  // carry too; its press sends the action named in `data-action`. Another node's activity
  // carries its origin as a label, is prefixed by the origin's key, since two nodes may use the
  // same id, and is never an action of this page: an internal destination from elsewhere is
  // no link at all.
  def activity(activity: Activity, origin: Optional[Origin] = Unset): Html of Flow =
    val id: Text = origin.lay(t"pyro-activity-${activity.id}") { origin => t"pyro-activity-${origin.key}-${activity.id}" }
    val title: Html of Flow = Div(`class` = cls(t"pyro-activity-title"))(phrase(activity.title))
    val state: Html of Flow = Div(`class` = cls(t"pyro-activity-state"))(phrase(activity.state))

    val label: Html of Flow = origin.lay(Html.Fragment[Flow]()): origin =>
      Div(`class` = cls(t"pyro-activity-origin"))(originLabel(origin))

    val body: Html of Flow = Html.Fragment(label, title, gauge(activity.status, Unset), state)
    val remote: List[Name[CssClass]] = if origin.present then List(cls(t"pyro-activity-remote")) else Nil
    val classes: List[Name[CssClass]] = cls(t"pyro-activity") :: remote

    activity.destination match
      case Inline.Destination.External(url) =>
        A(href = url, id = id, `class` = classes)(body)

      case Inline.Destination.Internal(action) if origin.absent =>
        A(href = t"#", id = id, `class` = classes :+ cls(t"pyro-action"), `data-action` = action.id)(body)

      case local: Inline.Destination.Local if origin.absent =>
        A(href = href(local), id = id, `class` = classes :+ cls(t"pyro-locate"))(body)

      case _ =>
        Div(id = id, `class` = classes)(body)

  private def originLabel(origin: Origin): Text =
    if origin.local then origin.tool else t"${origin.tool} · ${origin.machine}"

  def activities(activities: List[Activity]): Html of Flow = Html.Fragment(activities.map(activity(_))*)

  def remoteActivities(activities: List[(Origin, Activity)]): Html of Flow =
    Html.Fragment(activities.map { (origin, activity) => this.activity(activity, origin) }*)

  // The tools running, one item each: a link to its web front-end where it serves one, a
  // muted name otherwise; this process's own marked as current. A link to a tool of this
  // machine says so, for the script to re-address when the page is viewed from elsewhere.
  def toolLinks(origins: List[Origin]): Html of Flow =
    val items: List[Html of Flow] = origins.map(toolLink(_))
    Html.Fragment[Flow](items*)

  private def toolLink(origin: Origin): Html of Flow =
    val current: List[Name[CssClass]] = if origin.self then List(cls(t"pyro-menubar-current")) else Nil

    origin.url match
      case url: Text =>
        A(href = url, `class` = cls(t"pyro-menubar-tool") :: current, `data-local` = if origin.local then t"true" else t"false")(originLabel(origin))

      case _ =>
        Span(`class` = cls(t"pyro-menubar-tool") :: cls(t"pyro-menubar-unlinked") :: current)(originLabel(origin))

  private def standingGlyph(standing: Standing): Text = standing match
    case Standing.Pending   => t"·"
    case Standing.Running   => t"▶"
    case Standing.Succeeded => t"✓"
    case Standing.Failed    => t"✗"
    case Standing.Warned    => t"△"
    case Standing.Skipped   => t"‑"
