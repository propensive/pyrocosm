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
// definitions, since a wildcard import beats a package member declared in another file.
import soundness.{Origin as _, *}

import attributives.textAttributive
import dysasymptotics.{linearAccess, linearSize}
import htmlDoms.whatwg
import htmlDoms.whatwg.*
// `Em` and `Span` are imported by name, so they mean the HTML elements and not the names the
// umbrella also offers: hiding them from the umbrella works too, but `-Wunused` thinks it unused.
import htmlDoms.whatwg.{Em, Span}
import nomenclature.CssClass.nominative

// The static half of the web frontend: phrasing to phrasing content, a block to flow content.
// Everything is semantic HTML carrying `pyro-*` classes for the stylesheet, each naming what an
// element is or a state it is in, and nothing carries a `style` attribute: appearance is the
// stylesheet's, so a theme is a `:root` block and a restyling is another sheet. Elements a
// user can act upon carry the handle's id, which is what the browser sends back.
object HtmlRenderer:
  // A fixed column's share of the width, as the stylesheet knows it: a multiple of five percent.
  val shares: List[Int] = List.range(1, 21).map(_*5)
  def share(percent: Int): Int = ((percent + 2)/5*5).max(5).min(100)

  // The class of a tone, `pyro-tone-success` and so on.
  def toneClass(tone: Tone): Name[CssClass] =
    unsafely(Name[CssClass](t"pyro-tone-${tone.toString.tt.lower}"))

  // The DOM id of a panel or a handle: what a patch replaces and what an event names.
  def panelId(panel: Panel): Text = t"pyro-panel-${panel.id.label}"

  // `data-node="0.2.1"` on a collapsible tree node: its path within its tree, by which the
  // script keeps the open state a reader gave it across repaints of its panel.
  given nodePath: ("data-node" is Attribute of Whatwg.Textual in Whatwg) = Whatwg.globalAttribute()

  // `data-serial="7"` on a panel's content: the number of the assignment of its cell which the
  // content is of, by which the script knows whether a change to it is a change from what it
  // shows.
  given dataSerial: ("data-serial" is Attribute of Whatwg.Textual in Whatwg) =
    Whatwg.globalAttribute()

  // `data-action="…"` on an activity's card: the action its press sends. The card has an id of
  // its own, as the same action may belong to a navigation item already on the page.
  given actionId: ("data-action" is Attribute of Whatwg.Textual in Whatwg) =
    Whatwg.globalAttribute()

  // `data-path="/api/com.example"` on an import toggle or a bookmark button: the page it is of,
  // which the script sends with the click.
  given dataPath: ("data-path" is Attribute of Whatwg.Textual in Whatwg) = Whatwg.globalAttribute()

  given dataLabel: ("data-label" is Attribute of Whatwg.Textual in Whatwg) =
    Whatwg.globalAttribute()

  // `data-schedule="[4,8,12]"` on a deck which plays itself: the time, in seconds from the
  // page's opening, at which each of its steps lands, which the script conducts it to.
  given dataSchedule: ("data-schedule" is Attribute of Whatwg.Textual in Whatwg) =
    Whatwg.globalAttribute()

  // `aria-label="…"` on a control which shows no words of its own.
  given ariaLabel: ("aria-label" is Attribute of Whatwg.Textual in Whatwg) =
    Whatwg.globalAttribute()

  // What the code renderer's companion has, which was this one's before code had a module of
  // its own: its classes, its JSON text and the attributes its elements carry.
  export CodeRenderer.{accentClass, cls, jsonText, given}

  // `data-local="true"` on a menu link to a tool of this machine, whose `localhost` the script
  // replaces with the page's own host when the page is viewed from another.
  given localLink: ("data-local" is Attribute of Whatwg.Textual in Whatwg) =
    Whatwg.globalAttribute()

// The `purview` is the reader's: what it imports is elided from every qualified name, and an
// import toggle or a bookmark button shows whether its page is in it. The `prefix` is the path
// the application is served under (`/fume`, say; empty at the origin's root), which goes before
// every local destination's link; a page's path as the application knows it, which a purview
// button carries for the frontend, has no prefix. The `locale` is the reader's, in which
// Pyrocosm's own words (`Phrases`) are said: the matter is the application's, as it gives it.
class HtmlRenderer
  ( purview: Purview                = Purview(),
    prefix:  Text                   = t"",
    val locale: Locale[Tongue.Chrome] = Locale(en) )
extends CodeRenderer:

  private given Locale[Tongue.Chrome] = locale

  // The attribute givens are imported wholesale: a macro summons them, which `-Wunused` does not
  // see, so it would call any of them imported by name unused.
  import HtmlRenderer.{toneClass, cls, given}

  def phrase(content: List[Inline]): Html of Phrasing = Html.Fragment(content.map(inline1)*)

  // Content that is one snippet alone, blank text aside: a paragraph or a cell that is wholly
  // code is marked `pyro-solitary`, and its code shown bare, since a box around all of it says
  // nothing.
  private def solitary(content: List[Inline]): Boolean =
    content.filter { case Inline.Textual(text) => !text.blank; case _ => true } match
      case List(Inline.Snippet(_, _)) => true
      case _                          => false

  private def solitaryClass(content: List[Inline]): List[Name[CssClass]] =
    if solitary(content) then List(cls(t"pyro-solitary")) else Nil

  // The `href` of a local destination: its path under the prefix, and its fragment.
  protected def href(local: Inline.Destination.Local): Text =
    local.fragment.lay(t"$prefix${local.path}"): fragment => t"$prefix${local.path}#$fragment"

  // A label which leads somewhere: an anchor around it for a local or external destination, an
  // action's anchor for an internal one.
  private def lead(destination: Inline.Destination, label: Html of Phrasing): Html of Phrasing =
    destination match
      case Inline.Destination.External(url) =>
        A(href = url, `class` = List(cls(t"pyro-link"), cls(t"pyro-external")))(label)

      case Inline.Destination.Internal(action) =>
        val style: List[Name[CssClass]] = List(cls(t"pyro-link"), cls(t"pyro-action"))
        A(href = t"#", id = action.id, `class` = style)(label)

      case local: Inline.Destination.Local =>
        A(href = href(local), `class` = List(cls(t"pyro-link"), cls(t"pyro-locate")))(label)

  def tokens(tokens: List[Token]): Html of Phrasing =
    val pairs = purview.elide(tokens).map: (token: Token) => (token, this.token(token))
    Html.Fragment(qualified(pairs)*)

  // An import toggle or a bookmark button: a button carrying its page's path and what to list
  // it by, active when the page is in the purview, so that the script sends the click and the
  // frontend answers with the panels re-rendered under the new purview. It shows a mark alone;
  // its title says what it does, and to what.
  private def purviewButton
    ( kind:   Text,
      path:   Text,
      label:  Text,
      active: Boolean,
      title:  Text,
      mark:   Html of Phrasing = Html.Fragment[Phrasing]() )
  :   Html of Phrasing =

    val classes: List[Name[CssClass]] =
      List(cls(t"pyro-$kind")) + (if active then List(cls(t"pyro-active")) else Nil)

    Button
      ( `type`       = t"button",
        `class`      = classes,
        `data-path`  = path,
        `data-label` = label,
        title        = title )
      ( mark )

  private def importButton(path: Text, label: Text): Html of Phrasing =
    val active: Boolean = purview.imported(path)

    val title: Text =
      if active then Phrases.removeImport(label)() else Phrases.addImport(label)()

    purviewButton(t"import", path, label, active, title)

  private def bookmarkButton(path: Text, label: Text): Html of Phrasing =
    val active: Boolean = purview.bookmarked(path)

    val title: Text =
      if active then Phrases.removeBookmark(label)() else Phrases.addBookmark(label)()

    val mark: Text =
      t"""<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">""" +
        t"""<path d="M6 3h12v18l-6-5-6 5z"/></svg>"""

    purviewButton(t"bookmark", path, label, active, title, phrase(List(Inline.Icon(mark, t""))))

  // The reader's purview as a side panel: the imports, then the bookmarks, each entry a link
  // to its page with the button which removes it; replaced as one when the purview changes.
  // An entry's path is the page's as the application knows it, so its link goes under the
  // prefix, as a local destination's does; the button carries the path bare.
  def purviewPanel: Html of Flow =
    def entries(kind: Text, entries: List[Purview.Entry], empty: Text): Html of Flow =
      if entries.nil then P(`class` = cls(t"pyro-empty"))(empty)
      else
        val items = entries.map: (entry: Purview.Entry) =>
          val remove: Html of Phrasing =
            purviewButton(kind, entry.path, t"", true, Phrases.remove(entry.label)())

          val style: List[Name[CssClass]] = List(cls(t"pyro-link"), cls(t"pyro-locate"))
          Li(A(href = t"$prefix${entry.path}", `class` = style)(entry.label), t" ", remove)

        Ul(`class` = cls(t"pyro-purview-list"))(items*)

    Section(id = t"pyro-purview", `class` = List(cls(t"pyro-panel"), cls(t"pyro-purview")))
      ( H2(Phrases.imports()),
        entries(t"import", purview.imports, Phrases.noImports()),
        H2(Phrases.bookmarks()),
        entries(t"bookmark", purview.bookmarks, Phrases.noBookmarks()) )

  // Text, but for the Powerline triangle (U+E0B0) a prompt is marked with: it is a private-use
  // character which a terminal's font has and a page's does not, so it is drawn as a shape, a
  // character wide and a line high, in the colour of the text around it.
  private def textual(text: Text): Html of Phrasing =
    if !text.contains(t"\ue0b0") then text else
      val arrow: Html of Phrasing = Span(`class` = cls(t"pyro-powerline"))()

      val pieces: List[Html of Phrasing] = text.cut(t"\ue0b0") match
        case head :: tail =>
          (head: Html of Phrasing) :: tail.flatMap: (part: Text) =>
            List[Html of Phrasing](arrow, part)

        case _ =>
          Nil

      Html.Fragment[Phrasing](pieces*)

  private def inline1(node: Inline): Html of Phrasing = node match
    case Inline.Textual(text)        => textual(text)
    case Inline.Phrase(content)      => phrase(content)
    case Inline.Emphasis(content)    => Em(phrase(content))
    case Inline.Strong(content)      => Strong(phrase(content))
    case Inline.Toned(tone, content) => Span(`class` = toneClass(tone))(phrase(content))
    case Inline.Snippet(_, tokens0)  => Code(`class` = cls(t"pyro-code"))(tokens(tokens0))
    case Inline.Keystroke(keypress)  => Kbd(`class` = cls(t"pyro-key"))(keyText(keypress))
    case Inline.Reference(id)        => Code(`class` = cls(t"pyro-reference"))(id)
    case Inline.Break()              => Br

    case Inline.Symbol(glyph) =>
      Span(`class` = List(cls(t"pyro-glyph"), cls(t"pyro-glyph-${glyph.toString.tt.lower}")))
        ( glyphText(glyph) )

    case Inline.Math(math) =>
      Span(`class` = cls(t"pyro-math"))(safely(Ergo.serialize(math)).or(t"…"))

    // An icon which is markup is drawn inline, as a figure is, in a holder which sizes it to the
    // text and colours it with it; markup that does not parse shows its description.
    case Inline.Icon(source, alt) =>
      if !source.starts(t"<svg") then Img(src = source, alt = alt, title = alt, `class` = cls(t"pyro-icon"))
      else
        val holder = cls(t"pyro-glyph")

        safely(source.read[Html of "svg"]).lay(Span(`class` = holder, title = alt)(alt)): svg =>
          if alt == t"" then Span(`class` = holder)(svg) else Span(`class` = holder, title = alt)(svg)

    // The colour is no theme's, so no class can say it, and nothing carries an inline style:
    // the square is drawn, as an icon is, filled with the colour; its title says which it is.
    case Inline.Swatch(colour) =>
      val drawing: Text =
        t"""<svg viewBox="0 0 1 1"><rect width="1" height="1" fill="${colour.hex}"/></svg>"""

      safely(drawing.read[Html of "svg"]).lay(Span(title = colour.hex)(colour.hex)): svg =>
        Span(`class` = cls(t"pyro-swatch"), title = colour.hex)(svg)

    case Inline.Link(destination, content) => lead(destination, phrase(content))
    case Inline.Import(path, label)        => importButton(path, label)
    case Inline.Bookmark(path, label)      => bookmarkButton(path, label)

    // The note is in the element, after the content, so that it is read by what has no hover
    // (a screen reader, a text browser); the stylesheet shows it only when pointed at or
    // focused, which the tabindex allows from the keyboard.
    case Inline.Annotated(content, note) =>
      Span(`class` = cls(t"pyro-annotated"), tabindex = t"0")
        ( phrase(content), Span(`class` = cls(t"pyro-annotation"))(phrase(note)) )

    case Inline.Amount(value, units) =>
      val (number, unit) = Amounts.scaled(value, units)

      Span(`class` = cls(t"pyro-amount"), title = t"${value.toString} $units")
        ( Span(`class` = cls(t"pyro-figure"))(number),
          t" ",
          Span(`class` = cls(t"pyro-units"))(unit) )

    case Inline.Numeral(value, precision) =>
      Span(`class` = cls(t"pyro-figure"))(Amounts.figure(value, precision))

  // A keypress without the brackets clavichord's rendering puts around each key: a keycap is
  // already a box, so `[⌃]+[C]` reads as `⌃+C`.
  private def keyText(keypress: Keypress): Text =
    keypress.show.s.replace("[", "").nn.replace("]", "").nn.tt

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

  // Blocks in order, but for code which is directly followed by captured output: what it
  // printed when it ran, which is shown as the lower part of the code's own block, beneath a
  // line, and not behind a gutter. Plain text is not code that ran, so is not joined to the
  // output after it.
  def blocks(blocks: List[Block]): Html of Flow =
    // The output at the head of `todo`, rendered without a gutter, and what follows it.
    def printed(todo: List[Block], done: List[Html of Flow]): (List[Html of Flow], List[Block]) =
      todo match
        case Block.Output(text, error, runs) :: tail =>
          printed(tail, output(text, error, runs, false) :: done)

        case _ =>
          (done.reverse, todo)

    def recur(todo: List[Block], done: List[Html of Flow]): List[Html of Flow] = todo match
      case Nil =>
        done.reverse

      case (code: Block.Code) :: (rest @ ((_: Block.Output) :: _))
        if code.language != pyrocosm.Language.Plain =>
          val (outputs, tail) = printed(rest, Nil)
          val ran: Html of Flow = Div(`class` = cls(t"pyro-code-ran"))((block(code) :: outputs)*)
          recur(tail, ran :: done)

      case head :: tail =>
        recur(tail, block(head) :: done)

    Html.Fragment(recur(blocks, Nil)*)

  def block(block: Block): Html of Flow = block match
    case Block.Paragraph(content) => P(`class` = solitaryClass(content))(phrase(content))

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

      if ordered then Ol(`class` = cls(t"pyro-listing"))(entries*)
      else Ul(`class` = cls(t"pyro-listing"))(entries*)

    case Block.Quotation(content) => Blockquote(`class` = cls(t"pyro-quotation"))(blocks(content))
    case Block.Rule(side)         => Hr(`class` = cls(t"pyro-rule-${side.toString.tt.lower}"))

    case code: Block.Code => this.code(code)

    case Block.Table(columns, rows, caption) => table(columns, rows, caption)

    case Block.Record(entries, title) =>
      val terms = entries.bind: (entry: Block.Entry) =>
        List[Html of "dt" | "dd"](Dt(phrase(entry.key)), Dd(blocks(entry.value)))

      val list = Dl(`class` = cls(t"pyro-record"))(terms*)

      title.lay(list: Html of Flow): title =>
        Section(`class` = cls(t"pyro-record-section"))(H4(phrase(title)), list)

    case Block.Notice(tone, title, content) =>
      val heading: Html of Flow =
        title.lay(Html.Fragment[Flow]()): title => Header(Strong(phrase(title)))

      Aside(`class` = List(cls(t"pyro-notice"), toneClass(tone)))
        ( Html.Fragment[Flow](heading, blocks(content)) )

    // A diagnostic is an article in its tone: a header naming the place, the severity and the
    // code, the message, the excerpt and whatever follows.
    case Block.Diagnostic(tone, location, message, excerpt, detail, code) =>
      val glyph: Glyph = tone match
        case Tone.Failure => Glyph.Cross
        case Tone.Warning => Glyph.Warning
        case Tone.Success => Glyph.Check
        case _            => Glyph.Bullet

      val severity: Text = tone match
        case Tone.Failure => Phrases.error()
        case Tone.Warning => Phrases.warning()
        case Tone.Success => Phrases.ok()
        case Tone.Info    => Phrases.info()
        case _            => Phrases.note()

      val where: Html of Phrasing =
        location.lay(Html.Fragment[Phrasing]()): (location: Block.Location) =>
          Code(`class` = cls(t"pyro-location"))(location.text)

      val coded: Html of Phrasing =
        code.lay(Html.Fragment[Phrasing]()): (code: fulminate.Bolt) =>
          Code(`class` = cls(t"pyro-code"))(code.text)

      val rest: Html of Flow =
        if detail.nil then Html.Fragment[Flow]()
        else Div(`class` = cls(t"pyro-detail"))(blocks(detail))

      Article(`class` = List(cls(t"pyro-diagnostic"), toneClass(tone)))
        ( Header
            ( Span(`class` = cls(t"pyro-glyph"))(glyphText(glyph)),
              where,
              Span(`class` = cls(t"pyro-severity"))(severity),
              coded ),
          Div(`class` = cls(t"pyro-message"))(blocks(message)),
          blocks(excerpt),
          rest )

    case Block.Disclosure(summary, content, open) =>
      val style = cls(t"pyro-disclosure")

      if open then Details(`class` = style, open = true)(Summary(phrase(summary)), blocks(content))
      else Details(`class` = style)(Summary(phrase(summary)), blocks(content))

    case Block.Image(source, alt) =>
      Figure(`class` = cls(t"pyro-image"))(Img(src = source, alt = alt), Figcaption(alt))

    // A figure's drawing, its ids qualified by the figure's so several drawings share a page,
    // is read as foreign (SVG) content into a holder, `<figure id>-svg`, which a redraw replaces
    // whole and a part revision patches inside. Markup that does not parse shows its description.
    case Block.Figure(figure) =>
      val markup: Text = pyrocosm.Figure.namespace(figure.id, purview.drawing(figure.svg))
      val drawing: Optional[Html of "svg"] = safely(markup.read[Html of "svg"])

      val holderId: Text = t"${figure.id}-svg"
      val holderClass = cls(t"pyro-drawing-holder")

      val holder: Html of Flow =
        def fallback: Html of Flow =
          Div(id = holderId, `class` = holderClass)(P(phrase(figure.alt)))

        drawing.lay(fallback): svg => Div(id = holderId, `class` = holderClass)(svg)

      Figure(id = figure.id, `class` = cls(t"pyro-drawing"))(holder, Figcaption(phrase(figure.alt)))

    case Block.Deck(leaves, schedule) =>
      deck(leaves, schedule)

    case Block.Tree(roots) =>
      val nodes = roots.indexed.map: (root: Block.TreeNode, index: Ordinal) =>
        treeNode(root, t"${index.n0}")

      Ul(`class` = cls(t"pyro-tree"))(nodes*)

    // A graph as its vertices, each with what it depends on; a drawn layout is later work.
    case Block.Graph(vertices, edges) =>
      val labels: Map[Text, List[Inline]] =
        vertices.map { (vertex: Block.Vertex) => vertex.id -> vertex.label }.to[Map]

      val items = vertices.map: (vertex: Block.Vertex) =>
        val dependencies =
          edges.filter(_.from == vertex.id).map: (edge: Block.Edge) =>
            labels(edge.to).or(Inline.text(edge.to))

        val label = vertexLabel(vertex)

        if dependencies.nil then Li(label)
        else
          val targets = dependencies.indexed.map: (target, index) =>
            Html.Fragment[Phrasing]
              ( if index.n0 == 0 then Html.Fragment[Phrasing]() else t", ", phrase(target) )

          Li(label, Span(`class` = cls(t"pyro-arrow"))(t" → "), Html.Fragment(targets*))

      Ul(`class` = cls(t"pyro-graph"))(items*)

    case Block.Chart(kind, series)    => chart(kind, series)
    case Block.Gauge(status, caption) => gauge(status, caption)
    case Block.Trace(stacks)          => trace(stacks)
    case Block.Group(content)         => Div(`class` = cls(t"pyro-group"))(blocks(content))

    case Block.Output(text, error, runs) => output(text, error, runs, true)

  // Captured output, verbatim, each line behind a gutter naming its stream (but for output
  // joined to the code which printed it, which has none), and each run of it in its style: a
  // colour is set inline, since the output chose it and no theme can name it, and the
  // attributes are classes; a run with neither is bare text.
  private def output(text: Text, error: Boolean, runs: List[Block.Run], gutter: Boolean)
  :   Html of Flow =

    val margin: List[Html of Phrasing] =
      if gutter then List(Span(`class` = cls(t"pyro-gutter"))(t"░ ")) else Nil

    val lines: List[List[(Text, Block.Run)]] = Block.Run.lines(text, runs)
    val count = lines.size

    def hex(rgb: Block.Rgb): Text = f"#${rgb.red}%02x${rgb.green}%02x${rgb.blue}%02x".tt

    def piece(slice: Text, run: Block.Run): Html of Phrasing =
      val flags: List[(Boolean, Text)] =
        List
          ( (run.bold, t"bold"), (run.faint, t"faint"), (run.italic, t"italic"),
            (run.underline, t"underline"), (run.strike, t"strike"), (run.reverse, t"reverse") )

      val classes: List[Name[CssClass]] =
        flags.filter(_(0)).map: (flag: (Boolean, Text)) => cls(t"pyro-ansi-${flag(1)}")

      val foreground: List[Text] = run.foreground.let(c => List(t"color:${hex(c)}")).or(Nil)
      val background: List[Text] =
        run.background.let(c => List(t"background-color:${hex(c)}")).or(Nil)

      val style: Text = (foreground + background).join(t";")

      if classes.nil && style == t"" then Html.Fragment[Phrasing](slice)
      else if style == t"" then Span(`class` = classes)(slice)
      else Span(`class` = classes, style = style)(slice)

    def pieces(line: List[(Text, Block.Run)]): Html of Phrasing =
      val styled: List[Html of Phrasing] = line.map: (pair: (Text, Block.Run)) =>
        piece(pair(0), pair(1))

      Html.Fragment[Phrasing](styled*)

    val rows: List[Html of Phrasing] =
      lines.indexed.map: (line: List[(Text, Block.Run)], index: Ordinal) =>
        val end: Text = if index.n0 == count - 1 then t"" else t"\n"
        Html.Fragment[Phrasing]((margin + List[Html of Phrasing](pieces(line), end))*)

    val stream = if error then cls(t"pyro-output-stderr") else cls(t"pyro-output-stdout")
    Pre(`class` = List(cls(t"pyro-output"), stream))(rows*)

  // An item with a destination and no action is a real link (which opens in a new tab as a
  // link does): the anchor wraps its one paragraph's phrasing, since an anchor cannot hold
  // blocks; an item of several blocks leads from its first paragraph.
  // An item's note is a paragraph of its own after its content, which the stylesheet sets
  // smaller and fainter; within an item which leads somewhere, it is not part of the link.
  private def item(item: Block.Item): Html of "li" =
    val note: List[Html of Flow] =
      if item.note.nil then Nil else List(P(`class` = cls(t"pyro-item-note"))(phrase(item.note)))

    def content: List[Html of Flow] = blocks(item.content) :: note

    def linked: Html of "li" =
      item.destination.lay(Li(content*)): destination =>
        Li(`class` = cls(t"pyro-lead"))((leading(destination, item.content) + note)*)

    item.action.lay(linked): action =>
      Li(id = action.id, `class` = cls(t"pyro-action"))(content*)

  private def leading(destination: Inline.Destination, content: List[Block]): List[Html of Flow] =
    content match
      case Block.Paragraph(phrasing) :: rest =>
        P(lead(destination, phrase(phrasing))) :: rest.map(block)

      case other =>
        other.map(block)

  // A collapsible node (one with `open` and children) is a `<details>` whose summary is its
  // label, named by its path for the script; any other node is its label and its children.
  //
  // A `hollow` node is the node alone: its list of children is there, and empty, for the
  // script to put in it the children the page already shows (`Graft`).
  def treeNode(node: Block.TreeNode, path: Text, hollow: Boolean = false): Html of "li" =
    val toned: Html of Phrasing =
      node.tone.lay(phrase(node.label)): tone => Span(`class` = toneClass(tone))(phrase(node.label))

    val label: Html of Phrasing =
      if node.action.present then toned
      else node.destination.lay(toned): destination => lead(destination, toned)

    val children: Html of Flow =
      if node.children.nil then Html.Fragment[Flow]()
      else if hollow then Ul()
      else
        val nodes = node.children.indexed.map: (child: Block.TreeNode, index: Ordinal) =>
          treeNode(child, t"$path.${index.n0}")

        Ul(nodes*)

    val plain: List[Html of Flow] = List(label, children)

    val content: List[Html of Flow] =
      if node.children.nil then plain
      else node.open.lay(plain): (open: Boolean) =>
        val style = cls(t"pyro-node")

        val details: Html of Flow =
          if open
          then Details(`class` = style, open = true, `data-node` = path)(Summary(label), children)
          else Details(`class` = style, `data-node` = path)(Summary(label), children)

        List(details)

    // A label which is code alone is shown bare, as a cell's is: in a tree of names, a box
    // around each says nothing.
    val bare: List[Name[CssClass]] = solitaryClass(node.label)

    node.action.lay(if bare.nil then Li(content*) else Li(`class` = bare)(content*)): action =>
      Li(id = action.id, `class` = cls(t"pyro-action") :: bare)(content*)

  private def vertexLabel(vertex: Block.Vertex): Html of Phrasing =
    val label: Html of Phrasing =
      vertex.tone.lay(phrase(vertex.label)): tone =>
        Span(`class` = toneClass(tone))(phrase(vertex.label))

    vertex.action.lay(label): action =>
      A(href = t"#", id = action.id, `class` = cls(t"pyro-action"))(label)

  // A deck: every slide a section, the first current, which is what a page without the script
  // shows; and its controls. The script shows one slide at a time and steps through them, or,
  // given a schedule, conducts them to it.
  def deck(leaves: List[Block.Leaf], schedule: List[Double] = Nil): Html of Flow =
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

    val content: Html of Flow = Html.Fragment(slides*)

    if schedule.nil then Div(`class` = cls(t"pyro-deck"), tabindex = t"0")(content, controls)
    else
      val times: Text = t"[${schedule.map(_.toString.tt).join(t",")}]"
      Div(`class` = cls(t"pyro-deck"), tabindex = t"0", `data-schedule` = times)(content, controls)

  private def table
    ( columns: List[Block.Column], rows: List[Block.Row], caption: Optional[List[Inline]] )
  :   Html of Flow =

    def columnClasses(column: Block.Column): List[Name[CssClass]] =
      val alignment: List[Name[CssClass]] =
        if column.alignment == Block.Alignment.Start then Nil
        else List(cls(t"pyro-align-${column.alignment.toString.tt.lower}"))

      val numeric: List[Name[CssClass]] = if column.numeric then List(cls(t"pyro-numeric")) else Nil
      val minor: List[Name[CssClass]] = if column.minor then List(cls(t"pyro-minor")) else Nil

      val sizing: List[Name[CssClass]] = column.sizing match
        case Block.Sizing.Stretch        => List(cls(t"pyro-stretch"))
        case Block.Sizing.Rigid          => List(cls(t"pyro-rigid"))
        case Block.Sizing.Paragraph      => Nil
        case Block.Sizing.Collapsible(_) => List(cls(t"pyro-collapsible"))

        case Block.Sizing.Fixed(percent) =>
          List(cls(t"pyro-share-${HtmlRenderer.share(percent).toString.tt}"))

      alignment + numeric + sizing + minor

    // A table with a fixed column is laid out by its columns' shares rather than its content.
    val fixed: Boolean =
      columns.exists: (column: Block.Column) =>
        column.sizing match
          case Block.Sizing.Fixed(_) => true
          case _                     => false

    val tableClasses: List[Name[CssClass]] =
      List(cls(t"pyro-table")) + (if fixed then List(cls(t"pyro-fixed")) else Nil)

    val headings = columns.map: (column: Block.Column) =>
      Th(`class` = columnClasses(column) + solitaryClass(column.title))(phrase(column.title))

    val head = Thead(Tr(headings*))

    // A row's note is a row of its own beneath it, whose rule the row above loses so that the
    // two read as one: one cell across every column, or in a table of fixed columns, the last
    // column's, so that the columns before it stand clear.
    val bodyRows = rows.bind: (row: Block.Row) =>
      val cells = columns.indexed.map: (column, index) =>
        val style: List[Name[CssClass]] = columnClasses(column)

        row.cells.at(index).lay(Td(`class` = style)(Html.Fragment[Phrasing]())): cell =>
          Td(`class` = style + solitaryClass(cell.content))(phrase(cell.content))

      val toned: List[Name[CssClass]] =
        row.tone.lay(Nil: List[Name[CssClass]]): tone => List(toneClass(tone))

      val classes: List[Name[CssClass]] =
        if row.note.nil then toned else toned :+ cls(t"pyro-noted")

      def plain: Html of "tr" =
        row.id.lay(Tr(`class` = classes)(cells*)): id => Tr(id = id, `class` = classes)(cells*)

      val main: Html of "tr" = row.action.lay(plain): action =>
        Tr(id = action.id, `class` = cls(t"pyro-action") :: classes)(cells*)

      if row.note.nil then List(main) else
        val note: Html of "tr" =
          if !fixed then
            Tr(`class` = cls(t"pyro-note-row") :: toned)
              ( Td(colspan = columns.size)(phrase(row.note)) )

          else
            val noteCells = columns.indexed.map: (column, index) =>
              val content: Html of Phrasing =
                if index.n0 == columns.size - 1 then phrase(row.note)
                else Html.Fragment[Phrasing]()

              Td(`class` = columnClasses(column))(content)

            Tr(`class` = cls(t"pyro-note-row") :: toned)(noteCells*)

        List(main, note)

    val body = Tbody(bodyRows*)

    val captioned: List[Html of "caption"] =
      caption.lay(Nil: List[Html of "caption"]): caption => List(Caption(phrase(caption)))

    Table(`class` = tableClasses)(Html.Fragment(captioned*), head, body)

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
          ( Code(`class` = cls(t"pyro-trace-class"))
              ( Em(t"${stack.component}."), Strong(stack.className) ),
            t": ",
            Span(`class` = cls(t"pyro-trace-message"))(phrase(stack.message)) )

      def frame(frame: Block.Trace.Frame, sameClass: Boolean, sameFile: Boolean): Html of "tr" =
        val (prefix, segment) = Block.Trace.split(frame.owner)
        val repeat: List[Name[CssClass]] = if sameClass then List(cls(t"pyro-repeat")) else Nil
        val fileRepeat: List[Name[CssClass]] = if sameFile then List(cls(t"pyro-repeat")) else Nil

        val plumbing: List[Name[CssClass]] =
          if frame.plumbing then List(cls(t"pyro-plumbing")) else Nil

        val rowClasses: List[Name[CssClass]] = cls(t"pyro-frame") :: plumbing
        val joined = Block.Trace.joined(frame)

        Tr(`class` = rowClasses)
          ( Td(`class` = cls(t"pyro-at"))(t"at"),
            Td(`class` = cls(t"pyro-owner") :: accentClass(frame.namespace) :: repeat)
              ( Span(`class` = cls(t"pyro-prefix"))(prefix), Strong(segment) ),
            Td(`class` = cls(t"pyro-dot"))(if joined then t"." else t"⌗"),
            Td(`class` = cls(t"pyro-method"))(frame.method),
            Td(`class` = cls(t"pyro-file") :: fileRepeat)(frame.file),
            Td(`class` = cls(t"pyro-line"))
              ( Span(`class` = cls(t"pyro-colon"))(t":"), frame.line.let(_.show).or(t"") ),
            Td(`class` = List(cls(t"pyro-source"), cls(t"pyro-collapsible")))
              ( Code(frame.code.or(t"")) ) )

      def origin(origin: Block.Trace.Origin): Html of "tr" =
        Tr(`class` = List(cls(t"pyro-frame"), cls(t"pyro-inlined")))
          ( Td(`class` = cls(t"pyro-at"))(t"↳"),
            Td(`class` = cls(t"pyro-owner"))(origin.owner.or(t"")),
            Td(`class` = cls(t"pyro-dot"))(origin.owner.lay(t"")(_ => t".")),
            Td(`class` = cls(t"pyro-method"))(origin.name.or(Phrases.inlinedFrom())),
            Td(`class` = cls(t"pyro-file"))(origin.file),
            Td(`class` = cls(t"pyro-line"))
              ( Span(`class` = cls(t"pyro-colon"))(t":"), origin.line.show ),
            Td(`class` = List(cls(t"pyro-source"), cls(t"pyro-collapsible")))
              ( Code(origin.code.or(t"")) ) )

      val rows: List[Html of "tr"] =
        stack.frames.fold((Nil: List[Html of "tr"], t"", t"")):
          case ((acc, lastClass, lastFile), frame0) =>
            val row = frame(frame0, frame0.owner == lastClass, frame0.file == lastFile)
            val subRows = frame0.inlined.map(origin).reverse
            (subRows + (row :: acc), frame0.owner, frame0.file)

        . _1.reverse

      val table = Table(`class` = cls(t"pyro-frames"))(Tbody(rows*))
      Section(`class` = cls(t"pyro-trace"))(heading, table)

    val sections = stacks.indexed.map: (stack0: Block.Trace.Stack, index: Ordinal) =>
      if index.n0 == 0 then stack(stack0)
      else
        Html.Fragment[Flow]
          ( P(`class` = cls(t"pyro-caused-by"))(Phrases.causedBy()), stack(stack0) )

    Html.Fragment[Flow](sections*)

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

        val cells =
          series0.values
          . map: (value: Double) =>
              glyphs.s.charAt(((value/most)*7).toInt.max(0).min(7)).toString.tt

          . join

        val sparkline = Span(`class` = cls(t"pyro-sparkline"))(cells)
        List[Entry](Dd(`class` = cls(t"pyro-series-value"))(sparkline))

      case Block.Chart.Kind.Bars | Block.Chart.Kind.Histogram =>
        series0.values.map: (value: Double) =>
          val figure = Amounts.figure(value)

          val entry: Entry =
            Dd(`class` = cls(t"pyro-series-value"))
              ( Meter(`class` = cls(t"pyro-meter"), value = value, max = most)(figure),
                t" ",
                Span(`class` = cls(t"pyro-figure"))(figure) )

          entry

    val entries: List[Entry] = series.bind: (series0: Block.Series) =>
      val label: Entry = Dt(`class` = cls(t"pyro-series-label"))(phrase(series0.label))
      label :: values(series0)

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
      case Status.Fraction(value) =>
        val permille: Text = t"${(value*1000).toInt}"
        Progress(`class` = cls(t"pyro-progress"), value = permille, max = t"1000")(t"")

      case Status.Indeterminate() => Progress(`class` = cls(t"pyro-progress"))(t"")

      case Status.Reckoning(done, total) =>
        def bare: Html of Flow =
          whatwg.Data(`class` = cls(t"pyro-reckoning"), value = done.toString.tt)(done.toString.tt)

        total.lay[Html of Flow](bare): total =>
          val progress = Progress(value = t"${done.toString}", max = t"${total.toString}")(t"")
          val count = Span(t" ${done.toString}/${total.toString}")
          Div(`class` = cls(t"pyro-reckoning"))(progress, count)

      case Status.Standing(standing) =>
        val style: Name[CssClass] = cls(t"pyro-standing-${standing.toString.tt.lower}")
        Span(`class` = List(cls(t"pyro-standing"), style))(standingGlyph(standing))

      case Status.Elapsed(seconds) =>
        val (n, u) = Amounts.scaled(seconds, t"s")
        Time(`class` = cls(t"pyro-elapsed"), datetime = duration(seconds))(t"$n $u")

      case Status.Remaining(seconds) =>
        val (n, u) = Amounts.scaled(seconds, t"s")
        Time(`class` = cls(t"pyro-remaining"), datetime = duration(seconds))(t"$n $u left")

      case Status.Steps(steps) =>
        val items = steps.map: (step: Step) =>
          val glyph = Span(`class` = cls(t"pyro-standing"))(standingGlyph(step.standing))
          val style = cls(t"pyro-standing-${step.standing.toString.tt.lower}")
          Li(`class` = style)(glyph, t" ", phrase(step.name))

        Ol(`class` = cls(t"pyro-steps"))(items*)

    caption.lay(Figure(`class` = classes)(body)): caption =>
      Figure(`class` = classes)(Figcaption(phrase(caption)), body)

  // An activity is a card: its title, its gauge and its state, all of it the link to its view
  // when it has one. The card's own id keeps it from the action's, which a navigation item may
  // carry too; its press sends the action named in `data-action`. Another node's activity
  // carries its origin as a label, is prefixed by the origin's key, since two nodes may use the
  // same id, and is never an action of this page: an internal destination from elsewhere is
  // no link at all.
  def activity(activity: Activity, origin: Optional[Origin] = Unset): Html of Flow =
    val id: Text =
      origin.lay(t"pyro-activity-${activity.id}"): origin =>
        t"pyro-activity-${origin.key}-${activity.id}"

    val title: Html of Flow = Div(`class` = cls(t"pyro-activity-title"))(phrase(activity.title))
    val state: Html of Flow = Div(`class` = cls(t"pyro-activity-state"))(phrase(activity.state))

    val label: Html of Flow = origin.lay(Html.Fragment[Flow]()): origin =>
      Div(`class` = cls(t"pyro-activity-origin"))(originLabel(origin))

    val body: Html of Flow = Html.Fragment(label, title, gauge(activity.status, Unset), state)

    val remote: List[Name[CssClass]] =
      if origin.present then List(cls(t"pyro-activity-remote")) else Nil

    val classes: List[Name[CssClass]] = cls(t"pyro-activity") :: remote

    activity.destination match
      case Inline.Destination.External(url) =>
        A(href = url, id = id, `class` = classes)(body)

      case Inline.Destination.Internal(action) if origin.absent =>
        val style = classes :+ cls(t"pyro-action")
        A(href = t"#", id = id, `class` = style, `data-action` = action.id)(body)

      case local: Inline.Destination.Local if origin.absent =>
        A(href = href(local), id = id, `class` = classes :+ cls(t"pyro-locate"))(body)

      case _ =>
        Div(id = id, `class` = classes)(body)

  private def originLabel(origin: Origin): Text =
    if origin.local then origin.tool else t"${origin.tool} · ${origin.machine}"

  def activities(activities: List[Activity]): Html of Flow =
    Html.Fragment(activities.map(activity(_))*)

  def remoteActivities(activities: List[(Origin, Activity)]): Html of Flow =
    Html.Fragment(activities.map { (origin, activity) => this.activity(activity, origin) }*)

  // The tools running, one item each: a link to its web front-end where it serves one, a
  // muted name otherwise; this process's own marked as current. A link to a tool of this
  // machine says so, for the script to re-address when the page is viewed from elsewhere.
  def toolLinks(origins: List[Origin]): Html of Flow =
    val items: List[Html of Flow] = origins.map(toolLink(_))
    Html.Fragment[Flow](items*)

  private def toolLink(origin: Origin): Html of Flow =
    val current: List[Name[CssClass]] =
      if origin.self then List(cls(t"pyro-menubar-current")) else Nil

    origin.url match
      case url: Text =>
        val style = cls(t"pyro-menubar-tool") :: current
        val local: Text = if origin.local then t"true" else t"false"
        A(href = url, `class` = style, `data-local` = local)(originLabel(origin))

      case _ =>
        val style = cls(t"pyro-menubar-tool") :: cls(t"pyro-menubar-unlinked") :: current
        Span(`class` = style)(originLabel(origin))

  private def standingGlyph(standing: Standing): Text = standing match
    case Standing.Pending   => t"·"
    case Standing.Running   => t"▶"
    case Standing.Succeeded => t"✓"
    case Standing.Failed    => t"✗"
    case Standing.Warned    => t"△"
    case Standing.Skipped   => t"‑"
