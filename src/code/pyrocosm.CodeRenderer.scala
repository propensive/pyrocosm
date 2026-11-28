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

import soundness.*

import attributives.textAttributive
import dysasymptotics.linearSize
import htmlDoms.whatwg
import htmlDoms.whatwg.*
// `Em` and `Span` are imported by name, so they mean the HTML elements and not the names the
// umbrella also offers: hiding them from the umbrella works too, but `-Wunused` thinks it unused.
import htmlDoms.whatwg.{Em, Span}
import nomenclature.CssClass.nominative
import sortingAlgorithms.timsort
import textMetrics.uniformMetric

// Code as HTML, and nothing else: a block of code, plain or animated, its notes, its path and
// its alternatives. It is the part of the web renderer which a page that is not a Pyrocosm
// application's can use by itself, with `CodeStyles` and the script `CodeScript` holds; the
// web module's `HtmlRenderer` is one of these, so every tool draws code the same way.
object CodeRenderer:
  def accentClass(accent: Token.Accent): Name[CssClass] =
    unsafely(Name[CssClass](t"pyro-accent-${accent.toString.tt.lower}"))

  def cls(name: Text): Name[CssClass] = unsafely(Name[CssClass](name))

  // A class list, omitted when it is empty: honeycomb's own writes `class=""`.
  given classes: List[Name[CssClass]] is Attributive to Whatwg.CssClassList =
    (key, value) => if value.nil then Unset else (key, value.join(t" "))

  // `data-atom="a12"` on a token of an animated code block: its identity across the frames,
  // by which the script moves it from where it was to where it is.
  given atomId: ("data-atom" is Attribute of Whatwg.Textual in Whatwg) = Whatwg.globalAttribute()

  // `tabindex` on an animated block, so that it takes the arrow keys when focused.
  given tabIndex: ("tabindex" is Attribute of Whatwg.Textual in Whatwg) = Whatwg.globalAttribute()

  // `data-step="2"` on a step of an animated block, the index of the slide it shows; and
  // `aria-current="step"` on the step showing now.
  given dataStep: ("data-step" is Attribute of Whatwg.Textual in Whatwg) = Whatwg.globalAttribute()

  given ariaCurrent: ("aria-current" is Attribute of Whatwg.Textual in Whatwg) =
    Whatwg.globalAttribute()

  // `data-alternative="1"` on a tab of a code block with alternatives, and on the alternative
  // it shows.
  given dataAlternative: ("data-alternative" is Attribute of Whatwg.Textual in Whatwg) =
    Whatwg.globalAttribute()


  // A text as a JSON string literal, for the animation the renderer embeds.
  def jsonText(text: Text): Text =
    val builder = StringBuilder()
    builder.append('"')

    var index: Int = 0

    while index < text.s.length do
      val char: Char = text.s.charAt(index)

      char match
        case '"'                => builder.append("\\\"")
        case '\\'               => builder.append("\\\\")
        case '\n'               => builder.append("\\n")
        case '\r'               => builder.append("\\r")
        case '\t'               => builder.append("\\t")
        case char if char < ' ' => builder.append(f"\\u${char.toInt}%04x")
        case char               => builder.append(char)

      index += 1

    builder.append('"')
    builder.toString.tt

  // A renderer of code for a page of its own making: phrasing — a note's caption, a slide's
  // label, a tab's — is the words and their emphasis, and a link out; anything richer is
  // given as its plain text. The `prefix` goes before every local destination's link.
  class Standalone(prefix: Text = t"") extends CodeRenderer:
    def phrase(content: List[Inline]): Html of Phrasing = Html.Fragment(content.map(inline)*)

    protected def href(local: Inline.Destination.Local): Text =
      local.fragment.lay(t"$prefix${local.path}"): fragment => t"$prefix${local.path}#$fragment"

    private def inline(node: Inline): Html of Phrasing = node match
      case Inline.Textual(text)       => text
      case Inline.Phrase(content)     => phrase(content)
      case Inline.Emphasis(content)   => Em(phrase(content))
      case Inline.Strong(content)     => Strong(phrase(content))
      case Inline.Break()             => Br
      case Inline.Snippet(_, tokens0) => Code(`class` = cls(t"pyro-code"))(tokens0.map(token)*)

      case Inline.Link(Inline.Destination.External(url), content) =>
        A(href = url, `class` = List(cls(t"pyro-link"), cls(t"pyro-external")))(phrase(content))

      case Inline.Link(local: Inline.Destination.Local, content) =>
        A(href = href(local), `class` = List(cls(t"pyro-link"), cls(t"pyro-locate")))
          ( phrase(content) )

      case other => Inline.plain(List(other))

trait CodeRenderer:
  // The attribute givens are imported wholesale: a macro summons them, which `-Wunused` does not
  // see, so it would call any of them imported by name unused.
  import CodeRenderer.{accentClass, cls, jsonText, given}

  // Phrasing as HTML: a note's caption, a slide's label, the label of an alternative's tab.
  def phrase(content: List[Inline]): Html of Phrasing

  // The `href` of a local destination, which a linked token leads to.
  protected def href(local: Inline.Destination.Local): Text


  // A linked token is an anchor carrying the token's own classes, so that it stays one element
  // and the stylesheet and the editor see it as any other token.
  def token(token: Token): Html of Phrasing =
    val classes: List[Name[CssClass]] =
      val extra: List[Name[CssClass]] =
        if token.role == Token.Role.Binding then List(cls(t"pyro-binding")) else Nil

      accentClass(token.accent) :: extra

    // The hint is the element's title, which the browser shows as a tooltip.
    val title: Text = token.hint.or(t"")

    def plain: Html of Phrasing =
      token.hint.lay(Span(`class` = classes)(token.text)): hint =>
        Span(`class` = classes, title = hint)(token.text)

    token.link.lay[Html of Phrasing](plain): (link: Inline.Destination) =>
      link match
        case Inline.Destination.External(url) =>
          if token.hint.absent then A(href = url, `class` = classes)(token.text)
          else A(href = url, `class` = classes, title = title)(token.text)

        case Inline.Destination.Internal(action) =>
          val style = classes :+ cls(t"pyro-action")

          if token.hint.absent then A(href = t"#", id = action.id, `class` = style)(token.text)
          else A(href = t"#", id = action.id, `class` = style, title = title)(token.text)

        case local: Inline.Destination.Local =>
          val style = classes :+ cls(t"pyro-locate")

          if token.hint.absent then A(href = href(local), `class` = style)(token.text)
          else A(href = href(local), `class` = style, title = title)(token.text)

  // Each run of chained tokens as Amok rendered a qualified name: every linked segment after the
  // first wraps everything before it, its separators and itself in a `pyro-qualified` span, and
  // the first is wrapped alone, so that each link is a direct child of exactly the span which
  // holds the name up to it — which the stylesheet underlines while that link is pointed at:
  // `com`, then `com.example`, then `com.example.Foo`. A run of one token is the token alone.
  protected def qualified(items: List[(Token, Html of Phrasing)]): List[Html of Phrasing] =
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

  // A block of code: plain, or animated; and, with a path or alternatives, under a header.
  def code(code: Block.Code): Html of Flow =
    def single(language: pyrocosm.Language, lines: List[Block.Line], notes: List[Block.Note],
               animation: Optional[Block.Animation], id: Optional[Text]): Html of Flow =
      animation.lay(plainCode(language, lines, notes, code.start, id)): (animation: Block.Animation) =>
        animatedCode(language, animation, id)

    val own: Html of Flow = single(code.language, code.lines, code.notes, code.animation, code.id)

    if code.path.absent && code.alternatives.nil then own
    else codeGroup(code.path, code.alternatives, own): alternative =>
      single(alternative.language, alternative.lines, alternative.notes, alternative.animation, Unset)

  // The rows of a code block, each ended by a newline but the last: every line, behind its
  // number when `start` says which line of its source the first is; and beneath a line, a row
  // for each caret or captioned note ending on it, as `marker` draws them.
  private def codeRows(lines: List[Block.Line], notes: List[Block.Note], start: Optional[Int])
  :   List[Html of Phrasing] =

    val digits: Int = start.lay(0): (first: Int) => (first + lines.size - 1).show.length

    def number(value: Optional[Int]): Html of Phrasing =
      if start.absent then Html.Fragment[Phrasing]()
      else
        val text: Text = value.lay(t" "*digits): (value: Int) => value.show.pad(digits, Rtl)
        Span(`class` = cls(t"pyro-lineno"))(text)

    val rows: List[Html of Phrasing] =
      lines.indexed.bind: (line: Block.Line, index: Ordinal) =>
        val length: Int = line.tokens.fold(0): (total: Int, token: Token) =>
          total + token.text.length

        val first: Html of Phrasing =
          Html.Fragment[Phrasing]
            ( number(start.let(_ + index.n0)), codeLine(line, notes, true, index.n0) )

        val text: Text = line.tokens.map { (token: Token) => token.text }.join

        val beneath: List[Html of Phrasing] =
          markers(notes, index.n0, length).bind: (note: Block.Note) =>
            marker(note, index.n0, length, indentation(text), number(Unset))

        first :: beneath

    val count: Int = rows.size

    rows.indexed.map: (row: Html of Phrasing, index: Ordinal) =>
      if index.n0 == count - 1 then row else Html.Fragment[Phrasing](row, t"\n")

  // A diagnostic without a width, an error or a caution at a position: it is shown as a mark on
  // the one character at its position, where any other caret is a pointer beneath its line.
  private def pointed(note: Block.Note): Boolean =
    note.caret
    && (note.style == Block.Note.Style.Erroneous || note.style == Block.Note.Style.Caution)

  // The columns of line `index`, of `length` characters, which a note marks: its range, or,
  // for a diagnostic without a width, the character at its position — the last, at the end
  // of the line.
  private def marked(note: Block.Note, index: Int, length: Int): Optional[(Int, Int)] =
    note.range(index, length).let: (from: Int, to: Int) =>
      if !pointed(note) || length == 0 then (from, to) else
        val at: Int = from.min(length - 1)
        (at, at + 1)

  // The notes which end on line `index` and are drawn beneath it: a caret which is a pointer,
  // or a captioned note.
  private def markers(notes: List[Block.Note], index: Int, length: Int): List[Block.Note] =
    notes.filter: (note: Block.Note) =>
      note.lastLine == index && ((note.caret && !pointed(note)) || !note.caption.nil)

  // The space which leads a line's text, which a diagnostic's caption is set after.
  private def indentation(text: Text): Text = text.s.takeWhile(_.isWhitespace).tt

  // The rows beneath a line for one of its notes, spaced with the line's own columns, since a
  // `pre` keeps them; `margin` is what stands where the line's number does, so they align. A
  // diagnostic's span is already marked by its own style, and one without a width by the mark
  // of the character at its position, so it has no row of marks; its caption has a row of its
  // own, smaller, at the line's indentation wherever on the line the span is. Any other note
  // has `╱╲` or `‾` under each marked column, then its caption, on one row.
  private def marker
    ( note:   Block.Note,
      index:  Int,
      length: Int,
      indent: Text,
      margin: Html of Phrasing = Html.Fragment[Phrasing]() )
  :   List[Html of Phrasing] =

    val range: (Int, Int) = note.range(index, length).or((0, 0))
    val pad: Int = if note.caret then (range(0) - 1).max(0) else range(0)
    val marks: Text = if note.caret then t"╱╲" else t"‾"*(range(1) - range(0)).max(1)
    val style = List(cls(t"pyro-marker"), cls(t"pyro-marker-${note.style.toString.tt.lower}"))
    val marked: Html of Phrasing = Span(`class` = cls(t"pyro-marks"))(marks)

    note.style match
      case Block.Note.Style.Erroneous | Block.Note.Style.Caution =>
        if note.caption.nil then Nil else
          val text: Html of Phrasing =
            Span(`class` = cls(t"pyro-marker-caption"))(phrase(note.caption))

          List(Span(`class` = style)(margin, indent, text))

      case _ =>
        val caption: Html of Phrasing =
          if note.caption.nil then Html.Fragment[Phrasing]()
          else Html.Fragment[Phrasing](t" ", phrase(note.caption))

        List(Span(`class` = style)(margin, t" "*pad, marked, caption))

  // The class of a block any of whose notes focuses: the stylesheet fades the rest of it.
  private def focusing(notes: List[Block.Note]): List[Name[CssClass]] =
    if notes.exists(_.style == Block.Note.Style.Focus) then List(cls(t"pyro-focusing")) else Nil

  // A code block with a header: its path, and the tabs of its alternatives, in a strip above
  // the code; each alternative is rendered whole (an animated one with its steps), the first
  // shown and the others hidden until their tab is chosen, by the script. Without
  // alternatives, the block's own rendering sits beneath the header.
  private def codeGroup
    ( path:         Optional[Text],
      alternatives: List[Block.Alternative],
      own:          Html of Flow )
    ( render: Block.Alternative => Html of Flow )
  :   Html of Flow =

    val named: List[Html of Flow] =
      path.lay(Nil): (path: Text) => List(Span(`class` = cls(t"pyro-code-path"))(path))

    val tabs: List[Html of Flow] =
      if alternatives.nil then Nil
      else
        val items: List[Html of Ol.Transport] =
          alternatives.indexed.map: (alternative: Block.Alternative, index: Ordinal) =>
            val current: List[Name[CssClass]] =
              if index.n0 == 0 then List(cls(t"pyro-current")) else Nil

            Li
              ( `class` = cls(t"pyro-code-tab") :: current,
                `data-alternative` = index.n0.toString.tt )
              ( Button(`type` = t"button")(phrase(alternative.label)) )

        List(Ol(`class` = cls(t"pyro-code-tabs"))(items*))

    val header: Html of Flow = Div(`class` = cls(t"pyro-code-header"))((named + tabs)*)

    val body: List[Html of Flow] =
      if alternatives.nil then List(own)
      else
        alternatives.indexed.map: (alternative: Block.Alternative, index: Ordinal) =>
          val current: List[Name[CssClass]] =
            if index.n0 == 0 then List(cls(t"pyro-current")) else Nil

          Div
            ( `class` = cls(t"pyro-alternative") :: current,
              `data-alternative` = index.n0.toString.tt )
            ( render(alternative) )

    Div(`class` = cls(t"pyro-code-group"))((header :: body)*)

  def plainCode
    ( language: pyrocosm.Language,
      lines:    List[Block.Line],
      notes:    List[Block.Note],
      start:    Optional[Int],
      id:       Optional[Text] = Unset )
  :   Html of Flow =

    val numbered: List[Name[CssClass]] = if start.present then List(cls(t"pyro-numbered")) else Nil

    val classes: List[Name[CssClass]] =
      cls(t"pyro-codeblock") :: cls(t"pyro-language-${language.name}") :: (numbered + focusing(notes))

    val code: Html of Phrasing = Code(codeRows(lines, notes, start)*)

    id.lay(Pre(`class` = classes)(code)): (id: Text) => Pre(id = id, `class` = classes)(code)

  // An animated block: its first slide as lines of atoms, each carrying its id, with that
  // slide's annotations and label and in its language; its controls; and the animation as
  // JSON in a hidden element the script reads. Slide 0 is what a page without the script
  // shows.
  def animatedCode
    ( language: pyrocosm.Language, animation: Block.Animation, id: Optional[Text] = Unset )
  :   Html of Flow =

    val first: Optional[Block.Slide] = animation.slides.head
    val shown: pyrocosm.Language = first.let(_.language).or(language)

    val classes: List[Name[CssClass]] =
      List(cls(t"pyro-codeblock"), cls(t"pyro-language-${shown.name}")) + focusing(first.lay(Nil)(_.notes))

    val rows: List[Html of Phrasing] = animatedRows(animation.lines(0), first.lay(Nil)(_.notes))

    // The steps: every slide in order, its number circled beside its label, the first
    // current. Read down, they say what the animation does; the script makes a click on a step
    // other than the current one animate the block straight to its slide.
    val steps: List[Html of Ol.Transport] = animation.slides.map: (slide: Block.Slide) =>
      val index: Text = slide.index.toString.tt
      val number: Text = (slide.index + 1).toString.tt
      val description: Html of Phrasing =
        Span(`class` = cls(t"pyro-animation-description"))(phrase(slide.label))

      if slide.index == 0 then
        val circle: Html of Phrasing =
          Button
            ( `class` = cls(t"pyro-animation-number"),
              `type` = t"button",
              `aria-current` = t"step" )
            ( number )

        Li(`class` = List(cls(t"pyro-animation-step"), cls(t"pyro-current")), `data-step` = index)
          ( circle, description )
      else
        val circle: Html of Phrasing =
          Button(`class` = cls(t"pyro-animation-number"), `type` = t"button")(number)

        Li(`class` = cls(t"pyro-animation-step"), `data-step` = index)(circle, description)

    val parts: List[Html of Flow] =
      List
        ( Pre(`class` = classes)(Code(rows*)),
          Ol(`class` = cls(t"pyro-animation-steps"))(steps*),
          Pre(`class` = cls(t"pyro-animation-data"))(animationJson(language, animation)) )

    id.lay(Div(`class` = cls(t"pyro-animated"), tabindex = t"0")(parts*)): (id: Text) =>
      Div(id = id, `class` = cls(t"pyro-animated"), tabindex = t"0")(parts*)

  // One slide's lines as rows of atoms — block-level lines (the stylesheet's rule), so no
  // newline stands between them — with the frame's notes laid over them: an atom any part of
  // which a note covers is marked whole, since an atom is never split (its identity is what
  // the animation moves), and a caret or captioned note has its row beneath its line, as a
  // plain block draws it.
  private def animatedRows(lines: List[List[Block.Atom]], notes: List[Block.Note])
  :   List[Html of Phrasing] =

    lines.indexed.bind: (atoms: List[Block.Atom], index: Ordinal) =>
      val length: Int = atoms.fold(0): (total: Int, atom: Block.Atom) => total + atom.text.length

      val ranges: List[(Int, Int, Block.Note)] =
        notes.bind: (note: Block.Note) =>
          marked(note, index.n0, length).lay(Nil): (range: (Int, Int)) =>
            List((range(0), range(1), note))

      var offset: Int = 0

      // Each atom, with the note which covers some of it, and the part of it that note covers.
      val spans: List[Html of Phrasing] =
        atoms.map: (atom: Block.Atom) =>
          val start: Int = offset
          val end: Int = offset + atom.text.length

          val covering: Optional[(Int, Int, Block.Note)] =
            ranges.seek: (range: (Int, Int, Block.Note)) =>
              range(0) < range(1) && range(0) < end && range(1) > start

          offset = end

          covering.lay(atomSpan(atom)): (from: Int, to: Int, note: Block.Note) =>
            atomSpan(atom, note, (from.max(start) - start, to.min(end) - start))

      val row: Html of Phrasing = Span(`class` = cls(t"pyro-line"))(spans*)

      val text: Text = atoms.map { (atom: Block.Atom) => atom.text }.join

      val beneath: List[Html of Phrasing] =
        markers(notes, index.n0, length).bind: (note: Block.Note) =>
          marker(note, index.n0, length, indentation(text))

      row :: beneath

  // An atom, and the mark of a note on it: on the whole of it, as classes of its own, or on
  // the part of it the note covers (`part`, as columns of its text), as a span within it.
  def atomSpan
    ( atom: Block.Atom,
      note: Optional[Block.Note]  = Unset,
      part: Optional[(Int, Int)]  = Unset )
  :   Html of Phrasing =

    val binding: List[Name[CssClass]] =
      if atom.role == Token.Role.Binding then List(cls(t"pyro-binding")) else Nil

    val marks: List[Name[CssClass]] =
      note.lay(Nil): (note: Block.Note) =>
        List(cls(t"pyro-marked"), cls(t"pyro-marked-${note.style.toString.tt.lower}"))

    val length: Int = atom.text.length
    val whole: Boolean = part.lay(true) { (from: Int, to: Int) => from <= 0 && to >= length }

    val classes: List[Name[CssClass]] =
      cls(t"pyro-atom") :: accentClass(atom.accent) :: (binding + (if whole then marks else Nil))

    note.lay(Span(`class` = classes, `data-atom` = atom.id)(atom.text)): (note: Block.Note) =>
      val title: Text = Inline.plain(note.caption)

      if whole then Span(`class` = classes, `data-atom` = atom.id, title = title)(atom.text)
      else
        val (from, to) = part.or((0, length))
        def piece(start: Int, end: Int): Text = atom.text.s.substring(start, end).nn.tt
        val inner: Html of Phrasing = Span(`class` = cls(t"pyro-mark") :: marks)(piece(from, to))

        Span(`class` = classes, `data-atom` = atom.id, title = title)
          ( piece(0, from), inner, piece(to, length) )

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
        val id: Text = jsonText(atom.id)
        val text: Text = jsonText(atom.text)
        val accent: Text = jsonText(atom.accent.toString.tt.lower)
        t"""{"id":$id,"text":$text,"accent":$accent,"binding":$binding}"""

      . join(t",")

    def notes(frame: Block.Slide): Text =
      frame.notes.map: (note: Block.Note) =>
        val line: Text = note.line.toString.tt
        val start: Text = note.start.toString.tt
        val end: Text = note.end.toString.tt
        val last: Text = note.last.lay(t"null")(_.toString.tt)
        val style: Text = jsonText(note.style.toString.tt.lower)

        val caption: Text =
          if note.caption.nil then t"\"\"" else jsonText(phrase(note.caption).show)

        val head: Text = t"""{"line":$line,"start":$start,"end":$end"""
        t"""$head,"last":$last,"style":$style,"caption":$caption}"""

      . join(t",")

    val slides: Text =
      animation.slides.map: (frame: Block.Slide) =>
        val lines: Text = ids(frame.lines)
        val label: Text = if frame.label.nil then t"\"\"" else jsonText(phrase(frame.label).show)

        val frameLanguage: Text =
          frame.language.lay(t"null"): (language: pyrocosm.Language) => jsonText(language.name)

        val index: Text = frame.index.toString.tt
        val hold: Text = optional(frame.hold)
        val at: Text = optional(frame.at)
        val head: Text = t"""{"index":$index,"label":$label,"hold":$hold,"at":$at"""
        t"""$head,"language":$frameLanguage,"lines":[$lines],"notes":[${notes(frame)}]}"""

      . join(t",")

    val placements: Text =
      animation.placements.map: (placement: Block.Placement) =>
        val slide: Text = placement.slide.toString.tt
        t"[$slide,${placement.line.toString.tt},${jsonText(placement.atom)}]"

      . join(t",")

    val transitions: Text =
      animation.transitions.map: (transition: Block.Transition) =>
        val from: Text = transition.from.toString.tt
        val to: Text = transition.to.toString.tt
        val kind: Text = jsonText(transition.kind.toString.tt.lower)
        val duration: Text = optional(transition.duration)
        val departing: Text = ids(transition.departing)
        val arriving: Text = ids(transition.arriving)

        val moved: Text =
          transition.moved
          . map: (move: Block.Transition.Move) =>
              t"[${jsonText(move.atom)},${jsonText(move.from)},${jsonText(move.to)}]"

          . join(t",")

        val removed: Text = ids(transition.removed)
        val inserted: Text = ids(transition.inserted)
        val head: Text = t"""{"from":$from,"to":$to,"kind":$kind,"duration":$duration,"""
        val middle: Text = t"""$head"departing":[$departing],"arriving":[$arriving],"""
        t"""$middle"moved":[$moved],"removed":[$removed],"inserted":[$inserted]}"""

      . join(t",")

    val name: Text = jsonText(language.name)
    val autoplay: Text = animation.autoplay.toString.tt
    val loop: Text = animation.loop.toString.tt
    val head: Text = t"""{"language":$name,"atoms":[$atoms],"slides":[$slides],"""
    val body: Text = t"""$head"placements":[$placements],"transitions":[$transitions],"""
    t"""$body"autoplay":$autoplay,"loop":$loop}"""

  // One line of code, the `index`th of its block, with the segments of the notes which touch
  // it laid over its tokens; a caret marks no segment. The line ends in a newline unless it is
  // the `last`.
  def codeLine(line: Block.Line, notes: List[Block.Note], last: Boolean, index: Int = 0)
  :   Html of Phrasing =

    val length: Int = line.tokens.fold(0): (total: Int, token: Token) => total + token.text.length

    val ranges: List[(Int, Int, Block.Note)] =
      notes.bind: (note: Block.Note) =>
        marked(note, index, length).lay(Nil): (range: (Int, Int)) => List((range(0), range(1), note))

    // Each token, cut at the boundaries where a segment begins or ends inside it, with a
    // running offset into the line.
    var offset: Int = 0
    var rendered: List[(Token, Html of Phrasing)] = Nil

    line.tokens.each: (token: Token) =>
      val end = offset + token.text.length
      val ends: List[Int] = ranges.bind: (range: (Int, Int, Block.Note)) => List(range(0), range(1))

      val cuts: List[Int] =
        (offset :: end :: ends).filter { (cut: Int) => cut >= offset && cut <= end }.distinct.sort

      var pieces: List[Html of Phrasing] = Nil

      cuts.zip(cuts.tail.or(Nil)).each: (from: Int, to: Int) =>
        val text = token.text.s.substring(from - offset, to - offset).nn.tt
        val base = this.token(token.copy(text = text))

        val covering: Optional[Block.Note] =
          ranges
          . seek: (range: (Int, Int, Block.Note)) =>
              range(0) <= from && range(1) >= to && range(0) < range(1)

          . let(_(2))

        val styled: Html of Phrasing =
          covering.lay(base): (note: Block.Note) =>
            val style = cls(t"pyro-note-${note.style.toString.tt.lower}")
            Span(`class` = style, title = Inline.plain(note.caption))(base)

        pieces = styled :: pieces

      // The token's pieces as one element, so that a qualified name chains whole tokens.
      rendered = (token, Html.Fragment(pieces.reverse*)) :: rendered
      offset = end

    val ordered: List[Html of Phrasing] = qualified(rendered.reverse)

    Span(`class` = cls(t"pyro-line"))
      ( Html.Fragment(ordered*), if last then Html.Fragment[Phrasing]() else t"\n" )
