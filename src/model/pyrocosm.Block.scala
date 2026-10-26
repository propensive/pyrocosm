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

// Excluded from the umbrella: `Language` (cosmopolite), which would outrank this package's own
// definitions, since a wildcard import beats a package member declared in another file.
import soundness.{Codecs as _, Language as _, *}

import dysasymptotics.{linearAccess, linearSize}

// Flow content: the things which occur one after another down a page or a pane. Comparable to
// Markdown's block nodes, and richer: tables with layout semantics, records, notices, trees,
// graphs, charts and gauges. Every block is data; nothing here says how it looks.
object Block:
  enum Side:
    case Above, Below, Between

  // How a table column may give way when the table is squeezed, mirroring escritoire`s
  // `Columnar` strategies on the terminal; the web maps them to `1fr`, `max-content` and
  // container queries.
  enum Sizing:
    case Stretch                                        // absorbs the spare width; at most one per table
    case Rigid                                          // never narrower than its content: figures
    case Paragraph                                      // wraps
    case Collapsible(priority: Double)                  // vanishes below a threshold; higher goes first

  enum Alignment:
    case Start, End, Center, Justify

  case class Column
    ( title:     List[Inline],
      alignment: Alignment = Alignment.Start,
      sizing:    Sizing    = Sizing.Paragraph,
      numeric:   Boolean   = false )

  // A row with an `Action` is selectable: a click on the web, focus and Enter in the terminal.
  // This is how master/detail interfaces are built, with no layout-level machinery.
  // A cell and a line are records of their own, not nested lists: a repeated TEL field cannot
  // nest, so `List[List[_]]` would flatten on the wire.
  case class Cell(content: List[Inline])
  object Line:
    // Tokens as lines: a token's text may hold newlines, each of which ends a line; the parts
    // between keep the token's accent and role. An empty part adds no token, so an empty line
    // is a line of none.
    def split(tokens: List[Token]): List[Line] =
      var done: List[Line] = Nil
      var current: List[Token] = Nil

      tokens.each: (token: Token) =>
        token.text.cut(t"\n").indexed.each: (part: Text, index: Ordinal) =>
          if index.n0 > 0 then
            done = Line(current.reverse) :: done
            current = Nil

          if part != t"" then current = token.copy(text = part) :: current

      (Line(current.reverse) :: done).reverse

  case class Line(tokens: List[Token])

  // A row's `id`, like a heading's, is an anchor: the fragment of a local destination which
  // targets it, verbatim, so it must not look like a handle or a `pyro-` id. A row with an
  // action is identified by the action, and the anchor is ignored.
  case class Row
    ( cells:  List[Cell],
      tone:   Optional[Tone]   = Unset,
      action: Optional[Action] = Unset,
      id:     Optional[Text]   = Unset )

  // An item either acts (a press the application handles) or leads to a destination; the
  // action wins when both are given.
  case class Item
    ( content:     List[Block],
      action:      Optional[Action]             = Unset,
      destination: Optional[Inline.Destination] = Unset )

  case class Entry(key: List[Inline], value: List[Block])

  // A mark on a code block: an error, a caution, a highlight or a parameter, with a caption
  // which may carry highlighted code of its own. `line` and `start` are the (zero-based) line
  // and column where it begins; it ends at column `end` of `line`, or of `last` when it spans
  // lines. A note of no width — `start == end` on one line — is a *caret*: it points at a
  // position rather than marking a segment, which is how a compiler reports an error at a
  // point. A note of width is underlined; a caret is drawn beneath its position.
  object Note:
    enum Style:
      case Erroneous, Caution, Highlight, Param

    // The note for a source `Span`, in whichever of denominative's modes it was recorded: a
    // `Line` span is one line's segment, `Lines` and `Area` spans reach across lines. An
    // `Offset` span names characters of the whole text, not a line, and an empty span nothing,
    // so neither yields a note. Spans count from zero, as notes do.
    def of(span: Span, style: Style, caption: List[Inline] = Nil): Optional[Note] =
      span.startLine.let: (startLine: Ordinal) =>
        val endLine: Int = span.endLine.lay(startLine.n0)(_.n0)
        val startColumn: Int = span.startColumn.lay(0)(_.n0)

        // An area ends on a column of its last line; a run of whole lines, at the end of the
        // last, which `Int.MaxValue` stands for until the line's length is known.
        val endColumn: Int =
          if span.mode == Span.Mode.Lines then Int.MaxValue else span.endColumn.lay(startColumn)(_.n0)

        val last: Optional[Int] = if endLine > startLine.n0 then endLine else Unset
        Note(startLine.n0, startColumn, endColumn, style, caption, last)

  case class Note
    ( line:    Int,
      start:   Int,
      end:     Int,
      style:   Note.Style,
      caption: List[Inline]  = Nil,
      last:    Optional[Int] = Unset ):

    def caret: Boolean = last.absent && start == end
    def lastLine: Int = last.or(line)
    def covers(index: Int): Boolean = index >= line && index <= lastLine

    // The segment of line `index`, of `length` characters, which the note marks: from its
    // start on its first line, to its end on its last, and whole lines between. `Unset` on a
    // line it does not touch. The columns are clamped to the line, so a note which reaches
    // beyond the line (an `end` of `Int.MaxValue`, or stale coordinates) marks what there is.
    def range(index: Int, length: Int): Optional[(Int, Int)] =
      if !covers(index) then Unset else
        val from: Int = if index == line then start else 0
        val to: Int = if index == lastLine then end else length
        (from.min(length), to.min(length).max(from.min(length)))

    // The same note, `lines` lines earlier and `columns` columns further right: how a note on
    // a whole file becomes one on an excerpt of it, and one on a prompt's text a note on a
    // transcript's indented copy.
    def shifted(lines: Int = 0, columns: Int = 0): Note =
      copy(line = line - lines, start = start + columns, end = end + columns, last = last.let(_ - lines))

  object Code:
    // The lines around `notes` cut from the whole of a source, `context` lines either side,
    // numbered from where in the source they came; the notes move with them. Nothing is cut
    // when there are no notes: the whole is shown, numbered from its first line.
    def excerpt(language: Language, lines: List[Line], notes: List[Note], context: Int = 1): Code =
      val first: Int = notes.map(_.line).minimum.let { (line: Int) => (line - context).max(0) }.or(0)

      val limit: Int =
        notes.map(_.lastLine).maximum.let { (line: Int) => (line + context + 1).min(lines.size) }.or(lines.size)

      val shown: List[Line] = lines.skip(first).keep(limit - first)
      Code(language, shown, notes.map(_.shifted(lines = first)), first + 1)

  // A code block's animation, as an intermediate form between what an author writes and what
  // a renderer shows: identity, placement and change, never geometry. An `Atom` is a token
  // with an identity that holds across slides; a `Placement` puts an atom on a line of a
  // slide; a `Slide` is one complete state of the block — its lines, its annotations (marks
  // which add to the tokens or change how they are shown, without changing them), its label,
  // its language, and its timing; and a `Transition` is what happens between two slides —
  // its kind, its duration, and what departs, arrives, moves and is inserted or removed.
  // A renderer shows a slide alone, or plays a transition: the browser measures where each
  // atom is before and after and animates the difference, the terminal shows the first slide.
  case class Atom(id: Text, text: Text, accent: Token.Accent, role: Optional[Token.Role] = Unset)
  case class Placement(slide: Int, line: Int, atom: Text)

  object Slide:
    // What is said of a slide before it is aligned — `Animation.of` adds the index and the
    // line names, which alignment decides — and how it is arrived at: the kind of transition
    // into it, and that transition's duration when the renderer's own will not do.
    case class Detail
      ( label:      List[Inline]       = Nil,
        hold:       Optional[Double]   = Unset,
        at:         Optional[Double]   = Unset,
        language:   Optional[Language] = Unset,
        notes:      List[Note]         = Nil,
        transition: Transition.Kind    = Transition.Kind.Morph,
        duration:   Optional[Double]   = Unset )

  // `lines` names the slide's lines in order, by an identity that holds across slides (a
  // line that grew, shrank or moved keeps its id; a new line has a new one), so that a front
  // end can tell a token joining a line that remains from one reaching a new line. `hold` is
  // how long a played animation rests on the slide, in seconds; `at` puts it on the page's
  // one clock, which a slide without `at` follows by its predecessor's `hold` (1.5 s unless
  // said) when any slide of its block has one; `language` is the slide's own, the block's
  // otherwise; and `notes` are its annotations, in the coordinates of its own lines.
  case class Slide
    ( index:    Int,
      label:    List[Inline]       = Nil,
      hold:     Optional[Double]   = Unset,
      lines:    List[Text]         = Nil,
      at:       Optional[Double]   = Unset,
      language: Optional[Language] = Unset,
      notes:    List[Note]         = Nil )

  object Transition:
    // How one slide becomes the next. A morph keeps every token that survives and moves it
    // to its place, fading out what departs and in what arrives; a fade lets the whole slide
    // go before the next appears; an edit types the change with a cursor, as an editor would.
    // A renderer plays what it can — one that cannot play an edit shows it as a morph.
    enum Kind:
      case Morph, Fade, Edit

    // An atom on one line of the slide before and another of the slide after, by line name.
    case class Move(atom: Text, from: Text, to: Text)

  // The change from slide `from` to slide `to`: the atoms which depart, arrive and move
  // between lines, and the lines removed and inserted, by name; `duration` is in seconds,
  // the renderer's own when unset. Derived by `Animation.of` from the slides, so that it
  // never disagrees with them, and so that what a step does is known before it is rendered.
  case class Transition
    ( from:      Int,
      to:        Int,
      kind:      Transition.Kind       = Transition.Kind.Morph,
      duration:  Optional[Double]      = Unset,
      departing: List[Text]            = Nil,
      arriving:  List[Text]            = Nil,
      moved:     List[Transition.Move] = Nil,
      removed:   List[Text]            = Nil,
      inserted:  List[Text]            = Nil )

  object Animation:
    // The atoms of each slide's lines, aligned across the slides as one sequence of tokens
    // per slide, the lines' ends among them (every line's, the last included, so that each
    // has one), by dissonance's `evolve` with no notion of similarity: a token survives from
    // one slide to the next when the diff keeps it, so the surviving tokens of any two slides
    // are in the same order in both — a token is removed and another inserted, never kept
    // and reordered. A line is named by the token that ends it, so a line remains, however it
    // grew, shrank or moved, exactly when its end survives.
    // The transition into each slide but the first follows: what the alignment says departs,
    // arrives and changes line, and which lines go and come.
    def of(frames: List[List[Line]], details: List[Slide.Detail] = Nil): Animation =
      val break: Token = Token(t"\n", Token.Accent.Unparsed)

      val sequences: List[List[Token]] =
        frames.map: (lines: List[Line]) =>
          lines.map(_.tokens).join(List(break)) + List(break)

      val evolved: List[Evolution.Atom[Token]] = evolve(sequences).sequence

      var counter: Int = 0

      def fresh(prefix: Text): Text =
        counter += 1
        t"$prefix${counter.toString.tt}"

      val ids: List[Text] = evolved.map { (atom: Evolution.Atom[Token]) => fresh(if atom.value == break then t"l" else t"a") }

      val atoms: List[Atom] =
        evolved.zip(ids).filter(_(0).value != break).map: (atom: Evolution.Atom[Token], id: Text) =>
          Atom(id, atom.value.text, atom.value.accent, atom.value.role)

      // Each slide's placements and line names, walking its atoms in sequence: a line's
      // tokens are placed on the line so far, and its end names it.
      var placements: List[Placement] = Nil
      var lineNames: List[(Int, Text)] = Nil

      frames.indexed.each: (_, index: Ordinal) =>
        val f: Int = index.n0
        var line: Int = 0

        evolved.zip(ids).each: (atom: Evolution.Atom[Token], id: Text) =>
          if atom.presence.has(index) then
            if atom.value == break then
              lineNames = (f, id) :: lineNames
              line += 1
            else placements = Placement(f, line, id) :: placements

      val described: List[Slide] =
        frames.indexed.map: (_, index: Ordinal) =>
          val names: List[Text] = lineNames.reverse.filter(_(0) == index.n0).map(_(1))
          val detail: Slide.Detail = details.at(index).or(Slide.Detail())
          Slide(index.n0, detail.label, detail.hold, names, detail.at, detail.language, detail.notes)

      val placed: List[Placement] = placements.reverse

      // The line, by name, each atom of a slide is on.
      def lineOf(slide: Slide): Map[Text, Text] =
        placed.filter(_.slide == slide.index).map: (placement: Placement) =>
          placement.atom -> slide.lines.skip(placement.line).prim.or(t"?")
        . to[Map]

      val transitions: List[Transition] =
        described.indexed.bind: (slide: Slide, index: Ordinal) =>
          described.skip(index.n0 - 1).prim.lay(Nil): (previous: Slide) =>
            if index.n0 == 0 then Nil else
              val before: Map[Text, Text] = lineOf(previous)
              val after: Map[Text, Text] = lineOf(slide)
              val ids: List[Text] = atoms.map(_.id)
              val detail: Slide.Detail = details.at(index).or(Slide.Detail())

              val moved: List[Transition.Move] =
                ids.bind: (id: Text) =>
                  val from: Optional[Text] = before.at(id)
                  val to: Optional[Text] = after.at(id)

                  from.lay(Nil: List[Transition.Move]): (from: Text) =>
                    to.lay(Nil: List[Transition.Move]): (to: Text) =>
                      if from == to then Nil else List(Transition.Move(id, from, to))

              List(Transition
                ( previous.index,
                  slide.index,
                  detail.transition,
                  detail.duration,
                  ids.filter { (id: Text) => before.defines(id) && !after.defines(id) },
                  ids.filter { (id: Text) => after.defines(id) && !before.defines(id) },
                  moved,
                  previous.lines.filter(!slide.lines.has(_)),
                  slide.lines.filter(!previous.lines.has(_)) ))

      Animation(atoms, described, placed, transitions)

  case class Animation
    ( atoms:       List[Atom],
      slides:      List[Slide],
      placements:  List[Placement],
      transitions: List[Transition] = Nil,
      autoplay:    Boolean          = false,
      loop:        Boolean          = false ):

    // The lines of one slide, as atoms in order.
    def lines(slide: Int): List[List[Atom]] =
      val byId: Map[Text, Atom] = atoms.map { (atom: Atom) => atom.id -> atom }.to[Map]
      val placed: List[Placement] = placements.filter(_.slide == slide)
      val count: Int = placed.map(_.line).maximum.let(_ + 1).or(0)

      def recur(line: Int, acc: List[List[Atom]]): List[List[Atom]] =
        if line >= count then acc.reverse else
          val row: List[Atom] =
            placed.filter(_.line == line).map { (placement: Placement) => byId(placement.atom) }
            . sweep { case atom: Atom => atom }

          recur(line + 1, row :: acc)

      recur(0, Nil)

  // Where a diagnostic points: a file, and a line and column within it, counted from one as a
  // compiler and an editor count them.
  case class Location(path: Text, line: Optional[Int] = Unset, column: Optional[Int] = Unset):
    def text: Text = line.lay(path) { (line: Int) => column.lay(t"$path:$line") { (column: Int) => t"$path:$line:$column" } }

  object Location:
    // The location of a zero-based `Span` in `path`, or the file alone for an empty span.
    def of(path: Text, span: Span): Location =
      Location(path, span.startLine.let(_.n1), span.startColumn.let(_.n1))

  object Diagnostic:
    // A TEL error as a diagnostic: its message as the message; and, given the text it was read
    // from, an excerpt of the lines around its span, highlighted as TEL, with the span marked.
    // A parse error carries its span itself; a validation error's is on the `Tel.Focus` accrued
    // with it, which the caller passes as `span`. Every TEL error is an error, so its tone is
    // failure.
    def of
      ( error:  Tel.Error,
        path:   Optional[Text] = Unset,
        source: Optional[Text] = Unset,
        span:   Optional[Span] = Unset )
    :   Diagnostic =

      val located: Span = span.or(error.span)
      val location: Optional[Location] = path.let(Location.of(_, located))
      val message: List[Block] = List(Paragraph(Inline.message(error.message)))

      val excerpt: List[Block] = source.lay(Nil): (source: Text) =>
        val notes: List[Note] = Note.of(located, Note.Style.Erroneous).let(List(_)).or(Nil)
        List(Code.excerpt(Language.Tel, TelSyntax.lines(source), notes))

      Diagnostic(Tone.Failure, location, message, excerpt)

  // One slide of a deck: the blocks it shows at once.
  case class Leaf(content: List[Block])

  // `open`, when present, makes a node with children collapsible, and says whether it starts
  // open or closed; a frontend with no way to toggle (the terminal) shows every node.
  case class TreeNode
    ( label:       List[Inline],
      children:    List[TreeNode]              = Nil,
      tone:        Optional[Tone]              = Unset,
      action:      Optional[Action]            = Unset,
      open:        Optional[Boolean]           = Unset,
      destination: Optional[Inline.Destination] = Unset )

  // A node of a graph, identified by `id`, and an edge between two ids. A graph is stored as
  // its vertices and edges (which serialise plainly); `Graph.of` and `Graph#dag` convert to and
  // from acyclicity's `Dag`, which supplies the ordering and reachability operations.
  case class Vertex
    ( id:     Text,
      label:  List[Inline],
      tone:   Optional[Tone]   = Unset,
      action: Optional[Action] = Unset )

  case class Edge(from: Text, to: Text)

  object Graph:
    def of(dag: Dag[Vertex]): Graph =
      val edges: List[Edge] = dag.edges.map { (edge: (Vertex, Vertex)) => Edge(edge(0).id, edge(1).id) }.to[List]
      Graph(dag.nodes.to[List], edges)

    extension (graph: Graph)
      // A `Dag` is acyclic by construction, so a graph whose edges close a cycle raises.
      def dag: Dag[Vertex] raises Dag.Error =
        val byId: Map[Text, Vertex] = graph.vertices.map { (vertex: Vertex) => vertex.id -> vertex }.to[Map]

        Dag(graph.vertices.to[Set]): (vertex: Vertex) =>
          graph.edges.filter(_.from == vertex.id).map { (edge: Edge) => byId(edge.to) }.sweep { case vertex: Vertex => vertex }
          . to[Set]

  object Chart:
    enum Kind:
      case Sparkline, Bars, Histogram

  case class Series(label: List[Inline], values: List[Double], tone: Optional[Tone] = Unset)

  // A stack trace as data, digression's `StackTrace` reduced to what a rendering needs, so that
  // the model neither serialises a fulminate message nor depends on how digression resolves a
  // frame. Both renderers lay it out as digression's own terminal rendering does.
  object Trace:
    // One level of inlining beneath a frame, from the classfile's SMAP: detail about the frame
    // above, not a frame of its own. `owner` and `name` are the inline definition, when resolved.
    case class Origin
      ( file:  Text,
        line:  Int,
        owner: Optional[Text] = Unset,
        name:  Optional[Text] = Unset,
        code:  Optional[Text] = Unset )

    // `namespace` is the frame's package, which colours it; `owner` is the class to show (the
    // chain of source definitions when resolved, else the demangled class name, with a leading
    // `Ξ` for an object), and `method` the method to show. A `resolved` frame's source definition
    // was found, so its owner and method are joined by `.` rather than `⌗`; a `plumbing` frame
    // was synthesised by the compiler, and recedes.
    case class Frame
      ( namespace: Text,
        owner:     Text,
        method:    Text,
        file:      Text,
        line:      Optional[Int]  = Unset,
        code:      Optional[Text] = Unset,
        resolved:  Boolean        = false,
        plumbing:  Boolean        = false,
        inlined:   List[Origin]   = Nil )

    // One exception of the chain: the root, then each cause in turn.
    case class Stack(component: Text, className: Text, message: List[Inline], frames: List[Frame])

    // The stack's namespaces in order of first appearance, each numbered from zero: what both
    // renderers colour by, so a package takes the same accent in either medium.
    def accents(stack: Stack): Map[Text, Int] =
      stack.frames.map(_.namespace).distinct.indexed.map { (namespace, index) => namespace -> index.n0 }.to[Map]

    // The last segment of an owner, `Ξ` marking an object, and everything before it.
    private def pivot(owner: Text): Int = owner.s.lastIndexOf(".")
    private def segment(owner: Text): Text = owner.s.substring(pivot(owner) + 1).nn.tt

    // An owner as its prefix (with its trailing dot, or empty) and its last segment, less the
    // `Ξ` which marks an object: what a renderer shows subdued, and what it shows in the accent.
    def split(owner: Text): (Text, Text) =
      val prefix = if pivot(owner) >= 0 then owner.s.substring(0, pivot(owner) + 1).nn.tt else t""
      val last = segment(owner)
      (prefix, if last.starts(t"Ξ") then last.skip(1) else last)

    // Whether a frame's owner and method are joined by a dot: a resolved frame names a chain of
    // source definitions, and an object's method is its member; otherwise `⌗` marks an unresolved
    // JVM class and method.
    def joined(frame: Frame): Boolean = frame.resolved || segment(frame.owner).starts(t"Ξ")

    def of(stackTrace: StackTrace): Trace =
      def origin(inlined: StackTrace.Frame.Inlined): Origin =
        Origin
          ( inlined.file, inlined.line, inlined.source.let(_.owner), inlined.source.let(_.name),
            inlined.source.let(_.code).or(Unset) )

      def frame(frame: StackTrace.Frame): Frame =
        Frame
          ( frame.method.prefix, frame.displayClass, frame.displayMethod, frame.file, frame.line,
            frame.source.let(_.code).or(Unset), frame.source.present,
            frame.source.lay(false)(_.kind.plumbing), frame.inlined.map(origin) )

      def stack(stackTrace: StackTrace): Stack =
        Stack
          ( stackTrace.component, stackTrace.className, Inline.message(stackTrace.message),
            stackTrace.frames.map(frame) )

      def chain(stackTrace: StackTrace, done: List[Stack]): List[Stack] =
        val next = stack(stackTrace) :: done
        stackTrace.cause.lay(next.reverse) { cause => chain(cause, next) }

      Trace(chain(stackTrace, Nil))

  def paragraph(text: Text): Block = Paragraph(Inline.text(text))

  // The TEL codecs, anchored as those of `Inline` are.
  given telEncodable: Block is Tel.Encodable = Codecs.blockEncodable
  given telDecodable: Block is Tel.Decodable = Codecs.blockDecodable

  // The schema BinTEL is written under, anchored here as the codecs are (see `Inline`).
  given telSchematic: Block is stratiform.TelSchematic over Tels.Type = blockTelSchematic

enum Block:
  case Paragraph(content: List[Inline])
  case Heading(level: Int, content: List[Inline], id: Optional[Text] = Unset)  // `id`: an anchor, as `Row.id`
  case Listing(ordered: Boolean, items: List[Block.Item])
  case Quotation(content: List[Block])
  // A rule stands between what precedes and follows it, or marks an edge: `Above` is drawn
  // low in its row (`⎽`), a line above the content that follows; `Below` high (`⎺`), a line
  // beneath the content before it. Both are what a transcript keeps of a submitted line's
  // frame.
  case Rule(side: Block.Side = Block.Side.Between)
  // `start`, when present, is the source line number (from one) of the first line: the lines
  // are then numbered in the margin, as an excerpt of a file is and a sample is not.
  // `animation`, when present, is the block as a sequence of slides, of which `lines` is the
  // first, so a renderer that cannot animate shows a correct block.
  case Code
    ( language:  Language,
      lines:     List[Block.Line],
      notes:     List[Block.Note]          = Nil,
      start:     Optional[Int]             = Unset,
      animation: Optional[Block.Animation] = Unset )
  case Table(columns: List[Block.Column], rows: List[Block.Row], caption: Optional[List[Inline]] = Unset)
  case Record(entries: List[Block.Entry], title: Optional[List[Inline]] = Unset)
  case Notice(tone: Tone, title: Optional[List[Inline]], content: List[Block])
  case Disclosure(summary: List[Inline], content: List[Block], open: Boolean = false)
  case Image(source: Text, alt: Text)
  case Figure(figure: pyrocosm.Figure)                  // a live drawing, revised in place
  case Tree(roots: List[Block.TreeNode])

  // Slides, shown one at a time and stepped through: on the web, by the arrow keys, space and
  // the page keys, an animated block on a slide stepping through its frames before the deck
  // moves on; in the terminal, one after another.
  case Deck(leaves: List[Block.Leaf])
  case Graph(vertices: List[Block.Vertex], edges: List[Block.Edge])
  case Chart(kind: Block.Chart.Kind, series: List[Block.Series])
  case Gauge(status: Status, caption: Optional[List[Inline]] = Unset)
  case Trace(stacks: List[Block.Trace.Stack])           // an exception's stack trace and its causes
  // A report of something wrong, or worth knowing, at a place: a compiler's error or warning, a
  // linter's finding, a TEL document's fault. The `tone` is its severity; the `message` says
  // what; the `excerpt` shows the lines it concerns, with the place marked — a `Code` block
  // with `start`, typed as blocks because a derived codec cannot name one variant of the sum
  // it is deriving; `detail` is whatever follows — an explanation, a suggestion, a related
  // place.
  case Diagnostic
    ( tone:     Tone,
      location: Optional[Block.Location],
      message:  List[Block],
      excerpt:  List[Block] = Nil,
      detail:   List[Block] = Nil )
  case Group(content: List[Block])                      // a run of blocks which belong together
  case Output(text: Text, error: Boolean = false)      // captured standard output or error, verbatim
