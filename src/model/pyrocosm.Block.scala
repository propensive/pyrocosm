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

  case class Row
    ( cells:  List[Cell],
      tone:   Optional[Tone]   = Unset,
      action: Optional[Action] = Unset )

  case class Item(content: List[Block], action: Optional[Action] = Unset)

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

  // A code block's animation: identity and placement, never geometry. An `Atom` is a token
  // with an identity that holds across frames; a `Placement` puts an atom on a line of a
  // frame (flat, since a repeated TEL field cannot nest); a `Frame` may be captioned and may
  // say how long it holds when played. The front end measures where each atom is before and
  // after a step and animates the difference, so an inserted line, a reflowed line and a move
  // across lines need nothing of the model.
  case class Atom(id: Text, text: Text, accent: Token.Accent, role: Optional[Token.Role] = Unset)
  case class Placement(frame: Int, line: Int, atom: Text)
  case class Frame(index: Int, label: List[Inline] = Nil, hold: Optional[Double] = Unset)

  object Animation:
    // The atoms of each frame's lines, aligned across the frames: lines are matched by
    // similarity (dissonance's `evolve`), then the tokens within each run of matched lines, so
    // that a token which survives from one frame to the next keeps its identity, and a line
    // or a token which does not appears or departs. Amok's algorithm, with placements as its
    // result instead of presence classes.
    def of(frames: List[List[Line]], labels: List[List[Inline]] = Nil): Animation =
      def plain(line: Line): List[Token] = line.tokens.filter(_.accent != Token.Accent.Unparsed)

      // Lines are compared by their parsed tokens, so that whitespace and comments do not
      // decide; lines with none (unparsed text) by every token; two empty lines are alike.
      // Two lines are alike when one's tokens run contiguously within the other's (a line
      // grown or trimmed at either end, as `t"Hello"` becoming `val greeting = t"Hello"`),
      // or when fewer than half their tokens differ.
      def similar(left: Line, right: Line): Boolean =
        val (xs, ys): (List[Token], List[Token]) =
          if plain(left).nil && plain(right).nil then (left.tokens, right.tokens)
          else (plain(left), plain(right))

        def run(tokens: List[Token]): Text =
          tokens.map { (token: Token) => t"${token.accent.toString.tt}:${token.text}" }.join(t"\u0001", t"\u0001", t"\u0001")

        if xs.nil || ys.nil then xs.nil && ys.nil
        else run(ys).contains(run(xs)) || run(xs).contains(run(ys))
          || diff(Sequence(xs*), Sequence(ys*)).size.toDouble / (xs.size + ys.size) < 0.5

      val sameLine: (Line, Line) => Boolean = similar(_, _)
      val lineAtoms: List[Evolution.Atom[Line]] = evolve(frames, sameLine).sequence

      var counter: Int = 0

      def fresh(): Text =
        counter += 1
        t"a${counter.toString.tt}"

      var atoms: List[Atom] = Nil
      var placements: List[Placement] = Nil
      val cursors: scala.Array[Int] = new scala.Array[Int](frames.size)   // next line of each frame
      val lineIndex: scala.Array[Int] = new scala.Array[Int](frames.size) // lines placed so far

      // Each aligned line's tokens, aligned across the frames the line is in.
      lineAtoms.each: (lineAtom: Evolution.Atom[Line]) =>
        val present: List[Int] =
          frames.indexed.map(_(1).n0).filter { (f: Int) => lineAtom.presence.has(Ordinal.zerary(f)) }

        val versions: List[List[Token]] =
          present.map: (f: Int) =>
            val line: Line =
              frames.at(Ordinal.zerary(f)).let(_.at(Ordinal.zerary(cursors(f)))).or(Line(Nil))

            cursors(f) += 1
            line.tokens

        // Tokens by identity alone: given a similarity, `evolve` would pair a deletion with
        // an insertion as one atom whose text changed, which is a different token.
        val tokenAtoms: List[Evolution.Atom[Token]] = evolve(versions).sequence

        tokenAtoms.each: (tokenAtom: Evolution.Atom[Token]) =>
          val id: Text = fresh()
          atoms = Atom(id, tokenAtom.value.text, tokenAtom.value.accent, tokenAtom.value.role) :: atoms

          present.indexed.each: (f: Int, position: Ordinal) =>
            if tokenAtom.presence.has(position)
            then placements = Placement(f, lineIndex(f), id) :: placements

        present.each { (f: Int) => lineIndex(f) += 1 }

      val described: List[Frame] =
        frames.indexed.map { (_, index: Ordinal) => Frame(index.n0, labels.at(index).or(Nil)) }

      rejoin(Animation(atoms.reverse, described, placements.reverse))

    // A second pass, across lines: between consecutive frames, an atom which departs and an
    // atom which arrives with the same text and accent are one token which moved — a value
    // spread over several lines, say — and are merged, in order of appearance, so that the
    // front end carries it to its new line rather than fading it out and in.
    private def rejoin(animation: Animation): Animation =
      val byId: Map[Text, Atom] = animation.atoms.map { (atom: Atom) => atom.id -> atom }.to[Map]
      var placements: List[Placement] = animation.placements
      var merged: List[Text] = Nil   // the ids absorbed into another

      def same(left: Atom, right: Atom): Boolean = left.text == right.text && left.accent == right.accent

      def rename(from: Text, to: Text): Unit =
        placements = placements.map { (p: Placement) => if p.atom == from then p.copy(atom = to) else p }
        merged = from :: merged

      animation.frames.each: (frame: Frame) =>
        val f: Int = frame.index
        val now: List[Text] = placements.filter(_.frame == f).map(_.atom)
        val next: List[Text] = placements.filter(_.frame == f + 1).map(_.atom)
        var departing: List[Text] = now.filter { (id: Text) => !next.has(id) }
        val arriving: List[Text] = next.filter { (id: Text) => !now.has(id) }

        arriving.each: (arrival: Text) =>
          byId(arrival).let: (atom: Atom) =>
            departing.seek { (id: Text) => byId(id).let(same(_, atom)).or(false) }.let: (match0: Text) =>
              rename(arrival, match0)
              departing = departing.filter(_ != match0)

      Animation
        ( animation.atoms.filter { (atom: Atom) => !merged.has(atom.id) },
          animation.frames,
          placements,
          animation.autoplay,
          animation.loop )

  case class Animation
    ( atoms:      List[Atom],
      frames:     List[Frame],
      placements: List[Placement],
      autoplay:   Boolean = false,
      loop:       Boolean = false ):

    // The lines of one frame, as atoms in order.
    def lines(frame: Int): List[List[Atom]] =
      val byId: Map[Text, Atom] = atoms.map { (atom: Atom) => atom.id -> atom }.to[Map]
      val placed: List[Placement] = placements.filter(_.frame == frame)
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

  // `open`, when present, makes a node with children collapsible, and says whether it starts
  // open or closed; a frontend with no way to toggle (the terminal) shows every node.
  case class TreeNode
    ( label:    List[Inline],
      children: List[TreeNode]    = Nil,
      tone:     Optional[Tone]    = Unset,
      action:   Optional[Action]  = Unset,
      open:     Optional[Boolean] = Unset )

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

enum Block:
  case Paragraph(content: List[Inline])
  case Heading(level: Int, content: List[Inline])
  case Listing(ordered: Boolean, items: List[Block.Item])
  case Quotation(content: List[Block])
  // A rule stands between what precedes and follows it, or marks an edge: `Above` is drawn
  // low in its row (`⎽`), a line above the content that follows; `Below` high (`⎺`), a line
  // beneath the content before it. Both are what a transcript keeps of a submitted line's
  // frame.
  case Rule(side: Block.Side = Block.Side.Between)
  // `start`, when present, is the source line number (from one) of the first line: the lines
  // are then numbered in the margin, as an excerpt of a file is and a sample is not.
  // `animation`, when present, is the block as a sequence of frames, of which `lines` is the
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
