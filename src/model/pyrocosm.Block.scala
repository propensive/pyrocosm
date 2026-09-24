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
import soundness.{Language as _, *}

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

  // A range of one line of a code block to be marked: an error, a caution, a highlight or a
  // parameter, with an optional caption — fluence's annotated samples.
  object Note:
    enum Style:
      case Erroneous, Caution, Highlight, Param

  case class Note(line: Int, start: Int, end: Int, style: Note.Style, caption: Optional[Text] = Unset)

  case class TreeNode
    ( label:    List[Inline],
      children: List[TreeNode]   = Nil,
      tone:     Optional[Tone]   = Unset,
      action:   Optional[Action] = Unset )

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
      val edges: List[Edge] = List.from(dag.edges.map { (edge: (Vertex, Vertex)) => Edge(edge(0).id, edge(1).id) })
      Graph(List.from(dag.keys), edges)

    extension (graph: Graph)
      def dag: Dag[Vertex] =
        val byId: Map[Text, Vertex] = graph.vertices.map { (vertex: Vertex) => vertex.id -> vertex }.to[Map]

        // Acyclicity's graphs are still keyed by the stdlib's sets, so the prelude's are handed
        // over at its boundary.
        Dag(graph.vertices.to[Set].stdlib): (vertex: Vertex) =>
          graph.edges.filter(_.from == vertex.id).map { (edge: Edge) => byId(edge.to) }.sweep { case vertex: Vertex => vertex }
          . to[Set].stdlib

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
  case Code(language: Language, lines: List[Block.Line], notes: List[Block.Note] = Nil)
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
  case Group(content: List[Block])                      // a run of blocks which belong together
  case Output(text: Text, error: Boolean = false)      // captured standard output or error, verbatim
