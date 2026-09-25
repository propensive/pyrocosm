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

// Excluded from the umbrella: `Control` (coaxial), `Glyph` (phoenicia), `Language` (cosmopolite),
// `Standing` (ultimatum), `Step` (ultimatum), `Token` (harlequin), which would outrank this
// package's own definitions, since a wildcard import beats a package member declared in another
// file.
import soundness.{Control as _, Filter as _, Glyph as _, Language as _, Standing as _, Status as _, Step as _, Token as _, Tool as _, *}

import clavichord.Keypress
import probably.TestEvent

import dysasymptotics.{linearAccess, linearSize}
import harlequin.Scala
import termcapDefinitions.xtermTrueColorTermcap
import strategies.throwUnsafely
import formatting.indentedCssFormatting
import tableStyles.thickTableStyle
import errorDiagnostics.emptyDiagnostics
import charEncoders.utf8Encoder
import textMetrics.uniformMetric
import palettes.solarizedDarkGaugePalette
import discriminables.jsonByKindDiscriminable
import formatting.compactJsonFormatting
import logging.silentLogging

case class Person(name: Text, age: Int)

// A kind of metadata, as fume's benchmark summary will be: a record with text that has newlines
// and non-ASCII characters in it, since the note body must survive git's own handling.
case class Bench(name: Text, mean: Double, remark: Text)
given benchRecordable: Bench is Recordable = Recordable[Bench](t"bench")

// The shape of the model's `Inline` and `Block`: a sum whose variants recurse through a `List` of
// the sum itself. Both codecs must derive it in place, with no hand-anchored instance.
enum Node derives CanEqual:
  case Leaf(text: Text)
  case Branch(children: List[Node])

enum Direct derives CanEqual:
  case Leaf
  case Branch(left: Direct, value: Int, right: Direct)

// Message types for the remote channel tests: two layouts, so a protocol mismatch can be shown.
enum Ping derives CanEqual:
  case Hello(text: Text, count: Int)
  case Count(count: Int)

enum Pong derives CanEqual:
  case Hello(text: Text)

object Tests extends Suite(m"Pyrocosm tests"):
  // The plain-`java` entry point the release script and CI use (`java -cp <test jar>
  // pyrocosm.Tests`): since Soundness 0.65.0 a `Suite` has no `main` of its own — the host,
  // normally fume, drives it through `invoke` — so this prints one line per completed test and
  // exits with the suite's status (0 = every test passed, 1 = failures, 2 = the suite threw).
  // `fume run -c <test jar>` remains the full experience, with the rendered report.
  def main(args: Array[String]): Unit =
    val tally = Tally()
    val out = java.lang.System.out.nn

    val status = invoke(t"", event => event match
      case TestEvent.TestCompleted(test, _, _, outcome, _, _) =>
        tally.record(outcome.outcome == t"pass" || outcome.outcome == t"aspire-pass")
        out.println(t"[${outcome.outcome}] ${test.path.join(t" / ")}".s)

      case TestEvent.DetailMessage(_, message) =>
        out.println(t"    $message".s)

      case TestEvent.DetailCompare(_, expected, found, _) =>
        out.println(t"    expected: $expected".s)
        out.println(t"    found:    $found".s)

      case TestEvent.RunTerminated(error, _, _) =>
        out.println(t"suite threw: ${error.components.map(_.message).join(t"; ")}".s)

      case _ => ())

    out.println(t"${tally.passed} passed, ${tally.failed} failed".s)
    java.lang.System.exit(status)

  def run(): Unit =
    test(m"a live cell's assignment publishes the value"):
      val live = Live(1)
      live() = 2
      live()
    . assert(_ == 2)

    test(m"a live cell's assignment wakes every bound frontend"):
      val live = Live(t"a")
      var wakes = 0
      live.bindWake(() => wakes += 1)
      live.bindWake(() => wakes += 1)
      live() = t"b"
      wakes
    . assert(_ == 2)

    test(m"append extends a live list cell"):
      // `List(1, 2)` is a `List[Int] & Populated`; the cell must be typed as the plain list.
      val live = Live(List(1, 2): List[Int])
      live.append(3)
      live()
    . assert(_ == List(1, 2, 3))

    test(m"a hint is found by its type"):
      Hints(hints.Proportion(0.3), hints.web.Card)[hints.Proportion]
    . assert(_ == hints.Proportion(0.3))

    test(m"an absent hint is Unset"):
      Hints(hints.Proportion(0.3))[hints.terminal.MaxRows]
    . assert(_ == Unset)

    test(m"a renderer-specific hint is found by that renderer"):
      Hints(hints.web.Card, hints.terminal.Occupancy.Inline)[hints.terminal.Occupancy]
    . assert(_ == hints.terminal.Occupancy.Inline)

    test(m"text exhibits as itself"):
      t"hello".exhibit
    . assert(_ == Inline.Textual(t"hello"))

    test(m"a case class derives a record titled by its type"):
      val exhibit: Inline | Block = Person(t"Simon", 72).exhibit

      exhibit match
        case Block.Record(entries, title) =>
          entries.size == 2 && title == Inline.text(t"Person")

        case _ =>
          false
    . assert(_ == true)

    test(m"a list of values exhibits as a listing"):
      val exhibit: Inline | Block = (List(1, 2, 3): List[Int]).exhibit

      exhibit match
        case Block.Listing(false, items) => items.size == 3
        case _                           => false
    . assert(_ == true)

    test(m"markdown converts node for node"):
      val exhibit: Block = Parser.parse(t"# Title\n\nSome *emphasis* here.").exhibit

      exhibit match
        case Block.Group(Block.Heading(1, _) :: Block.Paragraph(content) :: Nil) =>
          content.exists:
            case Inline.Emphasis(_) => true
            case _                  => false

        case _ =>
          false
    . assert(_ == true)

    test(m"every live cell is reachable from the interface"):
      val run = Action(t"run")

      val panel =
        Panel
          ( Panel.Id(t"main"),
            Panel.Role.Primary,
            Unset,
            Live(Nil: List[Block]),
            controls = List(Control.Button(Inline.text(t"Run"), run)) )

      val verbose = Control.Toggle(Toggle(), Inline.text(t"Verbose"))
      Interface(Inline.text(t"Gallery"), List(panel), List(verbose)).cells.size
    . assert(_ == 3)

    val tree: Node =
      Node.Branch(List(Node.Leaf(t"a"), Node.Branch(List(Node.Leaf(t"b"), Node.Leaf(t"c")))))

    test(m"a sum recursive through a list round-trips as JSON"):
      tree.in[Json].show.read[Json].as[Node]
    . assert(_ == tree)

    test(m"a sum recursive through a list round-trips as TEL"):
      tree.in[Tel].as[Node]
    . assert(_ == tree)

    test(m"a sum recursive through a list round-trips as TEL text"):
      tree.in[Tel].show.read[Tel].as[Node]
    . assert(_ == tree)

    val emptyToken: Block = Block.Code(Language.Plain, List(Block.Line(List(Token.plain(t"x"))), Block.Line(List(Token.plain(t"")))))

    // An empty text is written as a compound with no atom (see `textTelEncodable`).
    test(m"a code line with an empty token round-trips as TEL text"):
      emptyToken.in[Tel].show.read[Tel].as[Block]
    . assert(_ == emptyToken)

    val emptyCells: Block = Block.Table(List(Block.Column(Inline.text(t""))), List(Block.Row(List(Block.Cell(List(Inline.Textual(t"")))))))

    test(m"empty phrasing and cells round-trip as TEL text"):
      emptyCells.in[Tel].show.read[Tel].as[Block]
    . assert(_ == emptyCells)

    // The derivation tutorial's own example of a type that "cannot be derived in place": direct
    // recursion, with no collection or `Optional` between the sum and itself.
    val direct: Direct = Direct.Branch(Direct.Leaf, 1, Direct.Branch(Direct.Leaf, 2, Direct.Leaf))

    test(m"a directly-recursive sum round-trips as JSON"):
      direct.in[Json].show.read[Json].as[Direct]
    . assert(_ == direct)

    test(m"a directly-recursive sum round-trips as TEL"):
      direct.in[Tel].show.read[Tel].as[Direct]
    . assert(_ == direct)

    val keypresses: List[Keypress] =
      List
        ( Keypress.CharKey('a'), Keypress.CharKey(' '), Keypress.CharKey('+'), Keypress.Enter,
          Keypress.FunctionKey(12), Keypress.Ctrl('C'), Keypress.Ctrl(Keypress.Left),
          Keypress.Shift(Keypress.Enter), Keypress.Alt(Keypress.Shift(Keypress.Tab)),
          Keypress.Meta(Keypress.Ctrl(Keypress.Alt(Keypress.Up))), Keypress.EscapeSeq('x') )

    test(m"every keypress parses back from its rendering"):
      keypresses.filter { (keypress: Keypress) => Keypresses.parse(keypress.show) != keypress }
    . assert(_ == Nil)

    test(m"malformed keypress text does not parse"):
      List(t"", t"a", t"[⌃]", t"[⌃]+[⌃]", t"[⇧]+[a]", t"[⌃]+[é]").map { (text: Text) => Keypresses.parse(text) }
    . assert(_.all(_ == Unset))

    val run = Action(t"run")

    val drawing: Text =
      t"""<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"><defs><linearGradient id="grad"/></defs><g id="series-0"><rect fill="url(#grad)"/></g><use href="#series-0"/></svg>"""

    val figure = Figure(Inline.text(t"a chart"), drawing)

    // A trace of two exceptions: a resolved frame with an inlining beneath it, a second frame of
    // the same class and file, a plumbing frame of an object with no line, and a cause.
    val trace: Block =
      Block.Trace:
        List
          ( Block.Trace.Stack
              ( t"java.lang", t"IllegalStateException", Inline.text(t"bad state"),
                List
                  ( Block.Trace.Frame
                      ( t"pyrocosm", t"pyrocosm.Tests", t"run", t"Tests.scala", 42, t"val x = 1", true, false,
                        List(Block.Trace.Origin(t"Inline.scala", 7, t"pyrocosm.Inline", t"text", t"List(Textual(text))")) ),
                    Block.Trace.Frame(t"pyrocosm", t"pyrocosm.Tests", t"main", t"Tests.scala", 1234),
                    Block.Trace.Frame(t"scala.runtime", t"scala.runtime.Ξfunction1", t"apply", t"function1.scala", Unset, Unset, false, true) ) ),
            Block.Trace.Stack
              ( t"java.io", t"IOException", Inline.text(t"disk"),
                List(Block.Trace.Frame(t"java.io", t"java.io.File", t"open", t"File.java", 9)) ) )

    val rich: Block =
      Block.Group:
        List
          ( Block.Heading(2, Inline.text(t"Results")),
            Block.Paragraph:
              List
                ( Inline.Textual(t"Press "),
                  Inline.Keystroke(Keypress.Ctrl('C')),
                  Inline.Textual(t" to stop; "),
                  Inline.Emphasis(Inline.text(t"emphasis")),
                  Inline.Strong(Inline.text(t"strong")),
                  Inline.Toned(Tone.Success, Inline.text(t"ok")),
                  Inline.Link(Inline.Destination.Internal(run), Inline.text(t"again")),
                  Inline.Math(unsafely(Ergo.parse(t"(x↗2 + y↗2)"))),
                  Inline.Symbol(Glyph.Check),
                  Inline.Reference(t"a1b2c3"),
                  Inline.Amount(0.0123, t"s"),
                  Inline.Figure(3.5, 2),
                  Inline.Icon(t"/icon.svg", t"an icon"),
                  Inline.Break() ),
            Block.Table
              ( List
                  ( Block.Column(Inline.text(t"Name")),
                    Block.Column(Inline.text(t"Time"), Block.Alignment.End, Block.Sizing.Rigid, true) ),
                List(Block.Row(List(Block.Cell(Inline.text(t"parse")), Block.Cell(List(Inline.Amount(0.5, t"s")))), Tone.Success, run)),
                Inline.text(t"Benchmarks") ),
            Block.Code(Language.Scala, List(Block.Line(List(Token(t"val", Token.Accent.Keyword), Token.plain(t" x")))),
                List(Block.Note(0, 0, 3, Block.Note.Style.Highlight))),
            Block.Notice(Tone.Warning, Unset, List(Block.Rule())),
            Block.Graph
              ( List(Block.Vertex(t"a", Inline.text(t"A")), Block.Vertex(t"b", Inline.text(t"B"))),
                List(Block.Edge(t"b", t"a")) ),
            Block.Gauge(Status.Reckoning(3, 10), Inline.text(t"progress")),
            Block.Gauge(Status.Elapsed.of(1.5*Second)),
            Block.Gauge(Status.Steps(List(Step(Inline.text(t"compile"), Standing.Running)))),
            Block.Listing(true, List(Block.Item(List(Block.paragraph(t"one")), run))),
            Block.Record(List(Block.Entry(Inline.text(t"key"), List(Block.paragraph(t"value")))), Inline.text(t"R")),
            Block.Disclosure(Inline.text(t"more"), List(Block.Image(t"x.png", t"an image"), Block.Figure(figure)), true),
            Block.Chart(Block.Chart.Kind.Sparkline, List(Block.Series(Inline.text(t"s"), List(1.0, 2.0)))),
            trace )

    test(m"a block with every node round-trips as TEL"):
      rich.in[Tel].show.read[Tel].as[Block]
    . assert(_ == rich)

    test(m"a graph converts to a dag and back"):
      val graph: Block.Graph = Block.Graph
        ( List(Block.Vertex(t"a", Inline.text(t"A")), Block.Vertex(t"b", Inline.text(t"B"))),
          List(Block.Edge(t"b", t"a")) )

      Block.Graph.of(graph.dag).edges
    . assert(_ == List(Block.Edge(t"b", t"a")))

    test(m"a list of case classes exhibits as a table with a numeric column"):
      val exhibit: Inline | Block = (List(Person(t"Simon", 72), Person(t"Ada", 36)): List[Person]).exhibit

      exhibit match
        case Block.Table(columns, rows, _) =>
          columns.map(_.numeric) == List(false, true) && rows.size == 2
        case _ =>
          false
    . assert(_ == true)

    test(m"highlighted source exhibits as a code block"):
      val exhibit: Block = Scala.highlight(t"val x = 1").exhibit
      exhibit match
        case Block.Code(Language.Scala, lines, _) => lines.at(Prim).let(_.tokens.at(Prim)).let(_.accent) == Token.Accent.Keyword
        case _                                    => false
    . assert(_ == true)

    // ── The terminal renderer ─────────────────────────────────────────────────────────────

    val renderer = TerminalRenderer()

    val prose: Block =
      Block.paragraph(t"The quick brown fox jumps over the lazy dog, again and again, until the line is far too long for forty columns.")

    test(m"a paragraph wraps within the width"):
      renderer.block(prose, 40).all(_.length <= 40)
    . assert(_ == true)

    test(m"a paragraph wraps onto several lines"):
      renderer.block(prose, 40).size
    . assert(_ > 2)

    val stretched: Block =
      Block.Table
        ( List
            ( Block.Column(Inline.text(t"Name"), sizing = Block.Sizing.Stretch),
              Block.Column(Inline.text(t"Time"), Block.Alignment.End, Block.Sizing.Rigid, true) ),
          List(Block.Row(List(Block.Cell(Inline.text(t"parse")), Block.Cell(List(Inline.Amount(0.5, t"s")))))) )

    test(m"a table with a stretch column spans exactly the width"):
      renderer.block(stretched, 60).filter(_.plain.starts(t"┃")).map(_.length)
    . assert(_.all(_ == 60))

    test(m"a table without a stretch column fits within the width"):
      renderer.block(rich, 60).filter(_.plain.starts(t"┃")).map(_.length)
    . assert(_.all(_ <= 60))

    // ── The table cache ───────────────────────────────────────────────────────────────────

    val bigActions: List[Action] = (0 until 40).map { index => Action(t"row-$index") }.to(List)

    def bigRow(index: Int, name: Text, note: Text, time: Double): Block.Row =
      Block.Row
        ( List(Block.Cell(Inline.text(name)), Block.Cell(Inline.text(note)), Block.Cell(List(Inline.Amount(time, t"s"))), Block.Cell(Inline.text(t"t$index"))),
          if index%7 == 0 then Tone.Success else Unset,
          bigActions.at(index.z) )

    def bigRows(count: Int): List[Block.Row] =
      (0 until count).map: index =>
        val note = if index == 11 then t"a note long enough that it must wrap onto several lines at this width" else t"note $index"
        bigRow(index, t"test number $index", note, index*0.25)
      . to(List)

    val bigColumns: List[Block.Column] =
      List
        ( Block.Column(Inline.text(t"Name"), sizing = Block.Sizing.Stretch),
          Block.Column(Inline.text(t"Note"), sizing = Block.Sizing.Paragraph),
          Block.Column(Inline.text(t"Time"), Block.Alignment.End, Block.Sizing.Rigid, true),
          Block.Column(Inline.text(t"Tag"), sizing = Block.Sizing.Collapsible(0.5)) )

    val big: Block.Table = Block.Table(bigColumns, bigRows(40), Inline.text(t"forty rows"))
    val bigLines: List[Teletype] = renderer.block(big, 60)

    // Teletypes compare by identity; their renderings compare by content.
    def shown(lines: List[Teletype]): List[Text] = lines.map(_.render(xtermTrueColorTermcap))

    def cacheOf(table: Block.Table, width: Int): TableCache =
      val cache = TableCache(renderer, table.columns, table.caption)
      cache.update(table.rows, width)
      cache

    test(m"a table cache's window is the full rendering's slice, wherever it falls"):
      val cache = cacheOf(big, 60)
      val total = bigLines.size
      List((0, 3), (2, 10), (14, 22), (total - 3, total), (0, total)).all: (from, until) =>
        shown(cache.window(from, until, Unset)) == shown(bigLines.skip(from).keep(until - from))
    . assert(_ == true)

    test(m"a table cache knows its height before rendering a row"):
      cacheOf(big, 60).height
    . assert(_ == bigLines.size)

    test(m"a repeated update renders nothing again"):
      val cache = cacheOf(big, 60)
      cache.window(0, bigLines.size, Unset)
      val before = cache.rendered
      cache.update(big.rows, 60)
      cache.window(0, bigLines.size, Unset)
      cache.rendered - before
    . assert(_ == 0)

    test(m"replacing one row renders one row, and the window still matches"):
      val cache = cacheOf(big, 60)
      cache.window(0, bigLines.size, Unset)
      val before = cache.rendered
      val rows = big.rows.indexed.map { (row, index) => if index.n0 == 5 then bigRow(5, t"test number x", t"note x", 1.25) else row }
      val table = big.copy(rows = rows)
      cache.update(rows, 60)
      val window = cache.window(0, bigLines.size, Unset)
      (cache.rendered - before, shown(window) == shown(renderer.block(table, 60)))
    . assert(_ == (1, true))

    test(m"a change of selection renders exactly the two rows concerned"):
      val cache = cacheOf(big, 60)
      cache.window(0, bigLines.size, bigActions.at(5.z))
      val before = cache.rendered
      val window = cache.window(0, bigLines.size, bigActions.at(7.z))
      (cache.rendered - before, shown(window) == shown(renderer.block(big, 60, selected = bigActions.at(7.z))))
    . assert(_ == (2, true))

    test(m"appending rows renders only the appended rows"):
      val cache = cacheOf(big, 60)
      cache.window(0, bigLines.size, Unset)
      val before = cache.rendered
      val rows = big.rows + List(bigRow(40, t"test number 40", t"note 40", 3.0), bigRow(41, t"test number 41", t"note 41", 3.25))
      val table = big.copy(rows = rows)
      cache.update(rows, 60)
      val all = renderer.block(table, 60)
      val window = cache.window(0, all.size, Unset)
      (cache.rendered - before, shown(window) == shown(all))
    . assert(_ == (2, true))

    test(m"a resized cache matches the rendering at the new width"):
      val cache = cacheOf(big, 60)
      cache.window(0, bigLines.size, Unset)
      cache.update(big.rows, 50)
      val all = renderer.block(big, 50)
      shown(cache.window(0, all.size, Unset)) == shown(all)
    . assert(_ == true)

    test(m"a row the layout cannot accommodate reflows the table, which still matches"):
      val cache = cacheOf(big, 60)
      cache.window(0, bigLines.size, Unset)
      val rows = big.rows + List(bigRow(40, t"test number 40", t"note 40", 123456.5))
      val table = big.copy(rows = rows)
      cache.update(rows, 60)
      val all = renderer.block(table, 60)
      (cache.reflows, shown(cache.window(0, all.size, Unset)) == shown(all))
    . assert(_ == (1, true))

    test(m"a gauge line is exactly the width"):
      renderer.block(Block.Gauge(Status.Fraction(0.3), Inline.text(t"work")), 50).at(Prim).let(_.length)
    . assert(_ == 50)

    test(m"code notes leave the text intact"):
      val code = Block.Code(Language.Scala, List(Block.Line(List(Token(t"val", Token.Accent.Keyword), Token.plain(t" xs = 1")))),
          List(Block.Note(0, 2, 6, Block.Note.Style.Erroneous)))
      renderer.block(code, 80).at(Prim).let(_.plain)
    . assert(_ == t"val xs = 1")

    test(m"the plain rendering carries no escape sequences"):
      renderer.plain(List(rich), 80).contains(t"")
    . assert(_ == false)

    // A run of phrasing with no space in it cannot be broken, and is emitted whole (as flame's
    // diagnostic wrapper does for an overlong word); every breakable line must fit.
    test(m"every node renders at a narrow width without overflowing"):
      renderer.blocks(List(rich), 32).map(_.plain).filter { (line: Text) => line.length > 32 && line.contains(t" ") }
    . assert(_ == Nil)

    test(m"a settled group renders its result paragraph"):
      val code = Block.Code(Language.Scala, List(Block.Line(List(Token(t"println(1)", Token.Accent.Term)))))
      val group = Block.Group(List(code, Block.Paragraph(List(Inline.Toned(Tone.Success, Inline.text(t"res1: Int = 12"))))))
      renderer.blocks(List(group), 80).map(_.plain).exists(_.contains(t"res1: Int = 12"))
    . assert(_ == true)

    test(m"settled entries precede the first animated one"):
      val code = Block.paragraph(t"x")
      val pending = Block.Group(List(code, Block.Gauge(Status.Indeterminate())))
      (Actions.settled(List(code, pending, code)), Actions.settled(List(code, code)))
    . assert(_ == (1, 2))

    // ── The arrangement solver ────────────────────────────────────────────────────────────

    def panel(name: Text, role: Panel.Role, priority: Panel.Priority): Panel =
      Panel(Panel.Id(name), role, Inline.text(name), Live(List(Block.paragraph(name))), priority)

    val arranged: Interface =
      Interface
        ( Inline.text(t"Test"),
          List
            ( panel(t"nav", Panel.Role.Navigation, Panel.Priority.Important),
              panel(t"main", Panel.Role.Primary, Panel.Priority.Essential),
              panel(t"detail", Panel.Role.Detail, Panel.Priority.Peripheral),
              panel(t"log", Panel.Role.Log, Panel.Priority.Important),
              panel(t"status", Panel.Role.Status, Panel.Priority.Essential) ) )

    test(m"a narrow terminal drops peripheral panels and keeps essential ones"):
      val plan = TerminalArrangement.plan(arranged, 60, 20)
      (plan.dropped.map(_.id.label), plan.centre.map(_.id.label), plan.rectoBeside)
    . assert(_ == (List(t"detail"), List(t"main"), false))

    test(m"a wide terminal shows every panel with detail beside the centre"):
      val plan = TerminalArrangement.plan(arranged, 120, 40)
      (plan.dropped, plan.recto.map(_.id.label), plan.rectoBeside)
    . assert(_ == (Nil, List(t"detail"), true))

    test(m"a very narrow terminal keeps only the essential panels"):
      TerminalArrangement.plan(arranged, 40, 12).dropped.map(_.id.label)
    . assert(_ == List(t"nav", t"detail", t"log"))

    // ── The web renderer ──────────────────────────────────────────────────────────────────

    val html = HtmlRenderer()

    test(m"every node renders as HTML"):
      html.block(rich).show.length
    . assert(_ > 0)

    test(m"a figure embeds its drawing verbatim, with its ids qualified"):
      val text = html.block(Block.Figure(figure)).show
      ( text.contains(t"<svg xmlns="),
        text.contains(t"""id="${figure.id}-series-0""""),
        text.contains(t"""url(#${figure.id}-grad)"""),
        text.contains(t"""href="#${figure.id}-series-0""""),
        text.contains(t"""<div id="${figure.id}-svg""""),
        text.contains(t"&lt;") )
    . assert(_ == (true, true, true, true, true, false))

    test(m"namespacing leaves text without ids alone"):
      Figure.namespace(t"f1", t"<g><rect width=\"2\"/></g>")
    . assert(_ == t"<g><rect width=\"2\"/></g>")

    test(m"the figures of a run of blocks are found wherever they nest"):
      Figure.of(List(rich)).map(_.id)
    . assert(_ == List(figure.id))

    test(m"replacing a part revises the figure and keeps the whole drawing"):
      val figure2 = Figure(Inline.text(t"a chart"), drawing)
      figure2.replace(t"series-0", t"<g id=\"series-0\"/>", t"<svg><g id=\"series-0\"/></svg>")
      (figure2.svg, figure2.revision())
    . assert(_ == (t"<svg><g id=\"series-0\"/></svg>", Figure.Revision.Replace(t"series-0", t"<g id=\"series-0\"/>")))

    test(m"a figure round-trips as TEL with its id"):
      (Block.Figure(figure): Block).in[Tel].show.read[Tel].as[Block]
    . assert(_ == Block.Figure(figure))

    test(m"an actionable row carries its action's id"):
      html.block(rich).show.contains(t"""<tr id="${run.id}" class="pyro-action pyro-tone-success">""")
    . assert(_ == true)

    test(m"a tone renders as its class"):
      html.phrase(List(Inline.Toned(Tone.Failure, Inline.text(t"no")))).show
    . assert(_ == t"""<span class="pyro-tone-failure">no</span>""")

    test(m"a keystroke renders as a kbd element"):
      html.phrase(List(Inline.Keystroke(Keypress.Ctrl('C')))).show
    . assert(_.starts(t"<kbd"))

    test(m"the web arrangement maps every role"):
      val plan = WebArrangement.plan(arranged)
      (plan.navigation.map(_.id.label), plan.primary.map(_.id.label), plan.detail.map(_.id.label), plan.status.map(_.id.label))
    . assert(_ == (List(t"nav"), List(t"main"), List(t"detail"), List(t"status")))

    val page = PyrocosmPage(arranged, html, WebTheme.default).markup.show

    test(m"the page has a content element for each panel"):
      arranged.panels.map { (panel: Panel) => page.contains(t"""id="${HtmlRenderer.panelId(panel)}-content"""") }
    . assert(_.all(_ == true))

    test(m"the page loads the generic script"):
      page.contains(t"""<script src="/pyrocosm.js" defer""")
    . assert(_ == true)

    test(m"the page is responsive and preconnects to the fonts' origins"):
      ( page.contains(t"""<meta name="viewport" content="width=device-width, initial-scale=1">"""),
        page.contains(t"""<link rel="preconnect" href="https://fonts.googleapis.com">"""),
        page.contains(t"""<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin="anonymous">""") )
    . assert(_ == (true, true, true))

    test(m"a menu bar with the wordmark and the configuration link precedes the masthead"):
      val nav = page.s.indexOf("<nav class=\"graffiti-top-menu pyro-menubar\">")
      (nav >= 0, nav < page.s.indexOf("<header"), page.contains(t"""<a href="/config" class="pyro-menubar-link">Configuration</a>"""), page.contains(t"""<a href="/" class="pyro-wordmark">Pyrocosm</a>"""))
    . assert(_ == (true, true, true, true))

    test(m"the masthead precedes the main matter, outside it"):
      val header = page.s.indexOf("<header")
      val main = page.s.indexOf("<main")
      val headerEnd = page.s.indexOf("</header>")
      (header >= 0, header < main, headerEnd < main)
    . assert(_ == (true, true, true))

    test(m"a page with navigation and detail has both side columns, and says so"):
      ( page.contains(t"""<aside class="graffiti-verso">"""), page.contains(t"""<aside class="graffiti-recto">"""),
        page.contains(t"""<body dir="ltr" class="pyro-has-verso pyro-has-recto">""") )
    . assert(_ == (true, true, true))

    test(m"an interface without global controls has no toolbar"):
      page.contains(t"pyro-toolbar")
    . assert(_ == false)

    test(m"global controls are a toolbar of commands in the masthead"):
      val append = Action(t"append")
      val controlled = Interface(Inline.text(t"Test"), arranged.panels, List(Control.Button(Inline.text(t"Append"), append)))
      val markup = PyrocosmPage(controlled, html, WebTheme.default).markup.show
      val toolbar = markup.s.indexOf("<menu class=\"pyro-toolbar\">")
      (toolbar >= 0, toolbar < markup.s.indexOf("<main"), markup.contains(t"""<li class="pyro-control"><button id="${append.id}" class="pyro-button">Append</button></li>"""))
    . assert(_ == (true, true, true))

    val prompt = Control.Field(Input(t"repl"), Control.Field.Kind.Code(Language.Scala), placeholder = t"scala>")

    val repl: Interface =
      Interface
        ( Inline.text(t"REPL"),
          List
            ( Panel(Panel.Id(t"transcript"), Panel.Role.Transcript, Unset, Live(Nil: List[Block])),
              Panel(Panel.Id(t"prompt"), Panel.Role.Prompt, Unset, Live(Nil: List[Block]), controls = List(prompt)) ) )

    test(m"a transcript is main content in both arrangements"):
      (WebArrangement.plan(repl).primary.map(_.id.label), TerminalArrangement.plan(repl, 80, 24).centre.map(_.id.label))
    . assert(_ == (List(t"transcript"), List(t"transcript")))

    test(m"a page without navigation or detail has no side columns"):
      val page = PyrocosmPage(repl, html, WebTheme.default).markup.show
      (page.contains(t"""class="graffiti-verso""""), page.contains(t"""class="graffiti-recto""""), page.contains(t"""<body dir="ltr">"""))
    . assert(_ == (false, false, true))

    test(m"a code field is an editable code element with its placeholder"):
      val page = PyrocosmPage(repl, html, WebTheme.default).markup.show
      val code: Text = page.cut(t"<code ").at(Sec).or(t"").cut(t">").at(Prim).or(t"")
      ( code.contains(t"""id="${prompt.input.id}""""),
        code.contains(t"""class="pyro-field pyro-editor""""),
        code.contains(t"""contenteditable="true""""),
        page.contains(t"""<span class="pyro-placeholder">scala&gt;</span>""") )
    . assert(_ == (true, true, true, true))

    test(m"a page with a session of its own names it"):
      PyrocosmPage(repl, html, WebTheme.default, t"s1").markup.show.contains(t"""<meta name="pyro-session" content="s1">""")
    . assert(_ == true)

    test(m"a decoration carries its tokens, note, marker, marks and completions"):
      prompt.decoration() = Control.Field.Decoration
        ( List(Token(t"pri", Token.Accent.Term)),
          List(Control.Field.Completion(t"println", t"term", t"…"), Control.Field.Completion(t"/session main", t"command", t"", whole = true)),
          incomplete = true,
          detail = List(Block.paragraph(t"reads as code")),
          marks = List(Block.Note(0, 0, 3, Block.Note.Style.Erroneous)) )

      val decoration = PyrocosmPage(repl, html, WebTheme.default).decoration(prompt).show
      List(t"""<span class="pyro-tokens" hidden="">""", t"pyro-note", t"pyro-completions", t"println", t"reads as code", t"""<li class="pyro-replacement">""", t"pyro-note-erroneous").all(decoration.contains(_))
    . assert(_ == true)

    test(m"a value a tab published is not sent back to it"):
      val published = Published()
      published.publish(t"repl", t"val x")
      published.fresh(t"repl", t"val x")

    . assert(_ == false)

    test(m"a value the application writes is sent once, and is then what the tabs show"):
      val published = Published()
      published.publish(t"repl", t"val x")
      (published.fresh(t"repl", t""), published.fresh(t"repl", t""))

    . assert(_ == (true, false))

    test(m"a field no tab has published sends its first value"):
      Published().fresh(t"repl", t"1 + 1")

    . assert(_ == true)

    test(m"a page seeds the field's history"):
      prompt.history() = List(t"val x = 1")
      PyrocosmPage(repl, html, WebTheme.default).markup.show.contains(t"""data-history="[&quot;val x = 1&quot;]"""")
    . assert(_ == true)

    test(m"a field whose text is an incomplete prefix carries the state on itself"):
      PyrocosmPage(repl, html, WebTheme.default).markup.show.contains(t"""class="pyro-field pyro-editor pyro-incomplete"""")
    . assert(_ == true)

    test(m"the page links the stylesheet rather than embedding it"):
      val page = PyrocosmPage(repl, html, WebTheme.default).markup.show
      (page.contains(t"""rel="stylesheet""""), page.contains(t"""href="/pyrocosm.css""""), page.contains(t"<style"))
    . assert(_ == (true, true, false))

    test(m"nothing carries an inline style"):
      (page.contains(t"style="), html.block(rich).show.contains(t"style="))
    . assert(_ == (false, false))

    test(m"a bar chart is a figure of meters"):
      val chart = html.block(Block.Chart(Block.Chart.Kind.Bars, List(Block.Series(Inline.text(t"s"), List(1.0, 2.0))))).show
      ( chart.starts(t"""<figure class="pyro-chart pyro-chart-bars"><dl class="pyro-series">"""),
        chart.contains(t"""<dt class="pyro-series-label">s</dt>"""),
        chart.contains(t"""<meter class="pyro-meter" value="1.0" max="2.0">1</meter>""") )
    . assert(_ == (true, true, true))

    test(m"a gauge is a figure with its caption"):
      html.block(Block.Gauge(Status.Fraction(0.3), Inline.text(t"work"))).show
    . assert(_.starts(t"""<figure class="pyro-gauge"><figcaption>work</figcaption><progress"""))

    test(m"a duration is a time element with its machine-readable form"):
      html.block(Block.Gauge(Status.Elapsed(90.0))).show
    . assert(_.contains(t"""<time class="pyro-elapsed" datetime="PT90.000S">"""))

    test(m"a column's default alignment is unmarked"):
      html.block(emptyCells).show.contains(t"pyro-align")
    . assert(_ == false)

    test(m"captured output renders behind a gutter in both media"):
      val output = Block.Output(t"one\ntwo\n", error = true)
      (renderer.plain(List(output), 80), html.block(output).show)
    . assert { (plain, page) => plain == t"░ one\n░ two" && page.contains(t"pyro-output-stderr") && page.contains(t"one\n") }

    val captured: Block = Block.Output(t"one\ntwo\n", error = true)

    test(m"captured output round-trips as TEL"):
      captured.in[Tel].show.read[Tel].as[Block]
    . assert(_ == captured)

    // ── Stack traces ──────────────────────────────────────────────────────────────────────

    test(m"a stack trace exhibits as a trace with its cause chain"):
      val exhibit: Block = StackTrace(Exception("outer", Exception("inner"))).exhibit
      exhibit match
        case Block.Trace(stacks) => stacks.map(_.className) == List(t"Exception", t"Exception") && stacks.all(_.frames.size > 0)
        case _                   => false
    . assert(_ == true)

    test(m"an exception exhibits through its stack trace"):
      val exhibit: Block = (Exception("oops"): Throwable).exhibit
      exhibit match
        case Block.Trace(stack :: Nil) => stack.message == Inline.text(t"oops") && stack.component == t"java.lang"
        case _                         => false
    . assert(_ == true)

    val traceLines: List[Text] = renderer.block(trace, 120).map(_.plain)

    test(m"a trace renders a headline, a line per frame and per inlining, and its cause"):
      traceLines.size
    . assert(_ == 8)

    test(m"a trace's headline names the exception and its message"):
      traceLines.at(Prim).or(t"")
    . assert(_ == t"java.lang.IllegalStateException: bad state")

    test(m"an inlining row follows its frame, and a cause follows the frames"):
      (traceLines.at(Sec).or(t"").contains(t"run"), traceLines.at(Ter).or(t"").trim.starts(t"↳"), traceLines.at(Sen).or(t""))
    . assert(_ == (true, true, t"caused by:"))

    test(m"a location is contiguous text aligned on its colon"):
      val rows: List[Text] = traceLines.skip(1).keep(4)
      val colons: List[Int] = rows.map(_.s.indexOf(":"))
      val contiguous = rows.all { (row: Text) => row.s.matches(".*\\S:\\d*(\\s.*)?") }
      (colons.distinct.size, contiguous)
    . assert(_ == (1, true))

    val traceHtml: Text = html.block(trace).show

    // The first stack's two `pyrocosm` frames share the first accent and its `scala.runtime`
    // frame takes the second; the cause's own first package starts again from the first.
    test(m"a trace's packages take accents in order of first appearance"):
      val first = traceHtml.s.indexOf("pyro-trace-accent-1")
      val second = traceHtml.s.indexOf("pyro-trace-accent-2")
      (first < second, traceHtml.s.split("pyro-trace-accent-1").nn.length, traceHtml.s.split("pyro-trace-accent-2").nn.length)
    . assert(_ == (true, 4, 2))

    test(m"a plumbing frame, a repeated name, an inlining and a cause are marked"):
      List(t"pyro-plumbing", t"pyro-repeat", t"pyro-inlined", t"pyro-caused-by").all(traceHtml.contains(_))
    . assert(_ == true)

    test(m"a location's file and colon cells are adjacent"):
      traceHtml.contains(t"Tests.scala</td><td class=\"pyro-line\"><span class=\"pyro-colon\">:</span>42</td>")
    . assert(_ == true)

    test(m"a trace round-trips as TEL"):
      trace.in[Tel].show.read[Tel].as[Block]
    . assert(_ == trace)

    test(m"a sum without a Showable exhibits as its toString, not a record"):
      val exhibit: Inline | Block = (Node.Leaf(t"a"): Node).exhibit
      exhibit == Inline.Textual(t"Leaf(a)")
    . assert(_ == true)

    val stylesheet: Text = WebStyles.css(WebTheme.default).show

    test(m"the stylesheet declares the palette as custom properties"):
      stylesheet.contains(t"--pyro-bg: #f5f5f7") && stylesheet.contains(t"--pyro-tone-accent: #d9480f") && stylesheet.contains(t"--pyro-on-accent: #ffffff") && stylesheet.contains(t"--pyro-code-bg: #16131f") && stylesheet.contains(t"--pyro-code-accent-keyword: #ff6633") && stylesheet.contains(t"--pyro-button: #f5c518") && stylesheet.contains(t"--pyro-title: #120b08") && stylesheet.contains(t"--pyro-menubar: #16131f") && stylesheet.contains(t"--pyro-on-menubar: #ffffff")
    . assert(_ == true)

    test(m"another theme is another root block"):
      val solarized = WebStyles.css(WebTheme.SolarizedDark).show
      solarized.contains(t"--pyro-bg: #002b36") && solarized.contains(t"--pyro-tone-success: #859900") && solarized.contains(t"--pyro-on-accent: #002b36") && solarized.contains(t"--pyro-code-bg: #073642")
    . assert(_ == true)

    test(m"the stylesheet begins by importing the fonts, and names all three"):
      ( stylesheet.starts(t"@import url(\"https://fonts.googleapis.com/css2?family=Yantramanav"),
        stylesheet.contains(t"\"Marcellus\""), stylesheet.contains(t"\"Yantramanav\""), stylesheet.contains(t"\"Sono\"") )
    . assert(_ == (true, true, true, true))

    test(m"the page's whole sheet, graffiti's rules included, still begins with the import"):
      PyrocosmPage(repl, html, WebTheme.default).css.show.starts(t"@import url(")
    . assert(_ == true)

    test(m"the stylesheet adapts to a narrow viewport"):
      stylesheet.contains(t"@media (max-width: 64rem)") && stylesheet.contains(t"@media (max-width: 40rem)")
    . assert(_ == true)

    test(m"the stylesheet declares a trace's colours and folds its source column on a narrow viewport"):
      val narrow = stylesheet.s.indexOf("@media (max-width: 64rem)")
      val folded = narrow >= 0 && stylesheet.s.indexOf(".pyro-frames .pyro-source", narrow) > narrow
      stylesheet.contains(t"--pyro-trace-accent-5") && stylesheet.contains(t"--pyro-trace-file") && folded
    . assert(_ == true)

    test(m"the stylesheet draws a meter in every engine"):
      stylesheet.contains(t".pyro-meter::-webkit-meter-optimum-value") && stylesheet.contains(t".pyro-meter::-moz-meter-bar")
    . assert(_ == true)

    test(m"the stylesheet refers to colours only through variables"):
      stylesheet.contains(t"$$") || stylesheet.contains(t"Chroma")
    . assert(_ == false)

    test(m"the stylesheet reads back as CSS"):
      try stylesheet.read[Css].rules.size
      catch case errors: Css.Errors =>
        java.lang.System.err.nn.println(s"CSS ERRORS: ${errors.errors.map { e => s"${e.reason} at ${e.line}:${e.column}" }}")
        0
    . assert(_ > 50)

    // ── On a terminal emulator ────────────────────────────────────────────────────────────

    test(m"rendered lines draw on a terminal emulator as their plain text"):
      val lines = renderer.blocks(List(rich), 80)
      val rendered = lines.map(_.render(xtermTrueColorTermcap)).join(t"\r\n")
      val pty = Pty(80, 40).consume(rendered)
      val first = lines.at(Prim).let(_.plain).or(t"")

      (0 until first.length).forall { column => pty.buffer.char(column.z, Prim) == first.s.charAt(column) }
    . assert(_ == true)

    // ── The code field ────────────────────────────────────────────────────────────────────

    // A board in memory: the plain characters put on it, by cell, and where the caret was left.
    class MemoryBoard(val width: Int, val height: Int) extends profanity.Board:
      val cells: java.util.HashMap[Int, Char] = java.util.HashMap()
      var column: Int = 0
      var row: Int = 0
      var caret: (Int, Int) = (0, 0)

      def move(column: Ordinal, row: Ordinal): Unit =
        this.column = column.n0
        this.row = row.n0

      def put(text: Text): Unit = text.s.foreach: char =>
        if char == '\n' then
          column = 0
          row += 1
        else
          if row < height && column < width then cells.put(row*width + column, char)
          column += 1

      def put(text: Teletype): Unit = put(text.plain)

      def clear(): Unit =
        cells.clear()
        column = 0
        row = 0

      def clearLine(): Unit = ()
      def cursor(visible: Boolean): Unit = ()
      def showCaret(column: Ordinal, row: Ordinal): Unit = caret = (column.n0, row.n0)
      def flush(): Unit = ()

      def line(row: Int): Text =
        val builder = java.lang.StringBuilder()

        (0 until width).foreach: column =>
          builder.append(cells.getOrDefault(row*width + column, ' '))

        builder.toString.tt

    def codeField(value: Text, prompt: List[Inline]): CodeField =
      val field: Control.Field =
        Control.Field
          ( Input(t"code"), Control.Field.Kind.Line, value = Live(value),
            decoration = Live(Control.Field.Decoration(prompt = prompt)) )

      CodeField(field, renderer, (_: Event) => (), pyrocosm.Session())

    val arrow: List[Inline] = List(Inline.Toned(Tone.Accent, Inline.text(t"\ue0b0 ")))
    val chevron: List[Inline] = Inline.text(t"> ")

    test(m"a prompt is drawn before the text, which starts past it, as does the caret"):
      val fixture = codeField(t"hello", arrow)
      val grid = MemoryBoard(20, 4)
      fixture.measure(20)
      fixture.render(grid, true)
      (grid.line(0).keep(7), grid.caret)

    . assert(_ == (t"\ue0b0 hello", (7, 0)))

    test(m"an empty prompt leaves the text and caret at the first column"):
      val fixture = codeField(t"hello", Nil)
      val grid = MemoryBoard(20, 4)
      val rows = fixture.measure(20)(1)
      fixture.render(grid, true)
      (grid.line(0).keep(5), grid.caret, rows)

    . assert(_ == (t"hello", (5, 0), 1))

    test(m"the text wraps at the width less the prompt and hangs beneath it"):
      val fixture = codeField(t"abcdefg", chevron)
      val grid = MemoryBoard(6, 4)
      val rows = fixture.measure(6)(1)
      fixture.render(grid, true)
      (grid.line(0), grid.line(1), grid.caret, rows)

    . assert(_ == (t"> abcd", t"  efg ", (5, 1), 2))

    test(m"a later line of a multi-line value hangs beneath the prompt"):
      val fixture = codeField(t"ab\ncd", chevron)
      val grid = MemoryBoard(10, 4)
      fixture.measure(10)
      fixture.render(grid, true)
      (grid.line(0), grid.line(1), grid.caret)

    . assert(_ == (t"> ab      ", t"  cd      ", (4, 1)))

    test(m"a caret moved into the text keeps the prompt's offset"):
      val fixture = codeField(t"hello", chevron)
      fixture.handle(Keypress.Left)
      fixture.handle(Keypress.Left)
      val grid = MemoryBoard(20, 4)
      fixture.measure(20)
      fixture.render(grid, true)
      grid.caret

    . assert(_ == (5, 0))

    test(m"a field too narrow for its prompt drops it"):
      val fixture = codeField(t"ab", chevron)
      val grid = MemoryBoard(2, 4)
      val rows = fixture.measure(2)(1)
      fixture.render(grid, true)
      (grid.line(0), rows, grid.caret)

    . assert(_ == (t"ab", 1, (0, 1)))

    // ── Git notes ─────────────────────────────────────────────────────────────────────────

    suite(m"Notes"):
      // A fresh repository under a temporary directory, driven by the same `git` on the path
      // that `Notes` uses. The identity is set in the repository's own config, since `Notes`
      // itself commits (to the notes refs) and CI has no global identity.
      val repo: Text = java.nio.file.Files.createTempDirectory("pyrocosm-notes").nn.toString.tt
      given WorkingDirectory = () => repo

      def git(arguments: Text*): Text =
        val fixed = scala.collection.immutable.List(t"git", t"-C", repo)
        Command((fixed ++ arguments)*).exec[Text]().trim

      def put(path: Text, content: Text): Unit =
        val file = java.nio.file.Path.of(repo.s, path.s).nn
        java.nio.file.Files.createDirectories(file.getParent.nn)
        java.nio.file.Files.writeString(file, content.s)

      git(t"init", t"-q")
      git(t"config", t"user.name", t"Tests")
      git(t"config", t"user.email", t"tests@example.com")
      put(t"src/a.scala", t"object A\n")
      put(t"src/deep/b.scala", t"object B\n")
      put(t"doc/readme.md", t"# Docs\n")
      put(t"README.md", t"# Top\n")
      put(t"out/junk", t"junk\n")
      git(t"add", t"-A")
      git(t"commit", t"-q", t"-m", t"first")
      val first: Text = git(t"rev-parse", t"HEAD")
      val firstTree: Text = git(t"rev-parse", t"HEAD^{tree}")

      put(t"doc/readme.md", t"# Docs, revised\n")
      git(t"add", t"-A")
      git(t"commit", t"-q", t"-m", t"second")
      val second: Text = git(t"rev-parse", t"HEAD")

      put(t"src/a.scala", t"object A2\n")
      git(t"add", t"-A")
      git(t"commit", t"-q", t"-m", t"third")
      val third: Text = git(t"rev-parse", t"HEAD")

      // The tree `first` has once the excluded paths are gone, made by git itself.
      git(t"checkout", t"-q", first)
      git(t"rm", t"-q", t"-r", t"doc", t"README.md", t"out")
      git(t"commit", t"-q", t"-m", t"stripped")
      val strippedTree: Text = git(t"rev-parse", t"HEAD^{tree}")
      git(t"checkout", t"-q", t"-B", t"main", third)

      given notes: Notes = Notes(repo)
      val filter = Filter(List(t"doc/", t"**/*.md", t"out"))
      val bench = Bench(t"sort", 1.5, t"first line\nsecond line with ünïcode ✓")

      test(m"a refspec resolves to its commit"):
        notes.commit(t"HEAD").text
      . assert(_ == third)

      test(m"a refspec that names nothing is an error"):
        capture[Notes.Error](notes.commit(t"nonesuch")).reason
      . assert(_ == Notes.Error.Reason.BadRef(t"nonesuch"))

      test(m"an empty filter fingerprints the commit's own tree"):
        notes.fingerprint(Commit.unsafe(first), Filter.none).text
      . assert(_ == firstTree)

      test(m"a filter removes the excluded paths from the tree"):
        notes.fingerprint(Commit.unsafe(first), filter).text
      . assert(_ == strippedTree)

      test(m"commits differing only in excluded files share a fingerprint"):
        notes.fingerprint(Commit.unsafe(second), filter).text
      . assert(_ == strippedTree)

      test(m"commits differing in included files do not share a fingerprint"):
        notes.fingerprint(Commit.unsafe(third), filter).text
      . assert(_ != strippedTree)

      test(m"a bare name matches at any depth, and everything beneath it"):
        val out = Filter(List(t"out"))
        (out.excludes(t"out"), out.excludes(t"out/x"), out.excludes(t"a/out/y"), out.excludes(t"output/z"))
      . assert(_ == (true, true, true, false))

      test(m"a trailing slash matches a directory and its contents, not a prefix"):
        val doc = Filter(List(t"doc/"))
        (doc.excludes(t"doc/r.md"), doc.excludes(t"doc/a/b"), doc.excludes(t"docs/x"))
      . assert(_ == (true, true, false))

      test(m"** spans segments and * stays within one"):
        val globs = Filter(List(t"**/*.md", t"src/*.scala"))
        (globs.excludes(t"README.md"), globs.excludes(t"a/b/c.md"), globs.excludes(t"a.mdx"), globs.excludes(t"src/a.scala"), globs.excludes(t"src/x/b.scala"))
      . assert(_ == (true, true, false, true, false))

      test(m"an include overrides an exclude"):
        val kept = Filter(List(t"doc/"), List(t"doc/keep.md"))
        (kept.excludes(t"doc/keep.md"), kept.excludes(t"doc/other.md"))
      . assert(_ == (false, true))

      test(m"a filter round-trips as TEL text"):
        filter.in[Tel].show.read[Tel].as[Filter]
      . assert(_ == filter)

      val fingerprint: Fingerprint = notes.fingerprint(Commit.unsafe(first), filter)

      test(m"a record is absent before it is written"):
        notes.read[Bench](fingerprint)
      . assert(_ == Unset)

      test(m"a record round-trips through a note, newlines and non-ASCII intact"):
        notes.write[Bench](fingerprint, bench)
        notes.read[Bench](fingerprint)
      . assert(_ == bench)

      test(m"binding twice leaves one index line"):
        notes.bind(Commit.unsafe(first), fingerprint)
        notes.bind(Commit.unsafe(first), fingerprint)
        notes.fingerprints(Commit.unsafe(first))
      . assert(_ == List(fingerprint))

      test(m"a commit's record is found through its index"):
        Commit.unsafe(first).record[Bench]
      . assert(_ == bench)

      test(m"a commit with no index has no record"):
        Commit.unsafe(third).record[Bench]
      . assert(_ == Unset)

      test(m"recording through a filter writes, binds and returns the fingerprint"):
        Commit.unsafe(second).record[Bench](filter, bench)
      . assert(_ == fingerprint)

      test(m"a second commit bound to the same fingerprint reads the same record"):
        Commit.unsafe(second).record[Bench]
      . assert(_ == bench)

      test(m"the reverse index lists every commit bound to a fingerprint"):
        notes.commits(fingerprint).map(_.text)
      . assert { commits => commits.has(first) && commits.has(second) && !commits.has(third) }

      test(m"the newest fingerprint of a commit is tried first"):
        val other = Fingerprint.unsafe(t"0123456789abcdef0123456789abcdef01234567")
        val newer = Bench(t"sort", 2.5, t"newer")
        Commit.unsafe(first).record[Bench](other, newer)
        (notes.fingerprints(Commit.unsafe(first)), Commit.unsafe(first).record[Bench], Commit.unsafe(first).recordings[Bench])
      . assert(_ == (List(Fingerprint.unsafe(t"0123456789abcdef0123456789abcdef01234567"), fingerprint), Bench(t"sort", 2.5, t"newer"), List(Bench(t"sort", 2.5, t"newer"), bench)))

      test(m"a note on a hash with no local object can be written and read"):
        val absent = Fingerprint.unsafe(t"fedcba9876543210fedcba9876543210fedcba98")
        notes.write[Bench](absent, bench)
        notes.read[Bench](absent)
      . assert(_ == bench)

      test(m"history walks back from a refspec, newest first"):
        notes.history(t"main", 2).map(_.text)
      . assert(_ == List(third, second))

      test(m"published notes are fetched and read by a clone"):
        val remote: Text = java.nio.file.Files.createTempDirectory("pyrocosm-remote").nn.toString.tt
        val clone: Text = java.nio.file.Files.createTempDirectory("pyrocosm-clone").nn.toString.tt
        java.nio.file.Files.delete(java.nio.file.Path.of(clone.s))
        Command(t"git", t"init", t"-q", t"--bare", remote).exec[Text]()
        git(t"remote", t"add", t"origin", remote)
        git(t"push", t"-q", t"origin", t"main")
        notes.publish()
        Command(t"git", t"clone", t"-q", remote, clone).exec[Text]()
        val cloned = Notes(clone)
        cloned.fetch()

        cloned.fingerprints(cloned.commit(second)) match
          case fingerprint :: _ => cloned.read[Bench](fingerprint)
          case _                => Unset
      . assert(_ == bench)

    // ── The standard command line ─────────────────────────────────────────────────────────

    suite(m"Tool"):
      // A project directory with a `.pyrocosm/demo/config.tel` at its root, a nested directory
      // to invoke from, and a home for the user's `~/.config/demo/config.tel`, reached through
      // `XDG_CONFIG_HOME` in an environment built for the test.
      val project: Text = java.nio.file.Files.createTempDirectory("pyrocosm-tool").nn.toString.tt
      val home: Text = java.nio.file.Files.createTempDirectory("pyrocosm-config").nn.toString.tt

      def put(root: Text, path: Text, content: Text): Unit =
        val file = java.nio.file.Path.of(root.s, path.s).nn
        java.nio.file.Files.createDirectories(file.getParent.nn)
        java.nio.file.Files.writeString(file, content.s)

      given Environment = name => if name == t"XDG_CONFIG_HOME" then home else Unset

      val demo: Tool = Tool(t"demo", prose = t"A tool.")
      val nested: Text = t"$project/sub/dir"
      java.nio.file.Files.createDirectories(java.nio.file.Path.of(nested.s))

      test(m"no configuration file is found in an unconfigured project"):
        demo.repoFile(nested)
      . assert(_ == Unset)

      test(m"another tool's configuration does not end the search"):
        put(project, t".pyrocosm/other/config.tel", t"tel 1.0\n")
        demo.repoFile(nested)
      . assert(_ == Unset)

      test(m"the repository's configuration is found from a nested directory"):
        put(project, t".pyrocosm/demo/config.tel", t"tel 1.0\nport 1\nquiet\npath a\npath b\n")
        demo.repoFile(nested).let(_.encode)
      . assert(_ == t"$project/.pyrocosm/demo/config.tel")

      test(m"the user's configuration file is under XDG_CONFIG_HOME"):
        demo.userFile.let(_.encode)
      . assert(_ == t"$home/demo/config.tel")

      test(m"a setting reads from the repository's configuration"):
        demo.configurator(nested).read(t"port")
      . assert(_ == t"1")

      test(m"a bare keyword reads as true"):
        demo.configurator(nested).read(t"quiet")
      . assert(_ == t"true")

      test(m"a repeated keyword joins its atoms"):
        demo.configurator(nested).read(t"path")
      . assert(_ == t"a:b")

      test(m"an absent keyword is unset"):
        demo.configurator(nested).read(t"missing")
      . assert(_ == Unset)

      test(m"a camelCase setting name reads its kebab-case keyword"):
        put(project, t".pyrocosm/demo/config.tel", t"tel 1.0\nport 1\nfail-fast\n")
        demo.configurator(nested).read(t"failFast")
      . assert(_ == t"true")

      test(m"the user's configuration supplies what the repository's does not"):
        put(home, t"demo/config.tel", t"tel 1.0\nport 2\nserve\n")
        demo.configurator(nested).read(t"serve")
      . assert(_ == t"true")

      test(m"the repository's configuration takes priority over the user's"):
        demo.configurator(nested).read(t"port")
      . assert(_ == t"1")

      test(m"the user's configuration alone is read outside any project"):
        demo.configurator(home).read(t"port")
      . assert(_ == t"2")

      test(m"an edit to a configuration file is honoured by the next read"):
        put(project, t".pyrocosm/demo/config.tel", t"tel 1.0\nport 3000\n")
        demo.configurator(nested).read(t"port")
      . assert(_ == t"3000")

      test(m"a build that wrote no version resource has an unknown version"):
        demo.version
      . assert(_ == t"unknown")

    // ── Remote machines ───────────────────────────────────────────────────────────────────

    suite(m"Remote"):
      import probates.cancelProbate
      import alphabets.hexLowerCase

      def ascii(text: Text): Data = Array.unsafeFrozen(text.s.getBytes("US-ASCII").nn)
      def bytes(data: Data): List[Byte] = data.to[List]

      // Codecs made as a tool makes its own: at the concrete type, with the throwing tactics.
      def pingCodec(name: Text): Channel.Codec[Ping] =
        import Channel.derivation.throwing
        val schema: Tels = Tels.tels[Ping](name)
        Channel.Codec(name, schema, message => Channel.encode(message, schema), data => Channel.decode[Ping](data))

      def pongCodec(name: Text): Channel.Codec[Pong] =
        import Channel.derivation.throwing
        val schema: Tels = Tels.tels[Pong](name)
        Channel.Codec(name, schema, message => Channel.encode(message, schema), data => Channel.decode[Pong](data))

      val codec: Channel.Codec[Ping] = pingCodec(t"ping")

      suite(m"Channel"):
        test(m"a codec's fingerprint is 32 bytes and stable"):
          (codec.fingerprint.length, codec.protocol == pingCodec(t"ping").protocol)
        . assert(_ == (32, true))

        test(m"a different message layout has a different fingerprint"):
          pongCodec(t"ping").protocol == codec.protocol
        . assert(_ == false)

        test(m"messages, raw bytes and handshake frames round-trip in order"):
          val (left, right) = Duplex.pair()
          val sender = Channel(codec, left)
          val receiver = Channel(codec, right)
          sender.send(Ping.Hello(t"hi", 3))
          sender.sendRaw(ascii(t"payload"))
          sender.sendHandshake(Data.fill(32) { i => i.toByte }, ascii(t"doc"))
          sender.send(Ping.Count(7))

          val first = receiver.receive()
          val second = receiver.receive()
          val third = receiver.receive()
          val fourth = receiver.receive()

          val raw: List[Byte] = second match
            case Channel.Frame.Raw(data) => bytes(data)
            case _                       => Nil

          val handshake: (Int, List[Byte]) = third match
            case Channel.Frame.Handshake(fingerprint, document) => (fingerprint.length, bytes(document))
            case _                                             => (0, Nil)

          (first, raw, handshake, fourth)
        . assert(_ == (Channel.Frame.Message(Ping.Hello(t"hi", 3)), bytes(ascii(t"payload")),
                       (32, bytes(ascii(t"doc"))), Channel.Frame.Message(Ping.Count(7))))

        test(m"a large raw frame crosses chunk boundaries intact"):
          val (left, right) = Duplex.pair()
          val sender = Channel(codec, left)
          val receiver = Channel(codec, right)
          val big: Data = Data.fill(300000) { i => (i%251).toByte }
          sender.sendRaw(big)
          receiver.receive() match
            case Channel.Frame.Raw(data) => data.length == big.length && Channel.same(data, big)
            case _                       => false
        . assert(_ == true)

        test(m"a closed duplex reads as Closed"):
          val (left, right) = Duplex.pair()
          left.close()
          Channel(codec, right).receive()
        . assert(_ == Channel.Frame.Closed)

      suite(m"Blobs"):
        val root: Text = java.nio.file.Files.createTempDirectory("pyrocosm-blobs").nn.toString.tt

        def put(path: Text, content: Text): Unit =
          val file = java.nio.file.Path.of(root.s, path.s).nn
          java.nio.file.Files.createDirectories(file.getParent.nn)
          java.nio.file.Files.writeString(file, content.s)

        put(t"classes/a/One.class", t"one")
        put(t"classes/b/Two.class", t"two")
        put(t"copy/b/Two.class", t"two")
        put(t"copy/a/One.class", t"one")
        put(t"lib.jar", t"not really a jar")

        def path(relative: Text): Path on Linux = unsafely(t"$root/$relative".as[Path on Linux])

        test(m"digests are 64 hex characters"):
          Blobs.digest(ascii(t"abc"))
        . assert(_ == t"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")

        test(m"a directory bundles deterministically whatever the order it was written"):
          val (first, _) = unsafely(Blobs.prepare(path(t"classes"), t"classes"))
          val (second, _) = unsafely(Blobs.prepare(path(t"copy"), t"copy"))
          (first.digest == second.digest, first.directory)
        . assert(_ == (true, true))

        test(m"a bundle contains every file under its relative path"):
          val (_, data) = unsafely(Blobs.prepare(path(t"classes"), t"classes"))
          val zipfile = unsafely(Zipfile.read(data))
          zipfile.entries.map(_.ref.encode.s.stripPrefix("/")).stdlib.sorted.to(List)
        . assert(_ == List("a/One.class", "b/Two.class"))

        test(m"a jar is digested as it is"):
          val (entry, data) = unsafely(Blobs.prepare(path(t"lib.jar"), t"lib.jar"))
          (entry.directory, entry.digest == Blobs.digest(ascii(t"not really a jar")), data.length)
        . assert(_ == (false, true, 16))

        test(m"chunks partition the data and an empty blob is one chunk"):
          val data: Data = Data.fill(Blobs.chunk*2 + 5) { i => i.toByte }
          val chunks = Blobs.chunks(data)
          (chunks.map(_(0)), chunks.map(_(1).length), Blobs.chunks(Data()).size)
        . assert(_ == (List(0, Blobs.chunk, Blobs.chunk*2), List(Blobs.chunk, Blobs.chunk, 5), 1))

        test(m"a store keeps what it is given under its digest, and rejects a lie"):
          java.nio.file.Files.createDirectories(java.nio.file.Path.of(root.s, "store"))
          val store = Blobs.Store(path(t"store"))
          val data = ascii(t"blob")
          val digest = Blobs.digest(data)
          val stored = store.put(digest, data)
          val lied = store.put(t"0"*64, data)
          (stored, lied, store.has(digest), store.missing(List(digest, t"0"*64)))
        . assert(_ == (true, false, true, List(t"0"*64)))

        test(m"an assembly stores a blob once its last chunk arrives"):
          val store = Blobs.Store(path(t"store"))
          val data: Data = Data.fill(3000) { i => (i*7).toByte }
          val digest = Blobs.digest(data)
          val assembly = Blobs.Assembly(store)
          val first = assembly.receive(digest, Channel.slice(data, 0, 1000), false)
          val second = assembly.receive(digest, Channel.slice(data, 1000, 3000), true)
          (first, second, store.has(digest))
        . assert(_ == (Unset, true, true))

      suite(m"Machine"):
        val document: Tel = unsafely(t"""tel 1.0

machine linux-box
  host build.example.org
  port 8091
  identity sha256:${t"ab"*32}
  token not-a-file
  capability linux x86-64
  capability quiet

machine nameless
  port 1

machine mac-mini
  host 10.0.0.7
""".read[Tel])

        val machines: List[Machine] = Machine.parse(document)

        test(m"machine blocks parse, and one without a host is skipped"):
          machines.map(_.name)
        . assert(_ == List(t"linux-box", t"mac-mini"))

        test(m"a machine's fields are read"):
          machines.prim.let: machine =>
            (machine.host, machine.port, machine.identity.let(Peer.render(_)), machine.token, machine.capabilities)
        . assert(_ == (t"build.example.org", 8091, t"sha256:${t"ab"*32}", t"not-a-file", List(t"linux", t"x86-64", t"quiet")))

        test(m"a missing port takes the tool's default"):
          machines.map(_.portOr(9000))
        . assert(_ == List(8091, 9000))

        test(m"an earlier document overrides a later one by name"):
          val override0: Tel = unsafely(t"tel 1.0\n\nmachine mac-mini\n  host 10.0.0.8\n".read[Tel])
          Machine.resolve(List(override0, document)).map(machine => (machine.name, machine.host))
        . assert(_ == List((t"mac-mini", t"10.0.0.8"), (t"linux-box", t"build.example.org")))

        test(m"a token that names no file is the secret itself"):
          Machine.secret(t"not-a-file")
        . assert(_ == t"not-a-file")

        test(m"a token that names a file is the file's trimmed content"):
          val file = java.nio.file.Files.createTempFile("pyrocosm-token", "").nn
          java.nio.file.Files.writeString(file, "  s3cret\n")
          Machine.secret(file.toString.tt)
        . assert(_ == t"s3cret")

        test(m"fingerprints parse in every rendering"):
          val expected: Data = Data.fill(32) { i => (i + 1).toByte }
          val hex: Text = expected.serialize[Hex]
          val colons: Text = hex.s.toUpperCase.nn.grouped(2).mkString(":").tt
          List(t"sha256:$hex", hex, colons, t"SHA256:$colons", t"sha256:abc").map(Peer.parseFingerprint(_) == Unset)
        . assert(_ == List(false, false, false, false, true))

      suite(m"Peer"):
        import supervisors.globalSupervisor

        val state: Text = java.nio.file.Files.createTempDirectory("pyrocosm-state").nn.toString.tt
        val config: Text = java.nio.file.Files.createTempDirectory("pyrocosm-config").nn.toString.tt

        given Environment = name =>
          if name == t"XDG_STATE_HOME" then state
          else if name == t"XDG_CONFIG_HOME" then config
          else Unset

        val identity: Peer.Identity = unsafely(Peer.identity)
        val token: Text = Peer.token.or(t"")

        test(m"an identity is generated once and read back thereafter"):
          val again = unsafely(Peer.identity)
          (identity.fingerprint.length, Channel.same(again.fingerprint, identity.fingerprint), token.s.length)
        . assert(_ == (32, true, 64))

        // A worker that echoes each message's text back, reversed, until the client closes.
        def echo(session: Peer.Session[Ping]): Unit =
          def recur(): Unit = session.receive() match
            case Channel.Frame.Message(Ping.Hello(text, count)) =>
              session.send(Ping.Hello(text.s.reverse.tt, count + 1))
              recur()

            case Channel.Frame.Raw(data) =>
              session.sendRaw(data)
              recur()

            case _ =>
              ()

          recur()

        def worker(gate: () => Optional[Text]): Peer.Listener[Ping] =
          Peer.Listener(t"demo", t"1.0", codec, token, identity, List(t"quiet"), gate)(echo)

        def machine(fingerprint: Data, secret: Text, port: Int): Machine =
          Machine(t"worker", t"127.0.0.1", port, fingerprint, secret, Nil)

        def serving[result](listener: Peer.Listener[Ping])(block: Int => result): result =
          import threading.platformThreading

          supervise:
            val port: Int = Port[Tcp]().number
            val task = async(listener.serve(port))
            Thread.sleep(300)

            try block(port) finally
              listener.stop()
              safely(task.await())

        test(m"a pinned client with the right token is welcomed and exchanges messages"):
          serving(worker(() => Unset)): port =>
            unsafely:
              Peer.connect(machine(identity.fingerprint, token, port), t"demo", t"1.0", codec, 1): session =>
                session.send(Ping.Hello(t"abc", 1))
                val reply = session.receive()
                session.sendRaw(ascii(t"raw"))
                val echoed = session.receive() match
                  case Channel.Frame.Raw(data) => bytes(data)
                  case _                       => Nil

                (reply, echoed, session.peer.tool, session.peer.capabilities, session.peer.identity.cores > 0)
        . assert(_ == (Channel.Frame.Message(Ping.Hello(t"cba", 2)), bytes(ascii(t"raw")), t"demo", List(t"quiet"), true))

        test(m"a wrong token is refused"):
          serving(worker(() => Unset)): port =>
            try
              unsafely(Peer.connect(machine(identity.fingerprint, t"wrong", port), t"demo", t"1.0", codec, 1)(_ => t"welcomed"))
            catch case error: Peer.Error => error.reason match
              case Peer.Error.Reason.Refused(reason) => reason
              case other                             => other.toString.tt
        . assert(_ == Peer.Refusal.token)

        test(m"a busy worker refuses"):
          serving(worker(() => Peer.Refusal.busy)): port =>
            try
              unsafely(Peer.connect(machine(identity.fingerprint, token, port), t"demo", t"1.0", codec, 1)(_ => t"welcomed"))
            catch case error: Peer.Error => error.reason match
              case Peer.Error.Reason.Refused(reason) => reason
              case other                             => other.toString.tt
        . assert(_ == Peer.Refusal.busy)

        test(m"another tool's protocol is refused"):
          serving(worker(() => Unset)): port =>
            try
              unsafely(Peer.connect(machine(identity.fingerprint, token, port), t"demo", t"1.0", pongCodec(t"pong"), 1)(_ => t"welcomed"))
            catch case error: Peer.Error => error.reason match
              case Peer.Error.Reason.Refused(reason) => reason
              case other                             => other.toString.tt
        . assert(_ == Peer.Refusal.protocol)

        test(m"a client pinning the wrong identity cannot connect"):
          serving(worker(() => Unset)): port =>
            val wrong: Data = Data.fill(32) { i => i.toByte }
            try
              unsafely(Peer.connect(machine(wrong, token, port), t"demo", t"1.0", codec, 1)(_ => t"welcomed"))
            catch case error: Peer.Error => error.reason match
              case Peer.Error.Reason.Unreachable(_) => t"unreachable"
              case other                            => other.toString.tt
        . assert(_ == t"unreachable")

        test(m"a machine declaring no identity is refused before connecting"):
          try
            unsafely(Peer.connect(Machine(t"x", t"127.0.0.1", 1, Unset, token, Nil), t"demo", t"1.0", codec, 1)(_ => t"welcomed"))
          catch case error: Peer.Error => error.reason match
            case Peer.Error.Reason.NoIdentity(name) => name
            case other                              => other.toString.tt
        . assert(_ == t"x")

// Counts the outcomes for the summary line. A class rather than local `var's, because the event
// sink is a pure `TestEvent -> Unit` and may capture nothing tracked.
private final class Tally:
  private var passes: Int = 0
  private var failures: Int = 0
  def passed: Int = passes
  def failed: Int = failures
  def record(pass: Boolean): Unit = if pass then passes += 1 else failures += 1
