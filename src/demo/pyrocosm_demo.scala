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

import scala.caps

// Excluded from the umbrella: `Control` (coaxial), `Glyph` (phoenicia), `Language` (cosmopolite),
// `Standing` (ultimatum), `Status` (exoskeleton), `Step` (ultimatum), `Token` (harlequin) and
// `Tool` (anthology), which would outrank this package's own definitions, since a wildcard
// import beats a package member declared in another file.
import soundness.{Control as _, Glyph as _, Language as _, Standing as _, Status as _, Step as _, Token as _, Tool as _, *}

import murmuration.zip

import dysasymptotics.{linearAccess, linearSize}
import strategies.throwUnsafely
import backstops.silentBackstop
import tableStyles.thickTableStyle
import executives.completionsExecutive
import interpreters.posixInterpreter
import textMetrics.uniformMetric
import probates.cancelProbate
import threading.platformThreading
import palettes.solarizedDarkGaugePalette
import webserverErrorPages.minimalErrorPage

// The gallery: one interface with every kind of node, every role and priority, a ticking gauge,
// a selectable table driving a detail panel, a log a button appends to, and a code field with
// fake decorations. The fixture for judging appearance and the arrangement vocabulary. Its
// matter is a tour in sections, each showing one family of nodes as the stylesheet sets them;
// the navigation panel, a choice and a pair of buttons move between them.
object Samples:
  val next = Action(t"next")
  val previous = Action(t"previous")
  val disabled = Action(t"disabled")
  val append = Action(t"append")
  val clear = Action(t"clear")
  val link = Action(t"link")
  val prompt = Input(t"prompt")
  val filter = Input(t"filter")
  val note = Input(t"note")
  val verbose = Toggle(t"verbose")
  val section = Choice(t"section")

  case class Benchmark(name: Text, mean: Double, ratio: Double)

  val samples: List[Benchmark] =
    List(Benchmark(t"parse", 0.00042, 1.0), Benchmark(t"encode", 0.0011, 2.6), Benchmark(t"decode", 0.00087, 2.1))

  val benchmarks: List[(Benchmark, Action)] =
    samples.map { (benchmark: Benchmark) => benchmark -> Action(benchmark.name) }

  def results: Block =
    Block.Table
      ( List
          ( Block.Column(Inline.text(t"Benchmark"), sizing = Block.Sizing.Stretch),
            Block.Column(Inline.text(t"Mean"), Block.Alignment.End, Block.Sizing.Rigid, true),
            Block.Column(Inline.text(t"Ratio"), Block.Alignment.End, Block.Sizing.Rigid, true) ),
        benchmarks.map: (benchmark, action) =>
          Block.Row
            ( List
                ( Block.Cell(Inline.text(benchmark.name)),
                  Block.Cell(List(Inline.Amount(benchmark.mean, t"s"))),
                  Block.Cell(List(Inline.Figure(benchmark.ratio, 1))) ),
              if benchmark.ratio == 1.0 then Tone.Success else Unset,
              action ),
        Inline.text(t"Select a row with Up, Down and Enter") )

  // A test suite of the tables section: each row has a tone by its outcome and an action which
  // shows it in the detail panel.
  case class Suite(name: Text, tests: Int, seconds: Double, outcome: Text, tone: Optional[Tone])

  val suiteSamples: List[Suite] =
    List
      ( Suite(t"model", 42, 0.83, t"passed", Tone.Success),
        Suite(t"terminal", 17, 2.4, t"failed", Tone.Failure),
        Suite(t"web", 23, 1.1, t"warned", Tone.Warning),
        Suite(t"notes", 5, 0.2, t"skipped", Unset) )

  val suites: List[(Suite, Action)] = suiteSamples.map { (suite: Suite) => suite -> Action(suite.name) }

  private def inlineCode(text: Text, accent: Token.Accent): Inline = Inline.Code(Language.Scala, List(Token(text, accent)))

  // Every case of an enum, as a list, through its `Enumerable` evidence.
  private def cases[enumeration <: scala.reflect.Enum: Enumerable as enumerable]: List[enumeration] =
    enumerable.values.to[List]

  private def toned(tone: Tone, text: Text): Inline = Inline.Toned(tone, Inline.text(text))

  // The phrasing `items`, each followed by `separator` but the last, which `terminal` follows.
  private def punctuated(items: List[List[Inline]], separator: Text = t", ", terminal: Text = t"."): List[Inline] =
    val joined: List[Inline] =
      items.indexed.bind { (item, index) => if index.n0 == 0 then item else Inline.Textual(separator) :: item }

    val end: List[Inline] = List(Inline.Textual(terminal))
    joined + end

  def overview: List[Block] =
    List
      ( Block.Heading(1, Inline.text(t"Every node")),
        Block.Paragraph:
          List
            ( Inline.Textual(t"Press "),
              Inline.Keystroke(Keypress.Tab),
              Inline.Textual(t" to move focus and "),
              Inline.Keystroke(Keypress.Escape),
              Inline.Textual(t" to leave. This paragraph has "),
              Inline.Emphasis(Inline.text(t"emphasis")),
              Inline.Textual(t", a "),
              Inline.Toned(Tone.Success, Inline.text(t"success tone")),
              Inline.Textual(t", a reference "),
              Inline.Reference(t"a1b2c3"),
              Inline.Textual(t", an amount "),
              Inline.Amount(0.00042, t"s"),
              Inline.Textual(t", a symbol "),
              Inline.Symbol(Glyph.Check),
              Inline.Textual(t" and some code "),
              Inline.Code(Language.Scala, List(Token(t"val", Token.Accent.Keyword), Token.plain(t" "), Token(t"x", Token.Accent.Term))),
              Inline.Textual(t".") ),
        results,
        Block.Code
          ( Language.Scala,
            List
              ( Block.Line(List(Token(t"def", Token.Accent.Keyword), Token.plain(t" "), Token(t"greet", Token.Accent.Term, Token.Role.Binding), Token(t"(", Token.Accent.Parens), Token(t"name", Token.Accent.Term), Token(t": ", Token.Accent.Symbol), Token(t"Text", Token.Accent.Typal), Token(t")", Token.Accent.Parens), Token(t" = ", Token.Accent.Symbol), Token(t"t\"Hello, $$name\"", Token.Accent.String))) ),
            List(Block.Note(0, 4, 9, Block.Note.Style.Highlight)) ),
        Block.Notice(Tone.Warning, Inline.text(t"A notice"), List(Block.paragraph(t"With a rule beneath."), Block.Rule())),
        Block.Listing(false, List(Block.Item(List(Block.paragraph(t"first"))), Block.Item(List(Block.paragraph(t"second"))))),
        Block.Record(List(Block.Entry(Inline.text(t"name"), List(Block.paragraph(t"gallery"))), Block.Entry(Inline.text(t"version"), List(Block.paragraph(t"0.1.0")))), Inline.text(t"Record")),
        Block.Tree(List(Block.TreeNode(Inline.text(t"root"), List(Block.TreeNode(Inline.text(t"leaf")), Block.TreeNode(Inline.text(t"branch"), List(Block.TreeNode(Inline.text(t"leaf")))))))),
        Block.Graph
          ( List(Block.Vertex(t"a", Inline.text(t"model")), Block.Vertex(t"b", Inline.text(t"terminal")), Block.Vertex(t"c", Inline.text(t"web"))),
            List(Block.Edge(t"b", t"a"), Block.Edge(t"c", t"a")) ),
        Block.Chart(Block.Chart.Kind.Sparkline, List(Block.Series(Inline.text(t"load"), List(1.0, 3.0, 2.0, 5.0, 4.0, 6.0, 3.0)))),
        Block.Chart(Block.Chart.Kind.Bars, List(Block.Series(Inline.text(t"sizes"), List(12.0, 30.0, 7.0)))),
        Block.Disclosure(Inline.text(t"More"), List(Block.paragraph(t"Hidden by default in a browser; shown here.")), true) )

  def typography: List[Block] =
    List
      ( Block.Heading(1, Inline.text(t"Typography")),
        Block.paragraph(t"Titles are set in Marcellus, prose in Yantramanav, and code and labels in Sono: at fixed width for code, at variable width for the small tracked capitals of labels and buttons."),
        Block.Heading(2, Inline.text(t"A second-level heading")),
        Block.paragraph(t"Headings step down by level, each in the title face, with the space above them growing with their rank."),
        Block.Heading(3, Inline.text(t"A third-level heading")),
        Block.Heading(4, Inline.text(t"A fourth-level heading")),
        Block.Paragraph:
          List
            ( Inline.Textual(t"Prose may carry "),
              Inline.Emphasis(Inline.text(t"emphasis")),
              Inline.Textual(t", a link to "),
              Inline.Link(Inline.Destination.External(t"https://propensive.dev/pyrocosm/"), Inline.text(t"the website")),
              Inline.Textual(t" and one which "),
              Inline.Link(Inline.Destination.Internal(link), Inline.text(t"acts within the page")),
              Inline.Textual(t". Keystrokes such as "),
              Inline.Keystroke(Keypress.Ctrl('C')),
              Inline.Textual(t", "),
              Inline.Keystroke(Keypress.Tab),
              Inline.Textual(t" and "),
              Inline.Keystroke(Keypress.Escape),
              Inline.Textual(t" are keycaps; a reference "),
              Inline.Reference(t"7f3a9c2"),
              Inline.Textual(t" is monospaced; amounts such as "),
              Inline.Amount(1536.0, t"B"),
              Inline.Textual(t" and "),
              Inline.Amount(0.0123, t"s"),
              Inline.Textual(t" are scaled to a unit; a figure "),
              Inline.Figure(3.14159, 2),
              Inline.Textual(t" keeps its precision; and mathematics "),
              Inline.Math(unsafely(Ergo.parse(t"(x↗2 + y↗2)"))),
              Inline.Textual(t" is set in a mathematical face.") ),
        Block.Quotation(List(Block.paragraph(t"A quotation is set apart with a rule at its side, in the muted colour."))),
        Block.Heading(2, Inline.text(t"Listings")),
        Block.Listing(true, List(Block.Item(List(Block.paragraph(t"An ordered listing"))), Block.Item(List(Block.paragraph(t"numbers its items"))), Block.Item(List(Block.paragraph(t"in the text face"))))),
        Block.Listing(false, List(Block.Item(List(Block.paragraph(t"An unordered listing"))), Block.Item(List(Block.paragraph(t"sets its items plainly, without bullets"))))),
        Block.Rule(),
        Block.paragraph(t"A rule between blocks is a hairline in the border colour.") )

  def tones: List[Block] =
    val each: List[(Tone, Text)] =
      List
        ( Tone.Success -> t"success", Tone.Failure -> t"failure", Tone.Warning -> t"warning",
          Tone.Muted -> t"muted", Tone.Accent -> t"accent", Tone.Info -> t"info" )

    val notices: List[Block] = each.map: (tone, name) =>
      val article = if name.starts(t"a") || name.starts(t"i") then t"An" else t"A"
      Block.Notice(tone, Inline.text(t"$article $name notice"), List(Block.paragraph(t"Its edge and title take the tone, and its background is tinted with it.")))

    val glyphs: List[List[Inline]] = cases[Glyph].map: (glyph: Glyph) =>
      List(Inline.Symbol(glyph), Inline.Textual(t" "), Inline.Textual(glyph.toString.tt.lower))

    val phrasing: List[List[Inline]] = each.map { (tone, name) => List(toned(tone, name)) }

    val intro: List[Block] =
      List
        ( Block.Heading(1, Inline.text(t"Tones and notices")),
          Block.Paragraph(Inline.Textual(t"Six tones colour phrasing: ") :: punctuated(phrasing)),
          Block.Heading(2, Inline.text(t"Notices")) )

    val symbols: List[Block] =
      List(Block.Heading(2, Inline.text(t"Glyphs")), Block.Paragraph(punctuated(glyphs)))

    intro + notices + symbols

  def tables: List[Block] =
    val table =
      Block.Table
        ( List
            ( Block.Column(Inline.text(t"Suite"), sizing = Block.Sizing.Stretch),
              Block.Column(Inline.text(t"Tests"), Block.Alignment.End, Block.Sizing.Rigid, true),
              Block.Column(Inline.text(t"Time"), Block.Alignment.End, Block.Sizing.Rigid, true),
              Block.Column(Inline.text(t"Outcome")) ),
          suites.map: (suite, action) =>
            Block.Row
              ( List
                  ( Block.Cell(Inline.text(suite.name)),
                    Block.Cell(List(Inline.Figure(suite.tests.toDouble, 0))),
                    Block.Cell(List(Inline.Amount(suite.seconds, t"s"))),
                    Block.Cell(List(suite.tone.lay(Inline.Textual(suite.outcome)) { tone => toned(tone, suite.outcome) })) ),
                suite.tone,
                action ),
          Inline.text(t"Test suites by outcome; select a row to see it in the detail panel") )

    List
      ( Block.Heading(1, Inline.text(t"Tables and records")),
        Block.paragraph(t"Column headers are small tracked capitals; rows are divided by hairlines; numeric columns align to the right in tabular figures; a toned row is tinted, and an actionable one lifts under the pointer."),
        table,
        Block.Heading(2, Inline.text(t"Records")),
        Block.Record
          ( List
              ( Block.Entry(Inline.text(t"version"), List(Block.paragraph(t"0.2.1"))),
                Block.Entry(Inline.text(t"licence"), List(Block.paragraph(t"Apache 2.0"))),
                Block.Entry(Inline.text(t"upstream"), List(Block.Paragraph(List(Inline.Textual(t"Soundness "), Inline.Reference(t"0.67.0"))))) ),
            Inline.text(t"Release") ),
        Block.Heading(2, Inline.text(t"Trees and graphs")),
        Block.Tree
          ( List
              ( Block.TreeNode
                  ( Inline.text(t"pyrocosm"),
                    List
                      ( Block.TreeNode(Inline.text(t"model"), tone = Tone.Success),
                        Block.TreeNode(Inline.text(t"terminal"), List(Block.TreeNode(Inline.text(t"fixtures"), tone = Tone.Warning))),
                        Block.TreeNode(Inline.text(t"web"), List(Block.TreeNode(Inline.text(t"styles")), Block.TreeNode(Inline.text(t"frontend"), tone = Tone.Failure))) ) ) ) ),
        Block.Graph
          ( List
              ( Block.Vertex(t"a", Inline.text(t"model"), Tone.Success),
                Block.Vertex(t"b", Inline.text(t"terminal"), Tone.Info),
                Block.Vertex(t"c", Inline.text(t"web"), Tone.Info),
                Block.Vertex(t"d", Inline.text(t"demo"), Tone.Accent) ),
            List(Block.Edge(t"b", t"a"), Block.Edge(t"c", t"a"), Block.Edge(t"d", t"b"), Block.Edge(t"d", t"c")) ) )

  def codeSection: List[Block] =
    val greet =
      Block.Line
        ( List
            ( Token(t"def", Token.Accent.Keyword), Token.plain(t" "), Token(t"greet", Token.Accent.Term, Token.Role.Binding),
              Token(t"(", Token.Accent.Parens), Token(t"name", Token.Accent.Term), Token(t": ", Token.Accent.Symbol),
              Token(t"Text", Token.Accent.Typal), Token(t")", Token.Accent.Parens), Token(t": ", Token.Accent.Symbol),
              Token(t"Text", Token.Accent.Typal), Token(t" = ", Token.Accent.Symbol), Token(t"t\"Hello, $$name\"", Token.Accent.String) ) )

    val count =
      Block.Line(List(Token(t"val", Token.Accent.Keyword), Token.plain(t" "), Token(t"count", Token.Accent.Term, Token.Role.Binding), Token(t" = ", Token.Accent.Symbol), Token(t"42", Token.Accent.Number)))

    val call =
      Block.Line(List(Token(t"greet", Token.Accent.Term), Token(t"(", Token.Accent.Parens), Token(t"oops", Token.Accent.Error), Token(t")", Token.Accent.Parens)))

    val imported =
      Block.Line(List(Token(t"import", Token.Accent.Keyword), Token.plain(t" "), Token(t"legacy", Token.Accent.Term), Token(t".", Token.Accent.Symbol), Token(t"*", Token.Accent.Symbol)))

    val notes =
      List
        ( Block.Note(0, 4, 9, Block.Note.Style.Highlight, t"a highlighted span"),
          Block.Note(0, 10, 14, Block.Note.Style.Param, t"a parameter"),
          Block.Note(2, 6, 10, Block.Note.Style.Erroneous, t"Not found: oops"),
          Block.Note(3, 7, 13, Block.Note.Style.Caution, t"legacy is deprecated") )

    val accents: List[List[Inline]] =
      List
        ( List(inlineCode(t"def", Token.Accent.Keyword), Inline.Textual(t" keyword")),
          List(inlineCode(t"inline", Token.Accent.Modifier), Inline.Textual(t" modifier")),
          List(inlineCode(t"\"text\"", Token.Accent.String), Inline.Textual(t" string")),
          List(inlineCode(t"42", Token.Accent.Number), Inline.Textual(t" number")),
          List(inlineCode(t"value", Token.Accent.Term), Inline.Textual(t" term")),
          List(inlineCode(t"Text", Token.Accent.Typal), Inline.Textual(t" type")),
          List(inlineCode(t"=>", Token.Accent.Symbol), Inline.Textual(t" symbol")),
          List(inlineCode(t"()", Token.Accent.Parens), Inline.Textual(t" parens")),
          List(inlineCode(t"oops", Token.Accent.Error), Inline.Textual(t" error")),
          List(inlineCode(t"/quit", Token.Accent.Command), Inline.Textual(t" command")) )

    List
      ( Block.Heading(1, Inline.text(t"Code and output")),
        Block.paragraph(t"A code block sits on the page colour behind a hairline, each token accented by its kind. Notes mark spans of a line: a highlight, a parameter, an error and a caution; hover one for its caption."),
        Block.Code(Language.Scala, List(greet, count, call, imported), notes),
        Block.Paragraph(Inline.Textual(t"Inline code takes the same accents: ") :: punctuated(accents)),
        Block.Heading(2, Inline.text(t"Captured output")),
        Block.paragraph(t"A program's output is shown verbatim behind a gutter naming its stream: info for standard output, failure for standard error."),
        Block.Output(t"compiling 3 sources\ndone in 1.2 s\n"),
        Block.Output(t"warning: unused import legacy.*\n", error = true),
        Block.Heading(2, Inline.text(t"Stack traces")),
        Block.paragraph(t"A stack trace is laid out as Soundness lays one out in a terminal: each package takes an accent in turn, the class's last segment is bold, a repeated class or file recedes, the location is one word aligned on its colon, and each cause follows the last."),
        trace )

  // A real trace, of an exception with a cause, thrown and caught here.
  def trace: Block =
    def inner(): Nothing = throw IllegalStateException("the index was rebuilt while it was being read")

    def outer(): Nothing throws java.io.IOException =
      try inner() catch case error: IllegalStateException => throw java.io.IOException("could not read the index", error)

    try outer() catch case error: java.io.IOException => Block.Trace.of(StackTrace(error))

  def charts: List[Block] =
    val all: List[Standing] = cases[Standing]

    val standings: List[Block] = all.map: (standing: Standing) =>
      Block.Gauge(Status.Standing(standing), Inline.text(standing.toString.tt.lower))

    val steps =
      Status.Steps(all.map { (standing: Standing) => Step(Inline.text(standing.toString.tt.lower), standing) })

    val stepped: List[Block] = List(Block.Heading(3, Inline.text(t"Steps")), Block.Gauge(steps, Inline.text(t"steps")))

    val intro: List[Block] =
      List
        ( Block.Heading(1, Inline.text(t"Charts and gauges")),
        Block.paragraph(t"A chart is a description list of its series: a sparkline is a run of block glyphs, a bar is a meter drawn by the browser against the largest value."),
        Block.Chart
          ( Block.Chart.Kind.Sparkline,
            List
              ( Block.Series(Inline.text(t"load"), List(1.0, 3.0, 2.0, 5.0, 4.0, 6.0, 3.0, 4.0, 7.0, 5.0)),
                Block.Series(Inline.text(t"memory"), List(4.0, 4.0, 5.0, 5.0, 6.0, 6.0, 7.0, 7.0, 8.0, 8.0)) ) ),
        Block.Chart
          ( Block.Chart.Kind.Bars,
            List
              ( Block.Series(Inline.text(t"model"), List(42.0)),
                Block.Series(Inline.text(t"terminal"), List(17.0)),
                Block.Series(Inline.text(t"web"), List(23.0)) ) ),
        Block.Chart(Block.Chart.Kind.Histogram, List(Block.Series(Inline.text(t"latency"), List(2.0, 9.0, 14.0, 8.0, 5.0, 3.0, 1.0, 1.0)))),
        Block.Heading(2, Inline.text(t"Gauges")),
        Block.paragraph(t"A gauge shows a status: a fraction, an indeterminate wait, a reckoning against a total or without one, a duration elapsed or remaining, a standing, or a run of steps."),
        Block.Gauge(Status.Fraction(0.65), Inline.text(t"fraction")),
        Block.Gauge(Status.Indeterminate(), Inline.text(t"indeterminate")),
        Block.Gauge(Status.Reckoning(17, 120), Inline.text(t"reckoning")),
        Block.Gauge(Status.Reckoning(17, Unset), Inline.text(t"count")),
        Block.Gauge(Status.Elapsed(96.5), Inline.text(t"elapsed")),
        Block.Gauge(Status.Remaining(12.0), Inline.text(t"remaining")),
        Block.Heading(3, Inline.text(t"Standings")) )

    intro + standings + stepped

  // The drawings section's figure: a bar chart the session revises in place every so often,
  // so its bars glide to their new heights. The axis is a thin `rect`: honeycomb reads `rect`
  // as a void element but not `line`, so a self-closing `line` does not parse.
  def bars(heights: List[Int]): Text =
    val fills = List(t"#d9480f", t"#b45309", t"#6d4c9f")
    val rects = heights.zip(fills).indexed.map: (pair, index) =>
      val (height, fill) = pair
      val x = 40 + index.n0*90
      t"""<rect x="${x.toString}" y="${(100 - height).toString}" width="60" height="${height.toString}" fill="$fill" rx="3"/>"""

    t"""<g id="bars">${rects.join}</g>"""

  def drawing(heights: List[Int]): Text =
    t"""<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 320 120" width="480" height="180" role="img">${bars(heights)}"""
    + t"""<rect x="20" y="100" width="280" height="1" fill="currentColor" fill-opacity="0.3"/>"""
    + t"""<g fill="currentColor" font-family="Inter, sans-serif" font-size="7" text-anchor="middle">"""
    + t"""<text x="70" y="114">model</text><text x="160" y="114">terminal</text><text x="250" y="114">web</text></g></svg>"""

  val heights: List[Int] = List(80, 50, 65)
  val throughput: Figure = Figure(Inline.text(t"Throughput by module"), drawing(heights))

  def drawings: List[Block] =
    List
      ( Block.Heading(1, Inline.text(t"Drawings")),
        Block.paragraph(t"A figure is an SVG drawing the page revises in place: this one's bars glide to new heights every few seconds, and its caption is set small and muted."),
        Block.Figure(throughput),
        Block.Disclosure
          ( Inline.text(t"How it is drawn"),
            List
              ( Block.paragraph(t"The application replaces the group of bars by its id; the stylesheet transitions a rectangle's geometry, so the browser animates the change."),
                code(t"figure.replace(part, markup, svg)") ) ) )

  // The tour, in order. `overview` is what `gallery static` prints. Lazy, as the drawings
  // section tokenizes a line, and the tokeniser's word lists are declared below.
  case class Section(name: Text, action: Action, content: List[Block])

  lazy val sections: List[Section] =
    List
      ( Section(t"Overview", Action(t"overview"), overview),
        Section(t"Typography", Action(t"typography"), typography),
        Section(t"Tones", Action(t"tones"), tones),
        Section(t"Tables", Action(t"tables"), tables),
        Section(t"Code", Action(t"code"), codeSection),
        Section(t"Charts", Action(t"charts"), charts),
        Section(t"Drawings", Action(t"drawings"), drawings) )

  // The navigation panel's listing: every section whose name matches the filter, the current
  // one emphasised in the accent.
  def navigation(current: Int, filter: Text): List[Block] =
    val items: List[Block.Item] = sections.indexed.bind: (section: Section, index: Ordinal) =>
      if filter != t"" && !section.name.lower.contains(filter.lower) then Nil
      else
        val label: List[Inline] =
          if index.n0 == current then List(Inline.Toned(Tone.Accent, List(Inline.Emphasis(Inline.text(section.name)))))
          else Inline.text(section.name)

        List(Block.Item(List(Block.Paragraph(label)), section.action))

    List(Block.Listing(false, items))

  def interface
    ( navigation: Live[List[Block]],
      matter:     Live[List[Block]],
      progress:   Live[List[Block]],
      log:        Live[List[Block]],
      detail:     Live[List[Block]],
      field:      Control.Field,
      filter:     Control.Field,
      note:       Control.Field,
      choice:     Control.Choice )
  :   Interface =

    Interface
      ( Inline.text(t"Pyrocosm gallery"),
        List
          ( Panel(Panel.Id(t"nav"), Panel.Role.Navigation, Inline.text(t"Sections"), navigation, Panel.Priority.Important, controls = List(filter)),
            Panel
              ( Panel.Id(t"main"), Panel.Role.Primary, Unset, matter, Panel.Priority.Essential,
                controls = List(Control.Button(Inline.text(t"Previous"), previous), Control.Button(Inline.text(t"Next"), next), Control.Button(Inline.text(t"Disabled"), disabled, Live(false)), choice) ),
            Panel(Panel.Id(t"detail"), Panel.Role.Detail, Inline.text(t"Detail"), detail, Panel.Priority.Peripheral),
            Panel(Panel.Id(t"log"), Panel.Role.Log, Inline.text(t"Log"), log, Panel.Priority.Important, controls = List(note), hints = Hints(hints.Follow, hints.terminal.MaxRows(8))),
            Panel(Panel.Id(t"prompt"), Panel.Role.Prompt, Unset, Live(Nil: List[Block]), Panel.Priority.Essential, controls = List(field)),
            Panel(Panel.Id(t"status"), Panel.Role.Status, Unset, progress, Panel.Priority.Important, hints = Hints(hints.terminal.Border.None)) ),
        List(Control.Button(Inline.text(t"Append"), append), Control.Button(Inline.text(t"Clear"), clear), Control.Toggle(verbose, Inline.text(t"Verbose"))),
        List(Shortcut(Keypress.Ctrl('L'), clear, Inline.text(t"clear the log"))) )

  // A toy tokeniser and completer, standing in for a compiler.
  private val keywords: List[Text] = List(t"val", t"def", t"if", t"else", t"then", t"import", t"given")
  private val words: List[Text] = List(t"println", t"print", t"printf", t"parse", t"partition", t"pyrocosm")

  // The tokens of one line: words by accent, with the spaces between them.
  def tokenize(line: Text): List[Token] =
    line.cut(t" ").indexed.bind: (word, index) =>
      val accent =
        if keywords.has(word) then Token.Accent.Keyword
        else if word.starts(t"\"") then Token.Accent.String
        else if word.s.forall(_.isDigit) && word != t"" then Token.Accent.Number
        else Token.Accent.Term
      val space: List[Token] = if index.n0 == 0 then Nil else List(Token.plain(t" "))
      space + List(Token(word, accent))

  // A code block of the text, one line per line.
  def code(text: Text): Block =
    Block.Code(Language.Scala, text.cut(t"\n").map { (line: Text) => Block.Line(tokenize(line)) })

  def decorate(text: Text, caret: Int): Control.Field.Decoration =
    // The tokens cover the whole text, with a newline token between lines, as a field's
    // decoration must.
    val tokens: List[Token] = text.cut(t"\n").indexed.bind: (line: Text, index: Ordinal) =>
      val break: List[Token] = if index.n0 == 0 then Nil else List(Token.plain(t"\n"))
      break + tokenize(line)

    val before = text.s.substring(0, caret.min(text.length)).nn
    val stem = before.reverse.takeWhile(_.isLetter).reverse.tt
    val completions =
      if stem.length < 2 then Nil
      else words.filter(_.starts(stem)).map { (word: Text) => Control.Field.Completion(word, t"term", t"…") }

    // A `/`-command completes as a whole line, as a REPL's do.
    val commands: List[Text] = List(t"/session", t"/set", t"/quit")
    val commanded: List[Control.Field.Completion] =
      if text.starts(t"/") && !text.contains(t" ") then commands.filter(_.starts(text)).map { (command: Text) => Control.Field.Completion(command, t"command", t"", whole = true) }
      else Nil

    // Prose, rather than code: several words and no keyword. A REPL would submit it elsewhere.
    val parts: List[Text] = text.cut(t" ")
    val prose: Boolean = parts.count(_ != t"") >= 3 && !parts.exists(keywords.has(_))

    // What the line has brought into scope, as a REPL reports it: every `val x` so far.
    val bindings: List[Text] = parts.zip(parts.skip(1)).sweep { case (t"val", name) if name != t"" => name }

    val read: List[Block] =
      if prose then List(Block.Paragraph(List(Inline.Toned(Tone.Muted, Inline.text(t"reads as prose"))))) else Nil

    val scope: List[Inline] = bindings.indexed.bind: (name: Text, index: Ordinal) =>
      val comma: List[Inline] = if index.n0 == 0 then Nil else List(Inline.Textual(t", "))
      val binding: List[Inline] =
        List(Inline.Code(Language.Scala, List(Token(name, Token.Accent.Term, Token.Role.Binding),
            Token(t": ", Token.Accent.Symbol), Token(t"Int", Token.Accent.Typal))))

      comma + binding

    val scoped: List[Block] =
      if bindings.nil then Nil
      else List(Block.Paragraph(Inline.Toned(Tone.Muted, Inline.text(t"⤷ scope: ")) :: scope))

    val detail: List[Block] = read + scoped

    // Any `oops` is an error, marked on its line and span, as a compiler's diagnostic would be.
    val marks: List[Block.Note] = text.cut(t"\n").indexed.bind: (line, index) =>
      val at = line.s.indexOf("oops")
      if at < 0 then Nil else List(Block.Note(index.n0, at, at + 4, Block.Note.Style.Erroneous))

    Control.Field.Decoration(tokens, commanded + completions, incomplete = text.s.count(_ == '(') > text.s.count(_ == ')'), detail = detail, marks = marks)

// One session of the gallery: the live cells, the ticking gauge, the code field and the event
// handler. Built once per run, and handed to whichever frontend the arguments choose, so the
// terminal and the browser show the same interface driven by the same logic.
class GallerySession():
  val progress: Live[List[Block]] = Live(Nil)
  val log: Live[List[Block]] = Live(List(Block.paragraph(t"Started.")))
  val detail: Live[List[Block]] = Live(List(Block.paragraph(t"Nothing selected.")))
  val navigation: Live[List[Block]] = Live(Samples.navigation(0, t""))
  val matter: Live[List[Block]] = Live(Samples.overview)

  val field: Control.Field =
    Control.Field(Samples.prompt, Control.Field.Kind.Code(Language.Scala), notification = Control.Field.Notify.Keystrokes, placeholder = t"type here")

  val filter: Control.Field =
    Control.Field(Samples.filter, Control.Field.Kind.Line, notification = Control.Field.Notify.Keystrokes, placeholder = t"Filter sections")

  val note: Control.Field =
    Control.Field(Samples.note, Control.Field.Kind.Multiline, placeholder = t"Add a note to the log; Enter submits")

  val choice: Control.Choice =
    Control.Choice(Samples.section, Samples.sections.map { (section: Samples.Section) => Inline.text(section.name) })

  val interface: Interface =
    Samples.interface(navigation, matter, progress, log, detail, field, filter, note, choice)

  private var fraction = 0.0
  private var count = 0
  private var current = 0
  private var filtered: Text = t""
  private var ticks = 0

  // Show a section by its index, which wraps around; the navigation marks it, the choice names
  // it, and the detail panel describes it.
  private def show(index: Int): Unit =
    val size = Samples.sections.size
    current = ((index % size) + size) % size

    Samples.sections.at(current.z).let: (section: Samples.Section) =>
      matter() = section.content
      navigation() = Samples.navigation(current, filtered)
      choice.current() = current
      detail() = List
        ( Block.Record
            ( List
                ( Block.Entry(Inline.text(t"section"), List(Block.paragraph(section.name))),
                  Block.Entry(Inline.text(t"position"), List(Block.paragraph(t"${(current + 1).toString} of ${size.toString}"))),
                  Block.Entry(Inline.text(t"blocks"), List(Block.paragraph(section.content.size.toString.tt))) ),
              Inline.text(section.name) ) )

  private val ticker = Thread(new Runnable:
    def run(): Unit =
      while true do
        Thread.sleep(150)
        ticks += 1
        fraction = if fraction >= 1.0 then 0.0 else fraction + 0.02
        progress() = List
          ( Block.Gauge(Status.Fraction(fraction), Inline.text(t"progress")),
            Block.Gauge(Status.Steps(List(Step(Inline.text(t"resolve"), Standing.Succeeded), Step(Inline.text(t"compile"), Standing.Running), Step(Inline.text(t"test"), Standing.Pending)))) )

        // Every few seconds the drawing's bars take new heights, which the page animates.
        if ticks % 20 == 0 then
          val heights = List(30 + (ticks*7) % 60, 30 + (ticks*11) % 60, 30 + (ticks*13) % 60)
          Samples.throughput.replace(t"bars", Samples.bars(heights), Samples.drawing(heights)))

  ticker.setDaemon(true)
  ticker.start()

  def handle(event: Event): Unit = event match
    case Event.Pressed(Samples.append) =>
      count += 1
      log.append(Block.paragraph(t"Appended line $count."))

    case Event.Pressed(Samples.clear) =>
      log() = Nil

    case Event.Pressed(Samples.previous) =>
      show(current - 1)

    case Event.Pressed(Samples.next) =>
      show(current + 1)

    case Event.Pressed(Samples.link) =>
      log.append(Block.paragraph(t"Followed the link within the page."))

    case Event.Pressed(action) =>
      Samples.sections.indexed.seek(_(0).action == action).let { (_, index) => show(index.n0) }

      Samples.benchmarks.seek(_(1) == action).let: (benchmark, _) =>
        detail() = List(Block.Record(List(Block.Entry(Inline.text(t"name"), List(Block.paragraph(benchmark.name))), Block.Entry(Inline.text(t"mean"), List(Block.Paragraph(List(Inline.Amount(benchmark.mean, t"s")))))), Inline.text(benchmark.name)))

      Samples.suites.seek(_(1) == action).let: (suite, _) =>
        val outcome: Inline = suite.tone.lay(Inline.Textual(suite.outcome)) { tone => Inline.Toned(tone, Inline.text(suite.outcome)) }
        detail() = List
          ( Block.Record
              ( List
                  ( Block.Entry(Inline.text(t"suite"), List(Block.paragraph(suite.name))),
                    Block.Entry(Inline.text(t"tests"), List(Block.Paragraph(List(Inline.Figure(suite.tests.toDouble, 0))))),
                    Block.Entry(Inline.text(t"time"), List(Block.Paragraph(List(Inline.Amount(suite.seconds, t"s"))))),
                    Block.Entry(Inline.text(t"outcome"), List(Block.Paragraph(List(outcome)))) ),
                Inline.text(suite.name) ) )

      log.append(Block.paragraph(t"Pressed ${action.label}."))

    case Event.Chosen(Samples.section, index) =>
      show(index)

    case Event.Edited(Samples.filter, text, _) =>
      filtered = text
      navigation() = Samples.navigation(current, filtered)

    case Event.Edited(_, text, caret) =>
      field.decoration() = Samples.decorate(text, caret)

    case Event.Submitted(Samples.note, text) =>
      if text != t"" then log.append(Block.Quotation(List(Block.paragraph(text))))

    case Event.Submitted(_, text) =>
      log.append(Samples.code(text))
      field.decoration() = Control.Field.Decoration()

    case Event.Toggled(_, state) =>
      log.append(Block.paragraph(t"Verbose: ${state.toString}"))

    case Event.Key(keypress) =>
      log.append(Block.Paragraph(List(Inline.Textual(t"Key: "), Inline.Keystroke(keypress))))

    case _ =>
      ()

object Repl:
  // A REPL: an inline transcript and a prompt. Each submission becomes a code entry with a
  // placeholder that settles a moment later, so settled entries are committed to the scrollback
  // (in the terminal) while the placeholder stays live. Built per session: a web tab or a
  // terminal each has one of its own.
  def apply()(using Monitor, Probate): (Interface, Event -> Unit) =
    val transcript: Live[List[Block]] = Live(Nil)
    val field = Control.Field(Input(t"repl"), Control.Field.Kind.Code(Language.Scala), notification = Control.Field.Notify.Keystrokes, placeholder = t"type Scala; Tab completes; Escape leaves", history = Live(List(t"val earlier = 1")))

    val interface =
      Interface
        ( Inline.text(t"Pyrocosm REPL"),
          List
            ( Panel(Panel.Id(t"transcript"), Panel.Role.Transcript, Unset, transcript, Panel.Priority.Essential, hints = Hints(hints.terminal.Border.None)),
              Panel(Panel.Id(t"prompt"), Panel.Role.Prompt, Unset, Live(Nil: List[Block]), Panel.Priority.Essential, controls = List(field), hints = Hints(hints.terminal.Border.None)) ),
          hints = Hints(hints.terminal.Occupancy.Inline) )

    var count = 0

    def handle(event: Event): Unit = event match
      case Event.Edited(_, text, caret) =>
        field.decoration() = Samples.decorate(text, caret)

      case Event.Submitted(_, text) =>
        if text == t"/clear" then transcript() = Nil else
          count += 1
          val n = count
          field.history.append(text)
          val marks = Samples.decorate(text, text.length).marks
          val code = Samples.code(text) match
            case Block.Code(language, lines, _) => Block.Code(language, lines, marks)
            case other                          => other

          val pending = Block.Group(List(code, Block.Gauge(Status.Indeterminate(), Inline.text(t"evaluating"))))
          transcript.append(pending)
          field.decoration() = Control.Field.Decoration()

          async:
            snooze(1.5*Second)
            val output = Block.Output(t"printed by the program\nand a second line\n")
            val settled = Block.Group(List(code, output, Block.Output(t"a complaint\n", error = true), Block.Paragraph(List(Inline.Toned(Tone.Success, Inline.text(t"res$n: Int = ${text.length.toString}"))))))
            transcript.amend { entries => entries.map { (entry: Block) => if entry eq pending then settled else entry } }

      case _ =>
        ()

    // The handler's settle task needs the monitor, which outlives every session; the handler
    // is vouched pure so a frontend can keep it.
    (interface, caps.unsafe.unsafeAssumePure(handle))

// The gallery as a Pyrocosm tool: `about`, `install`, `quit` and `--version` come from `Tool`,
// as does its configuration (`.pyrocosm/gallery/config.tel`, `~/.config/gallery/config.tel`);
// a `serve` there serves the gallery from the daemon on the configured `port`.
val Gallery: Tool =
  Tool
    ( t"gallery",
      prose = t"The Pyrocosm gallery shows every node of the Pyrocosm model, in the terminal or "
            + t"in a browser, as the fixture for judging how each appears.",
      web   = GalleryWeb )

// The gallery served by the daemon, at most once: `serve` returns when `stop` is called.
object GalleryWeb extends Tool.Web:
  def port: Int = 8080

  // The frontend holds the monitor and the error page, which outlive it; vouched pure so it
  // can be stopped from another invocation.
  @caps.unsafe.untrackedCaptures
  @volatile
  private var frontend: Optional[WebFrontend] = Unset

  def serve(port: Int)(using Monitor, Probate): Unit =
    val session = GallerySession()
    val running: WebFrontend = caps.unsafe.unsafeAssumePure(WebFrontend(port))
    frontend = running
    running.run(session.interface)(session.handle)

  def stop(): Unit = frontend.let(_.stop())

// `gallery` or `gallery terminal` runs the interface in the terminal; `gallery serve [port]`
// serves it as a web page; `gallery static [columns]` prints the overview once, for a look
// without a terminal session. The standard subcommands are handled first, by `Tool`.
@main
def gallery(arguments: Text*): Unit = cli:
  Gallery.standard:
    execute:
      // The invocation's capabilities are tracked; the frontend takes them as pure parameters,
      // so they are sealed once here, as flame does for every command.
      given Console = caps.unsafe.unsafeAssumePure(summon[Console])
      given Environment = caps.unsafe.unsafeAssumePure(summon[exoskeleton.Invocation].environment)

      given Stdio = caps.unsafe.unsafeAssumePure(summon[exoskeleton.Invocation].stdio)

      val words: List[Text] = summon[exoskeleton.Cli].arguments.map { (argument: Argument) => argument() }
      def number(default: Int): Int = words.at(Sec).let { (word: Text) => safely(word.as[Int]) }.or(default)

      words.at(Prim) match
        case t"static" =>
          val renderer = TerminalRenderer()
          renderer.blocks(Samples.overview, number(100)).each { (line: Teletype) => Out.println(line) }
          Exit.Ok

        // `gallery repl`: the REPL in the terminal.
        case t"repl" =>
          supervise:
            import parasite.probates.cancelProbate
            val (interface, handle) = Repl()
            TerminalFrontend().run(interface)(handle)
          Exit.Ok

        // `gallery serve [port]` serves the gallery, one interface for every tab; `gallery serve
        // repl [port]` serves the REPL, a session per tab.
        case t"serve" =>
          val repl: Boolean = words.at(Sec) == t"repl"
          val port = words.at(if repl then Ter else Sec).let { (word: Text) => safely(word.as[Int]) }.or(8080)
          supervise:
            import parasite.probates.cancelProbate
            if repl then
              Out.println(t"Serving the Pyrocosm REPL at http://localhost:${port.toString}/")
              WebFrontend(port).serve(() => Repl())
            else
              val session = GallerySession()
              Out.println(t"Serving the Pyrocosm gallery at http://localhost:${port.toString}/")
              WebFrontend(port).run(session.interface)(session.handle)
          Exit.Ok

        case _ =>
          supervise:
            val session = GallerySession()
            TerminalFrontend().run(session.interface)(session.handle)
          Exit.Ok
