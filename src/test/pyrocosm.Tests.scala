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
import cataclysm.*
import clavichord.*
import contingency.*
import denominative.*
import escapade.*
import fulminate.*
import gossamer.*
import harlequin.Scala
import symbolism.*
import jacinta.*
import probably.*
import punctuation.*
import quantitative.*
import rudiments.*
import spectacular.*
import stratiform.*
import turbulence.*
import vacuous.*
import yossarian.*

import anticipation.termcapDefinitions.xtermTrueColorTermcap
import contingency.strategies.throwUnsafely
import cataclysm.formatting.indentedCssFormatting
import escritoire.tableStyles.thickTableStyle
import fulminate.errorDiagnostics.emptyDiagnostics
import hieroglyph.charEncoders.utf8Encoder
import hieroglyph.textMetrics.uniformMetric
import ultimatum.palettes.solarizedDarkGaugePalette
import jacinta.discriminables.jsonByKindDiscriminable
import jacinta.formatting.compactJsonFormatting

case class Person(name: Text, age: Int)

// The shape of the model's `Inline` and `Block`: a sum whose variants recurse through a `List` of
// the sum itself. Both codecs must derive it in place, with no hand-anchored instance.
enum Node derives CanEqual:
  case Leaf(text: Text)
  case Branch(children: List[Node])

enum Direct derives CanEqual:
  case Leaf
  case Branch(left: Direct, value: Int, right: Direct)

object Tests extends Suite(m"Pyrocosm tests"):
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
          entries.stdlib.length == 2 && title == Inline.text(t"Person")

        case _ =>
          false
    . assert(_ == true)

    test(m"a list of values exhibits as a listing"):
      val exhibit: Inline | Block = (List(1, 2, 3): List[Int]).exhibit

      exhibit match
        case Block.Listing(false, items) => items.stdlib.length == 3
        case _                           => false
    . assert(_ == true)

    test(m"markdown converts node for node"):
      val exhibit: Block = Parser.parse(t"# Title\n\nSome *emphasis* here.").exhibit

      exhibit match
        case Block.Group(Block.Heading(1, _) :: Block.Paragraph(content) :: Nil) =>
          content.stdlib.exists:
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
      Interface(Inline.text(t"Gallery"), List(panel), List(verbose)).cells.stdlib.length
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
                  Inline.Toned(Tone.Success, Inline.text(t"ok")),
                  Inline.Link(Inline.Destination.Internal(run), Inline.text(t"again")),
                  Inline.Math(unsafely(Ergo.parse(t"(x↗2 + y↗2)"))),
                  Inline.Symbol(Glyph.Check),
                  Inline.Reference(t"a1b2c3"),
                  Inline.Amount(0.0123, t"s"),
                  Inline.Figure(3.5, 2),
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
            Block.Disclosure(Inline.text(t"more"), List(Block.Image(t"x.png", t"an image")), true),
            Block.Chart(Block.Chart.Kind.Sparkline, List(Block.Series(Inline.text(t"s"), List(1.0, 2.0)))) )

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
          columns.map(_.numeric) == List(false, true) && rows.stdlib.length == 2
        case _ =>
          false
    . assert(_ == true)

    test(m"highlighted source exhibits as a code block"):
      val exhibit: Block = Scala.highlight(t"val x = 1").exhibit
      exhibit match
        case Block.Code(Language.Scala, lines, _) => lines.stdlib.head.tokens.stdlib.head.accent == Token.Accent.Keyword
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
      renderer.block(prose, 40).stdlib.length
    . assert(_ > 2)

    val stretched: Block =
      Block.Table
        ( List
            ( Block.Column(Inline.text(t"Name"), sizing = Block.Sizing.Stretch),
              Block.Column(Inline.text(t"Time"), Block.Alignment.End, Block.Sizing.Rigid, true) ),
          List(Block.Row(List(Block.Cell(Inline.text(t"parse")), Block.Cell(List(Inline.Amount(0.5, t"s")))))) )

    test(m"a table with a stretch column spans exactly the width"):
      renderer.block(stretched, 60).stdlib.filter(_.plain.starts(t"┃")).map(_.length)
    . assert(_.forall(_ == 60))

    test(m"a table without a stretch column fits within the width"):
      renderer.block(rich, 60).stdlib.filter(_.plain.starts(t"┃")).map(_.length)
    . assert(_.forall(_ <= 60))

    test(m"a gauge line is exactly the width"):
      renderer.block(Block.Gauge(Status.Fraction(0.3), Inline.text(t"work")), 50).stdlib.head.length
    . assert(_ == 50)

    test(m"code notes leave the text intact"):
      val code = Block.Code(Language.Scala, List(Block.Line(List(Token(t"val", Token.Accent.Keyword), Token.plain(t" xs = 1")))),
          List(Block.Note(0, 2, 6, Block.Note.Style.Erroneous)))
      renderer.block(code, 80).stdlib.head.plain
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

    test(m"an actionable row carries its action's id"):
      html.block(rich).show.contains(t"""<tr id="${run.id}" class="pyro-action pyro-tone-success">""")
    . assert(_ == true)

    test(m"a tone renders as its class"):
      html.phrase(List(Inline.Toned(Tone.Failure, Inline.text(t"no")))).show
    . assert(_ == t"""<span class="pyro-toned pyro-tone-failure">no</span>""")

    test(m"a keystroke renders as a kbd element"):
      html.phrase(List(Inline.Keystroke(Keypress.Ctrl('C')))).show
    . assert(_.starts(t"<kbd"))

    test(m"the web arrangement maps every role"):
      val plan = WebArrangement.plan(arranged)
      (plan.navigation.map(_.id.label), plan.primary.map(_.id.label), plan.detail.map(_.id.label), plan.status.map(_.id.label))
    . assert(_ == (List(t"nav"), List(t"main"), List(t"detail"), List(t"status")))

    val page = PyrocosmPage(arranged, html, WebTheme.default).html.show

    test(m"the page has a content element for each panel"):
      arranged.panels.map { (panel: Panel) => page.contains(t"""id="${HtmlRenderer.panelId(panel)}-content"""") }
    . assert(_.all(_ == true))

    test(m"the page loads the generic script"):
      page.contains(t"""<script src="/pyrocosm.js" defer""")
    . assert(_ == true)

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

    test(m"a code field is an editable code element with its placeholder"):
      val page = PyrocosmPage(repl, html, WebTheme.default).html.show
      page.contains(t"""<code id="${prompt.input.id}" class="pyro-field pyro-editor" contenteditable="true"""") && page.contains(t"""<span class="pyro-placeholder">scala&gt;</span>""")
    . assert(_ == true)

    test(m"a page with a session of its own names it"):
      PyrocosmPage(repl, html, WebTheme.default, t"s1").html.show.contains(t"""<meta name="pyro-session" content="s1">""")
    . assert(_ == true)

    test(m"a decoration carries its tokens, note, marker and completions"):
      prompt.decoration() = Control.Field.Decoration
        ( List(Token(t"pri", Token.Accent.Term)),
          List(Control.Field.Completion(t"println", t"term", t"…"), Control.Field.Completion(t"/session main", t"command", t"", whole = true)),
          incomplete = true,
          detail = List(Block.paragraph(t"reads as code")) )

      val decoration = PyrocosmPage(repl, html, WebTheme.default).decoration(prompt).show
      List(t"pyro-tokens", t"pyro-incomplete", t"pyro-note", t"pyro-completions", t"println", t"reads as code", t"""<li class="pyro-whole">""").all(decoration.contains(_))
    . assert(_ == true)

    val stylesheet: Text = WebStyles.css(WebTheme.default).show

    test(m"the stylesheet declares the palette as custom properties"):
      stylesheet.contains(t"--pyro-bg: #002b36") && stylesheet.contains(t"--pyro-tone-success: #859900")
    . assert(_ == true)

    test(m"the stylesheet refers to colours only through variables"):
      stylesheet.contains(t"$$") || stylesheet.contains(t"Chroma")
    . assert(_ == false)

    test(m"the stylesheet reads back as CSS"):
      try stylesheet.read[Css].rules.stdlib.length
      catch case errors: Css.Errors =>
        java.lang.System.err.nn.println(s"CSS ERRORS: ${errors.errors.stdlib.map(e => s"${e.reason} at ${e.line}:${e.column}")}")
        0
    . assert(_ > 50)

    // ── On a terminal emulator ────────────────────────────────────────────────────────────

    test(m"rendered lines draw on a terminal emulator as their plain text"):
      val lines = renderer.blocks(List(rich), 80)
      val rendered = lines.map(_.render(xtermTrueColorTermcap)).join(t"\r\n")
      val pty = Pty(80, 40).consume(rendered)
      val first = lines.stdlib.head.plain

      (0 until first.length).forall { column => pty.buffer.char(column.z, Prim) == first.s.charAt(column) }
    . assert(_ == true)
