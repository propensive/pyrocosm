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

import ambience.*
import anticipation.*
import clavichord.*
import ethereal.cli
import exoskeleton.{execute, Argument}
import symbolism.*
import contingency.*
import escapade.*
import gossamer.*
import turbulence.*
import parasite.*
import quantitative.*
import profanity.*
import rudiments.*
import vacuous.*

import contingency.strategies.throwUnsafely
import exoskeleton.backstops.silentBackstop
import escritoire.tableStyles.thickTableStyle
import exoskeleton.executives.completionsExecutive
import exoskeleton.interpreters.posixInterpreter
import hieroglyph.textMetrics.uniformMetric
import parasite.probates.cancelProbate
import parasite.threading.platformThreading
import ultimatum.palettes.solarizedDarkGaugePalette
import scintillate.webserverErrorPages.minimalErrorPage

// The gallery: one interface with every kind of node, every role and priority, a ticking gauge,
// a selectable table driving a detail panel, a log a button appends to, and a code field with
// fake decorations. The fixture for judging appearance and the arrangement vocabulary.
object Samples:
  val next = Action(t"next")
  val append = Action(t"append")
  val clear = Action(t"clear")
  val prompt = Input(t"prompt")
  val verbose = Toggle(t"verbose")

  case class Benchmark(name: Text, mean: Double, ratio: Double)

  val benchmarks: List[(Benchmark, Action)] =
    List(Benchmark(t"parse", 0.00042, 1.0), Benchmark(t"encode", 0.0011, 2.6), Benchmark(t"decode", 0.00087, 2.1))
    . map { (benchmark: Benchmark) => benchmark -> Action(benchmark.name) }

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

  def interface(progress: Live[List[Block]], log: Live[List[Block]], detail: Live[List[Block]], field: Control.Field): Interface =
    Interface
      ( Inline.text(t"Pyrocosm gallery"),
        List
          ( Panel(Panel.Id(t"nav"), Panel.Role.Navigation, Inline.text(t"Sections"), Live(List(Block.Listing(false, List(Block.Item(List(Block.paragraph(t"Overview")), next), Block.Item(List(Block.paragraph(t"Results")), next))))), Panel.Priority.Important),
            Panel(Panel.Id(t"main"), Panel.Role.Primary, Inline.text(t"Overview"), Live(overview), Panel.Priority.Essential),
            Panel(Panel.Id(t"detail"), Panel.Role.Detail, Inline.text(t"Detail"), detail, Panel.Priority.Peripheral),
            Panel(Panel.Id(t"log"), Panel.Role.Log, Inline.text(t"Log"), log, Panel.Priority.Important, hints = Hints(hints.Follow, hints.terminal.MaxRows(8))),
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
    val tokens: List[Token] = text.cut(t"\n").indexed.bind: (line, index) =>
      (if index.n0 == 0 then Nil else List(Token.plain(t"\n"))) + tokenize(line)

    val before = text.s.substring(0, caret.min(text.length)).nn
    val stem = before.reverse.takeWhile(_.isLetter).reverse.tt
    val completions =
      if stem.length < 2 then Nil
      else words.filter(_.starts(stem)).map { (word: Text) => Control.Field.Completion(word, t"term", t"…") }

    // Prose, rather than code: several words and no keyword. A REPL would submit it elsewhere.
    val prose: Boolean = text.cut(t" ").stdlib.count(_ != t"") >= 3 && !text.cut(t" ").exists(keywords.has(_))
    val note: Optional[List[Inline]] = if prose then Inline.text(t"reads as prose") else Unset

    Control.Field.Decoration(tokens, completions, incomplete = text.s.count(_ == '(') > text.s.count(_ == ')'), note = note)

// One session of the gallery: the live cells, the ticking gauge, the code field and the event
// handler. Built once per run, and handed to whichever frontend the arguments choose, so the
// terminal and the browser show the same interface driven by the same logic.
class GallerySession():
  val progress: Live[List[Block]] = Live(Nil)
  val log: Live[List[Block]] = Live(List(Block.paragraph(t"Started.")))
  val detail: Live[List[Block]] = Live(List(Block.paragraph(t"Nothing selected.")))

  val field: Control.Field =
    Control.Field(Samples.prompt, Control.Field.Kind.Code(Language.Scala), notification = Control.Field.Notify.Keystrokes, placeholder = t"type here")

  val interface: Interface = Samples.interface(progress, log, detail, field)

  private var fraction = 0.0
  private var count = 0

  private val ticker = Thread(new Runnable:
    def run(): Unit =
      while true do
        Thread.sleep(150)
        fraction = if fraction >= 1.0 then 0.0 else fraction + 0.02
        progress() = List
          ( Block.Gauge(Status.Fraction(fraction), Inline.text(t"progress")),
            Block.Gauge(Status.Steps(List(Step(Inline.text(t"resolve"), Standing.Succeeded), Step(Inline.text(t"compile"), Standing.Running), Step(Inline.text(t"test"), Standing.Pending)))) ))

  ticker.setDaemon(true)
  ticker.start()

  def handle(event: Event): Unit = event match
    case Event.Pressed(Samples.append) =>
      count += 1
      log.append(Block.paragraph(t"Appended line $count."))

    case Event.Pressed(Samples.clear) =>
      log() = Nil

    case Event.Pressed(action) =>
      Samples.benchmarks.seek(_(1) == action).let: (benchmark, _) =>
        detail() = List(Block.Record(List(Block.Entry(Inline.text(t"name"), List(Block.paragraph(benchmark.name))), Block.Entry(Inline.text(t"mean"), List(Block.Paragraph(List(Inline.Amount(benchmark.mean, t"s")))))), Inline.text(benchmark.name)))
      log.append(Block.paragraph(t"Pressed ${action.label}."))

    case Event.Edited(_, text, caret) =>
      field.decoration() = Samples.decorate(text, caret)

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
    val field = Control.Field(Input(t"repl"), Control.Field.Kind.Code(Language.Scala), notification = Control.Field.Notify.Keystrokes, placeholder = t"type Scala; Tab completes; Escape leaves")

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
        count += 1
        val n = count
        val code = Samples.code(text)
        val pending = Block.Group(List(code, Block.Gauge(Status.Indeterminate(), Inline.text(t"evaluating"))))
        transcript.append(pending)
        field.decoration() = Control.Field.Decoration()

        async:
          snooze(1.5*Second)
          val settled = Block.Group(List(code, Block.Paragraph(List(Inline.Toned(Tone.Success, Inline.text(t"res$n: Int = ${text.length.toString}"))))))
          transcript.amend { entries => entries.map { (entry: Block) => if entry eq pending then settled else entry } }

      case _ =>
        ()

    // The handler's settle task needs the monitor, which outlives every session; the handler
    // is vouched pure so a frontend can keep it.
    (interface, caps.unsafe.unsafeAssumePure(handle))

// `gallery` or `gallery terminal` runs the interface in the terminal; `gallery serve [port]`
// serves it as a web page; `gallery static [columns]` prints the overview once, for a look
// without a terminal session.
@main
def gallery(arguments: Text*): Unit = cli:
  execute:
    // The invocation's capabilities are tracked; the frontend takes them as pure parameters, so
    // they are sealed once here, as flame does for every command.
    given Console = caps.unsafe.unsafeAssumePure(summon[Console])
    given Environment = caps.unsafe.unsafeAssumePure(summon[exoskeleton.Invocation].environment)

    given Stdio = caps.unsafe.unsafeAssumePure(summon[exoskeleton.Invocation].stdio)

    val words: List[Text] = summon[exoskeleton.Cli].arguments.map { (argument: Argument) => argument() }
    def number(default: Int): Int = words.stdlib.lift(1).flatMap(_.s.toIntOption).getOrElse(default)

    words.stdlib.headOption match
      case Some(t"static") =>
        val renderer = TerminalRenderer()
        renderer.blocks(Samples.overview, number(100)).each { (line: Teletype) => Out.println(line) }
        Exit.Ok

      // `gallery repl`: the REPL in the terminal.
      case Some(t"repl") =>
        supervise:
          import parasite.probates.cancelProbate
          val (interface, handle) = Repl()
          TerminalFrontend().run(interface)(handle)
        Exit.Ok

      // `gallery serve [port]` serves the gallery, one interface for every tab; `gallery serve
      // repl [port]` serves the REPL, a session per tab.
      case Some(t"serve") =>
        val repl: Boolean = words.stdlib.lift(1).contains(t"repl")
        val port = words.stdlib.lift(if repl then 2 else 1).flatMap(_.s.toIntOption).getOrElse(8080)
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
