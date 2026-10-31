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

// Excluded from the umbrella: `Language` (cosmopolite), `Token` (harlequin), which would outrank
// this package's own definitions, since a wildcard import beats a package member declared in
// another file.
import soundness.{Language as _, Token as _, *}

import dysasymptotics.linearSize

// The exhibitions which need the compiler, and so exist only on the JVM: highlighted source from
// harlequin, the semantic markup the compiler embeds in its diagnostics, parsed by delicious,
// and the compiler's notices (anthology) as the model's diagnostics. The model carries the
// *results* of highlighting as its own `Token's, so these instances are the bridge from
// harlequin's vocabulary to the model's.
//
// Their subjects and the typeclass both live elsewhere, so the givens cannot sit in a
// companion; they are named, at package level, for import by name.

// harlequin's accents and roles are the model's, case for case.
extension (token: harlequin.Token)
  def model: Token =
    val accent = Token.Accent.valueOf(token.accent.toString)

    val role: Optional[Token.Role] =
      token.role.let: (role: harlequin.Role) => Token.Role.valueOf(role.toString)

    Token(token.text, accent, role)

// The style a diagnostic's severity marks with.
private def noteStyle(importance: harlequin.Diagnostic.Importance): Block.Note.Style =
  importance match
    case harlequin.Diagnostic.Importance.Error   => Block.Note.Style.Erroneous
    case harlequin.Diagnostic.Importance.Warning => Block.Note.Style.Caution
    case harlequin.Diagnostic.Importance.Info    => Block.Note.Style.Highlight

extension (code: SourceCode)
  // The lines' tokens, with an unclosed string accented as one to the end of its line, or of
  // the source after `"""`: the scanner recovers from one as a separator or plain text, which
  // would show a string being typed in another colour until its closing quote.
  def modelLines: List[Block.Line] =
    val lines: List[List[Token]] =
      code.lines.to[List].map: (line: List[harlequin.Token]) => line.map(_.model)

    val joined: List[Token] = lines.indexed.bind: (line: List[Token], index: Ordinal) =>
      if index.n0 == 0 then line else Token.plain(t"\n") :: line

    Block.Line.split(Token.closeStrings(joined))

  // The model's language for the language that highlighted the source.
  def modelLanguage: Language = code.language match
    case Scala => Language.Scala
    case Java  => Language.Java

  // The tokens of every line, joined by newline tokens, for phrasing use.
  def flattened: List[Token] =
    code.modelLines.indexed.bind: (line: Block.Line, index: Ordinal) =>
      val break: List[Token] = if index.n0 == 0 then Nil else List(Token.plain("\n"))
      break + line.tokens

  // The source's focus (an error's span) and its diagnostics as notes on the block's own
  // lines: harlequin counts a fragment's lines from its `offset` in the whole, the model from
  // the fragment's first line.
  def modelNotes: List[Block.Note] =
    val focus: List[Block.Note] =
      code.focus.lay(Nil): (span: Span) =>
        Block.Note.of(span, Block.Note.Style.Erroneous).lay(Nil)(List(_))

    val diagnostics: List[Block.Note] =
      code.diagnostics.bind: (diagnostic: harlequin.Diagnostic) =>
        Block.Note
        . of(diagnostic.span, noteStyle(diagnostic.importance), Inline.text(diagnostic.message))
        . lay(Nil)(List(_))

    (focus + diagnostics).map(_.shifted(lines = code.offset))

  // The source as a code block. A fragment cut from a whole by `extract` or `fragment` knows
  // where it came from, so it is numbered from there; a highlight of a whole text is not, unless
  // asked.
  def modelCode(numbered: Boolean = code.offset > 0): Block.Code =
    Block.Code
      ( code.modelLanguage,
        code.modelLines,
        code.modelNotes,
        if numbered then code.offset + 1 else Unset )

given sourceCodePresentable: SourceCode is Presentable in Block = _.modelCode()

// A diagnostic's markup, as phrasing: code samples and types are highlighted as Scala terms and
// types; symbols and names appear as code; anything else is its text.
given markupPresentable: List[Markup] is Presentable in Inline = markup =>
  Inline.Phrase(markup.map(markupInline))

private def highlighted(text: Text, context: Scala.Context): Inline =
  Inline.Snippet(Language.Scala, Scala.highlight(text, context).flattened)

private def markupInline(markup: Markup): Inline = markup match
  case Markup.Textual(text)        => Inline.Textual(text)
  case Markup.Code(_, _)           => highlighted(markup.plain, Scala.Context.Term)
  case Markup.Typed(_, _, _, _)    => highlighted(markup.plain, Scala.Context.Type)
  case Markup.Symbolic(_, _, _, _) => highlighted(markup.plain, Scala.Context.Term)

  case Markup.Named(isType, _, _) =>
    highlighted(markup.plain, if isType then Scala.Context.Type else Scala.Context.Term)

  case Markup.Spanned(_, _, children) => Inline.Phrase(children.map(markupInline))

// A compiler's notice as a diagnostic. The message is read for the semantic markup the compiler
// embeds (delicious), so that a type shows as a type and a code sample as code, a multi-line
// sample as a code block of its own, and the lines of a missing-given search (frontier's tree)
// keep their marks; a message with no markup is its lines. Given the highlighted lines of the
// file, the excerpt around the notice's span is cut from them, numbered, with the span marked.
object Notices:
  def tone(importance: anthology.Importance): Tone = importance match
    case anthology.Importance.Error   => Tone.Failure
    case anthology.Importance.Warning => Tone.Warning
    case anthology.Importance.Info    => Tone.Info

  def style(importance: anthology.Importance): Block.Note.Style = importance match
    case anthology.Importance.Error   => Block.Note.Style.Erroneous
    case anthology.Importance.Warning => Block.Note.Style.Caution
    case anthology.Importance.Info    => Block.Note.Style.Highlight

  // A compiler run with colour writes its escapes into the message; they are not markup.
  def stripAnsi(text: Text): Text = text.s.replaceAll("\u001b\\[[0-9;]*m", "").nn.tt

  // The lines of a Scala source, highlighted once for the excerpts of every notice about it.
  def lines(source: Text): List[Block.Line] = Scala.highlight(source).modelLines

  // `types` renders a type marker's text: by default the compiler's own, which a caller with
  // stenography replaces by the abbreviated form.
  def diagnostic
    ( notice:  anthology.Notice,
      lines:   List[Block.Line]    = Nil,
      context: Int                 = 1,
      types:   Markup.Typed => Text = _.plain )
  :   Block.Diagnostic =

    val location: Optional[Block.Location] =
      if notice.file == t"" then Unset
      else notice.span.lay(Block.Location(notice.file)): (span: Span) =>
        Block.Location.of(notice.file, span)

    val message: List[Block] =
      notice.markup.let(SemanticMessage.parse(_))
      . lay(paragraphs(stripAnsi(notice.message).trim)): message => blocks(message.markup, types)

    val excerpt: List[Block] =
      if lines.nil then Nil else
        notice.span.lay(Nil): (span: Span) =>
          Block.Note.of(span, style(notice.importance)).lay(Nil): (note: Block.Note) =>
            List(Block.Code.excerpt(Language.Scala, lines, List(note), context))

    Block.Diagnostic(tone(notice.importance), location, message, excerpt)

  // Runs of phrasing become one paragraph, broken at the message's own newlines; a code sample
  // with a newline in it becomes a code block between paragraphs.
  def blocks(nodes: List[Markup], types: Markup.Typed => Text = _.plain): List[Block] =
    // Both accumulators are built in reverse and turned round when they are drained: `run` is
    // the phrasing of the paragraph being assembled, `blocks` the blocks completed so far.
    var blocks: List[Block] = Nil
    var run: List[Inline] = Nil

    // A paragraph is trimmed of the breaks and blank text at either end.
    def blank(node: Inline): Boolean = node match
      case Inline.Break()       => true
      case Inline.Textual(text) => text.blank
      case _                    => false

    def flush(): Unit =
      val trimmed: List[Inline] = run.skip(blank).reverse.skip(blank)
      if !trimmed.nil then blocks = Block.Paragraph(trimmed) :: blocks
      run = Nil

    def phrase(markup: Markup): Unit = markup match
      case Markup.Textual(text) =>
        stripAnsi(text).cut(t"\n").indexed.each: (line: Text, index: Ordinal) =>
          if index.n0 > 0 then run = Inline.Break() :: run
          if line != t"" then run = lineInlines(line).reverse + run

      case Markup.Code(_, _) =>
        if markup.plain.contains(t"\n") then
          flush()

          blocks =
            Scala.highlight(stripAnsi(markup.plain), Scala.Context.Term).modelCode(false) :: blocks

        else
          run = highlighted(markup.plain, Scala.Context.Term) :: run

      case typed: Markup.Typed =>
        run = highlighted(types(typed), Scala.Context.Type) :: run

      case Markup.Symbolic(_, _, _, children) => children.each(phrase)
      case Markup.Named(_, _, children)       => children.each(phrase)
      case Markup.Spanned(_, _, children)     => children.each(phrase)

    nodes.each(phrase)
    flush()
    blocks.reverse

  // A message's lines stay together in one paragraph, broken where the message breaks.
  private def paragraphs(text: Text): List[Block] =
    val content: List[Inline] =
      text.cut(t"\n").indexed.bind: (line: Text, index: Ordinal) =>
        val break: List[Inline] = if index.n0 == 0 then Nil else List(Inline.Break())
        break + lineInlines(line)

    List(Block.Paragraph(content))

  // The keywords of a missing-given diagnostic's tree.
  private val keywords: Set[Text] =
    Set(t"resolving", t"found", t"requires", t"candidate", t"propose")

  // One line of a message as phrasing: a line of a missing-given diagnostic's tree (frontier's
  // `■ resolving Foo`, `└─ ▸ propose bar`, and so on) has its mark toned, its keyword kept and
  // its name highlighted as the type or value it is; any other line is text.
  private def lineInlines(line: Text): List[Inline] = line match
    case r"$prefix([\s│├└─]*)$mark([■✓✗▪▸])\s+$keyword([a-z]+)\s+$name(\S.*?)$label(\s+\(.*\))?"
      if keywords.has(keyword) =>
      val tone: Tone = mark match
        case t"■" => Tone.Accent
        case t"✓" => Tone.Success
        case t"✗" => Tone.Failure
        case t"▪" => Tone.Warning
        case _    => Tone.Info

      val context = if keyword == t"propose" then Scala.Context.Term else Scala.Context.Type

      val labelled: List[Inline] =
        label.lay(Nil): (label: Text) => List(Inline.Toned(Tone.Muted, Inline.text(label)))

      List
        ( Inline.Toned(Tone.Muted, Inline.text(prefix)),
          Inline.Toned(tone, Inline.text(mark)),
          Inline.Textual(t" $keyword "),
          highlighted(name, context) ) +
        labelled

    case _ => List(Inline.Textual(line))

given noticePresentable: anthology.Notice is Presentable in Block = Notices.diagnostic(_)
