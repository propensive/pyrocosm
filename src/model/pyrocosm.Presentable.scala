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

import scala.compiletime

import anticipation.*
import clavichord.*
import denominative.*
import digression.*
import fulminate.*
import gossamer.*
import prepositional.*
import punctuation.*
import quantitative.*
import rudiments.*
import spectacular.*
import vacuous.*
import wisteria.*
import denominative.dysasymptotics.linearSize

// A value's rich rendering: what `show` is to text and `inspect` is to a debugger, `exhibit` is
// to an interface. An instance yields phrasing (`in Inline`) or flow (`in Block`) content, and
// the renderers take it from there, so a value displays in the terminal and on the web through
// one instance.
//
// Resolution follows `Inspectable`: a type's own instance first; then its `Showable`, as plain
// text; then a structural rendering derived from its shape (a product as a record, a sum by its
// variant); and, as a last resort, its `toString`.
object Presentable extends Presentable2:
  // Constructors that fix `Form`, so that an instance can be written as a lambda.
  def phrase[value](lambda: value => Inline): value is Presentable in Inline = lambda(_)
  def flow[value](lambda: value => Block): value is Presentable in Block = lambda(_)

  // Promote an exhibit to flow content: a phrase becomes a paragraph.
  def blocks(exhibit: Inline | Block): List[Block] = exhibit match
    case block: Block   => List(block)
    case inline: Inline => List(Block.Paragraph(List(inline)))

  def block(exhibit: Inline | Block): Block = exhibit match
    case block: Block   => block
    case inline: Inline => Block.Paragraph(List(inline))

  object Derivation extends Derivable[Presentable]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Presentable in Block =

      value =>
        val entries = fields(value): [field] => field =>
          Block.Entry(Inline.text(label), blocks(contextual.exhibit(field)))

        val title: Optional[List[Inline]] = if tuple then Unset else Inline.text(typeName)
        Block.Record(List.from(entries.readable), title)

    inline def disjunction[derivation: SumReflection]: derivation is Presentable in Block =
      value =>
        variant(value):
          [variant <: derivation] => variant => block(contextual.exhibit(variant))

    // A cell is phrasing: a field exhibited as a block is reduced to its paragraph's content, or
    // to an ellipsis where it has no single line.
    private def cell(exhibit: Inline | Block): List[Inline] = exhibit match
      case inline: Inline                => List(inline)
      case Block.Paragraph(content)      => content
      case _                             => List(Inline.Symbol(Glyph.Ellipsis))

    private def numeric(cells: List[List[Inline]]): Boolean =
      !cells.nil && cells.all:
        case Inline.Figure(_, _) :: Nil => true
        case Inline.Amount(_, _) :: Nil => true
        case _                          => false

    inline def tabulate[element <: Product: ProductReflection](list: List[element]): Block =
      val titles: List[Text] = List.from(contexts[element]() { [field] => context => label }.readable)

      val rows: List[List[List[Inline]]] = list.map: (element: element) =>
        List.from(fields(element) { [field] => field => cell(contextual.exhibit(field)) }.readable)

      val columns: List[Block.Column] = titles.indexed.map: (title, index) =>
        val column = rows.map { (row: List[List[Inline]]) => row.stdlib(index.n0) }
        val isNumeric = numeric(column)
        val alignment = if isNumeric then Block.Alignment.End else Block.Alignment.Start
        val sizing = if isNumeric then Block.Sizing.Rigid else Block.Sizing.Paragraph
        Block.Column(Inline.text(title), alignment, sizing, isNumeric)

      Block.Table(columns, rows.map { (row: List[List[Inline]]) => Block.Row(row.map { (cell: List[Inline]) => Block.Cell(cell) }) })

  given inline: Inline is Presentable in Inline = identity(_)
  given block: Block is Presentable in Block = identity(_)
  given text: Text is Presentable in Inline = Inline.Textual(_)
  given int: Int is Presentable in Inline = int => Inline.Figure(int.toDouble, 0)
  given long: Long is Presentable in Inline = long => Inline.Figure(long.toDouble, 0)
  given double: Double is Presentable in Inline = Inline.Figure(_)
  given keypress: Keypress is Presentable in Inline = Inline.Keystroke(_)

  // Inline, so that the units are known where the given is summoned. A lambda rather than an
  // anonymous class: at an inline site the class form infers a capture variable for `Self` and
  // then fails the override check.
  inline given quantity: [units <: Measure] => Quantity[units] is Presentable in Inline =
    phrase[Quantity[units]] { quantity => Inline.Amount.of(quantity) }

  // A list of case classes is a table, a column per field titled by the field's name, as
  // escritoire tabulates one. A column whose every cell is a figure or an amount is numeric.
  inline given table: [element <: Product: ProductReflection]
  =>  List[element] is Presentable in Block =
    flow[List[element]] { list => Derivation.tabulate(list) }

  // A message's nested emphasis levels collapse to the model's single emphasis.
  given message: Message is Presentable in Inline = message =>
    val content: List[Inline] =
      message.fold[List[Inline]](Nil): (acc, next, level) =>
        acc :+ (if level == 0 then Inline.Textual(next) else Inline.Emphasis(Inline.text(next)))

    Inline.Phrase(content)

  given error: Error is Presentable in Inline = error => message.exhibit(error.message)

  given stackTrace: StackTrace is Presentable in Block = stackTrace =>
    val columns = List
      ( Block.Column(Inline.text("Class")),
        Block.Column(Inline.text("Method")),
        Block.Column(Inline.text("File"), sizing = Block.Sizing.Collapsible(0.5)),
        Block.Column(Inline.text("Line"), Block.Alignment.End, Block.Sizing.Rigid, numeric = true) )

    val rows = stackTrace.frames.map: (frame: StackTrace.Frame) =>
      Block.Row:
        List
          ( Block.Cell(List(Inline.Code(Language.Scala, List(Token.plain(frame.displayClass))))),
            Block.Cell(List(Inline.Code(Language.Scala, List(Token.plain(frame.displayMethod))))),
            Block.Cell(Inline.text(frame.file)),
            Block.Cell(frame.line.lay(Nil: List[Inline]) { line => List(Inline.Figure(line.toDouble, 0)) }) )

    Block.Notice
      ( Tone.Failure,
        Inline.text(stackTrace.className),
        List(Block.Paragraph(List(message.exhibit(stackTrace.message))), Block.Table(columns, rows)) )

  // A Markdown document, converted node for node. Both emphasis strengths become the model's
  // one emphasis; inline HTML is dropped; an inline image becomes a link to it.
  given markdown: (Markdown of Layout) is Presentable in Block = markdown =>
    Block.Group(markdown.children.map(layout))

  private def phrasing(nodes: List[Prose]): List[Inline] = nodes.map(prose)

  private def prose(node: Prose): Inline = node match
    case Prose.Textual(text)       => Inline.Textual(text)
    case Prose.Emphasis(children*) => Inline.Emphasis(phrasing(children.toList.to(List)))
    case Prose.Strong(children*)   => Inline.Emphasis(phrasing(children.toList.to(List)))
    case Prose.Code(code)          => Inline.Code(Language.Plain, List(Token.plain(code)))
    case Prose.Softbreak           => Inline.Textual(" ")
    case Prose.Linebreak           => Inline.Break()
    case Prose.HtmlInline(_)       => Inline.Textual("")

    case Prose.Link(destination, _, content*) =>
      Inline.Link(Inline.Destination.External(destination), phrasing(content.toList.to(List)))

    case Prose.Image(destination, _, content*) =>
      Inline.Link(Inline.Destination.External(destination), phrasing(content.toList.to(List)))

  private def layout(node: Layout): Block = node match
    case Layout.Paragraph(_, prose*)      => Block.Paragraph(phrasing(prose.toList.to(List)))
    case Layout.Heading(_, level, prose*) => Block.Heading(level, phrasing(prose.toList.to(List)))
    case Layout.BlockQuote(_, layouts*)   => Block.Quotation(layouts.toList.to(List).map(layout))
    case Layout.ThematicBreak(_)          => Block.Rule()
    case Layout.HtmlBlock(_, html)        => Block.Code(Language("html"), lines(html))

    case Layout.CodeBlock(_, info, content) =>
      Block.Code(info.prim.lay(Language.Plain)(Language(_)), lines(content))

    case Layout.BulletList(_, _, items*) =>
      Block.Listing(false, items.toList.to(List).map { (item: List[Layout]) => Block.Item(item.map(layout)) })

    case Layout.OrderedList(_, _, _, _, items*) =>
      Block.Listing(true, items.toList.to(List).map { (item: List[Layout]) => Block.Item(item.map(layout)) })

  private def lines(content: Text): List[Block.Line] =
    content.cut(t"\n").map { (line: Text) => Block.Line(List(Token.plain(line))) }

trait Presentable2 extends Presentable3:
  // Below `table` in priority, so a list of products tabulates; above `derived`, so a list of
  // anything else lists rather than falling through to its `Showable`.
  given list: [element: Presentable as presentable] => List[element] is Presentable in Block =
    list =>
      Block.Listing(false, list.map { (element: element) => Block.Item(Presentable.blocks(presentable.exhibit(element))) })

trait Presentable3:
  // The structural fallback applies to products only: a sum (a stdlib `List` or `Option`,
  // say, whose variants are not the point) falls through to its `Showable` or its `toString`
  // rather than to a record of its variant. A sum exhibits by variant through an explicit
  // `derives Presentable`.
  inline given derived: [value] => value is Presentable = compiletime.summonFrom:
    case given (`value` is Showable)       => Presentable.phrase[value] { value => Inline.Textual(value.show) }
    case given ProductReflection[`value`]  => Presentable.Derivation.derived[value]
    case _                                 => Presentable.phrase[value] { value => Inline.Textual(value.toString.tt) }

trait Presentable extends Typeclass.Pure, Formal:
  type Form <: Inline | Block
  def exhibit(value: Self): Form

