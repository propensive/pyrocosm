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
import soundness.*

import dysasymptotics.linearAccess

// A value's rich rendering: what `show` is to text and `inspect` is to a debugger, `exhibit` is
// to an interface. An instance yields phrasing (`in Inline`) or flow (`in Block`) content, and
// the renderers take it from there, so a value displays in the terminal and on the web through
// one instance.
//
// Resolution follows `Inspectable`: a type's own instance first; then its `Showable`, as plain
// text; then a structural rendering derived from its shape (a product as a record, a sum by its
// variant); and, as a last resort, its `Inspectable`, which itself ends in `toString`.
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
        Block.Record(entries.to[List], title)

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
        case Inline.Numeral(_, _) :: Nil => true
        case Inline.Amount(_, _) :: Nil  => true
        case _                           => false

    inline def tabulate[element <: Product: ProductReflection](list: List[element]): Block =
      val titles: List[Text] = contexts[element]() { [field] => context => label }.to[List]

      val rows: List[List[List[Inline]]] = list.map: (element: element) =>
        fields(element) { [field] => field => cell(contextual.exhibit(field)) }.to[List]

      val columns: List[Block.Column] = titles.indexed.map: (title, index) =>
        val column = rows.map: (row: List[List[Inline]]) => row.at(index).or(Nil)
        val isNumeric = numeric(column)
        val alignment = if isNumeric then Block.Alignment.End else Block.Alignment.Start
        val sizing = if isNumeric then Block.Sizing.Rigid else Block.Sizing.Paragraph
        Block.Column(Inline.text(title), alignment, sizing, isNumeric)

      val cells = rows.map: (row: List[List[Inline]]) =>
        Block.Row(row.map { (cell: List[Inline]) => Block.Cell(cell) })

      Block.Table(columns, cells)

  given inline: Inline is Presentable in Inline = identity(_)
  given block: Block is Presentable in Block = identity(_)
  given text: Text is Presentable in Inline = Inline.Textual(_)
  given int: Int is Presentable in Inline = int => Inline.Numeral(int.toDouble, 0)
  given long: Long is Presentable in Inline = long => Inline.Numeral(long.toDouble, 0)
  given double: Double is Presentable in Inline = Inline.Numeral(_)
  given keypress: Keypress is Presentable in Inline = Inline.Keystroke(_)

  // Inline, so that the units are known where the given is summoned. A lambda rather than an
  // anonymous class: at an inline site the class form infers a capture variable for `Self` and
  // then fails the override check.
  inline given quantity: [units <: Measure] => Quantity[units] is Presentable in Inline =
    phrase[Quantity[units]]: quantity => Inline.Amount.of(quantity)

  // A list of case classes is a table, a column per field titled by the field's name, as
  // escritoire tabulates one. A column whose every cell is a figure or an amount is numeric.
  inline given table: [element <: Product: ProductReflection]
  =>  List[element] is Presentable in Block =
    flow[List[element]]: list => Derivation.tabulate(list)

  // A message's nested emphasis levels collapse to the model's single emphasis.
  given message: Message is Presentable in Inline =
    message => Inline.Phrase(Inline.message(message))

  given error: Error is Presentable in Inline = error => message.exhibit(error.message)

  // A stack trace is the model's own block, laid out as digression's terminal rendering is; an
  // exception exhibits through its stack trace, resolved as far as the resolver in scope can.
  given stackTrace: StackTrace is Presentable in Block = Block.Trace.of(_)

  given throwable: (resolver: StackTrace.Resolver) => Throwable is Presentable in Block =
    throwable => Block.Trace.of(StackTrace(throwable))

  // The tree-shaped data formats, each as a `Block.Tree` of collapsible nodes (see `Trees`):
  // the root and its children start open, anything deeper closed.
  given json: Json is Presentable in Block = json => Trees.json(Json.unseal(json))
  given jsonAst: Json.Ast is Presentable in Block = Trees.json(_)
  given yaml: Yaml is Presentable in Block = yaml => Trees.yaml(yaml.root)
  given yamlAst: Yaml.Ast is Presentable in Block = Trees.yaml(_)
  given cbor: Cbor is Presentable in Block = cbor => Trees.cbor(Cbor.unseal(cbor))
  given cborAst: Cbor.Ast is Presentable in Block = Trees.cbor(_)
  given tel: Tel is Presentable in Block = Trees.tel(_)

  // Any XML node, since `Self` is exact and a parsed document is typed by its node kind.
  given xml: [xml <: Xml] => xml is Presentable in Block = Trees.xml(_)

  // A Markdown document, converted node for node (`Markdowns`).
  given markdown: (Markdown of Layout) is Presentable in Block = markdown =>
    Block.Group(Markdowns.convert(markdown))

trait Presentable2 extends Presentable3:
  // Below `table` in priority, so a list of products tabulates; above `derived`, so a list of
  // anything else lists rather than falling through to its `Showable`.
  given list: [element: Presentable as presentable] => List[element] is Presentable in Block =
    list =>
      val items = list.map: (element: element) =>
        Block.Item(Presentable.blocks(presentable.exhibit(element)))

      Block.Listing(false, items)

trait Presentable3:
  // The structural fallback applies to products only: a sum (a stdlib `List` or `Option`,
  // say, whose variants are not the point) falls through to its `Showable` or its `Inspectable`
  // rather than to a record of its variant. A sum exhibits by variant through an explicit
  // `derives Presentable`. `Inspectable` comes last because it always resolves: spectacular
  // derives one for any type, ending in a `toString` in curly quotes, so it is this fallback's
  // `toString` too.
  inline given derived: [value] => value is Presentable = scala.compiletime.summonFrom:
    case given (`value` is Showable) =>
      Presentable.phrase[value]: value => Inline.Textual(value.show)

    case given ProductReflection[`value`] => Presentable.Derivation.derived[value]

    case given (`value` is Inspectable) =>
      Presentable.phrase[value]: value => Inline.Textual(value.inspect)

trait Presentable extends Typeclass.Pure, Formal:
  type Form <: Inline | Block
  def exhibit(value: Self): Form
