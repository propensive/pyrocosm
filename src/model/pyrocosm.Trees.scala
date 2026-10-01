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

import breviloquence.*
import contingency.strategies.throwUnsafely
import dysasymptotics.linearAccess

// The tree-shaped data formats — JSON, YAML, CBOR, XML and TEL — as trees of collapsible
// nodes, one node per container or leaf, labelled as code in the format's own notation: a
// key and its scalar on a leaf, a key and a count on a container. The root and its children
// start open, so the shape of a document shows without the whole of it; anything deeper
// starts closed, for the reader to open.
object Trees:
  private val openDepth: Int = 1

  private def node
    ( language: Language,
      tokens:   List[Token],
      children: List[Block.TreeNode] = Nil,
      depth:    Int = 0,
      tone:     Optional[Tone] = Unset )
  :   Block.TreeNode =

    val open: Optional[Boolean] = if children.nil then Unset else depth <= openDepth
    Block.TreeNode(List(Inline.Code(language, tokens)), children, tone, Unset, open)

  private def term(text: Text): Token = Token(text, Token.Accent.Term)
  private def typal(text: Text): Token = Token(text, Token.Accent.Typal)
  private def string(text: Text): Token = Token(text, Token.Accent.String)
  private def number(text: Text): Token = Token(text, Token.Accent.Number)
  private def keyword(text: Text): Token = Token(text, Token.Accent.Keyword)
  private def symbol(text: Text): Token = Token(text, Token.Accent.Symbol)
  private def plain(text: Text): Token = Token.plain(text)

  private def count(n: Int, noun: Text, plural: Text): List[Token] =
    List(plain(if n == 1 then t" 1 $noun " else t" $n $plural "))

  private def indices(n: Int): List[Int] = List.from(0 until n)

  // ── JSON ──────────────────────────────────────────────────────────────────────────────

  def json(ast: Json.Ast): Block = Block.Tree(List(jsonNode(Nil, ast, 0)))

  // `prefix` is the key (`name: `) or index (`[0] `) which locates the value in its parent.
  private def jsonNode(prefix: List[Token], ast: Json.Ast, depth: Int): Block.TreeNode =
    if ast.isObject then
      val children = indices(ast.objectSize).map: (index: Int) =>
        val key: List[Token] = List(term(ast.objectKey(index).tt), symbol(t": "))
        jsonNode(key, ast.objectValue(index), depth + 1)

      val label = prefix + List(symbol(t"{")) + count(ast.objectSize, t"field", t"fields") + List(symbol(t"}"))
      node(Language.Json, label, children, depth)

    else if ast.isArray then
      val children = indices(ast.arrayLength).map: (index: Int) =>
        jsonNode(List(symbol(t"[$index] ")), ast.arrayElement(index), depth + 1)

      val label = prefix + List(symbol(t"[")) + count(ast.arrayLength, t"element", t"elements") + List(symbol(t"]"))
      node(Language.Json, label, children, depth)

    else
      node(Language.Json, prefix + List(jsonScalar(ast)), Nil, depth)

  private def jsonScalar(ast: Json.Ast): Token =
    if ast.isString then string(t"\"${ast.string}\"")
    else if ast.isBoolean then keyword(ast.boolean.toString.tt)
    else if ast.isNull then keyword(t"null")
    else if ast.isNumber then number(ast.number.toString.tt)
    else plain(t"")

  // ── YAML ──────────────────────────────────────────────────────────────────────────────

  def yaml(ast: Yaml.Ast): Block = Block.Tree(List(yamlNode(Nil, ast, 0)))

  private def yamlNode(prefix: List[Token], ast: Yaml.Ast, depth: Int): Block.TreeNode =
    if ast.isAbsent then
      node(Language.Yaml, prefix, Nil, depth)

    else if ast.isObject then
      val children = indices(ast.objectSize).map: (index: Int) =>
        yamlNode(List(term(ast.objectKey(index).tt), symbol(t": ")), ast.objectValue(index), depth + 1)

      node(Language.Yaml, prefix + count(ast.objectSize, t"key", t"keys"), children, depth)

    else if ast.isArray then
      val children = indices(ast.arrayLength).map: (index: Int) =>
        yamlNode(List(symbol(t"- ")), ast.arrayElement(index), depth + 1)

      node(Language.Yaml, prefix + count(ast.arrayLength, t"item", t"items"), children, depth)

    else
      node(Language.Yaml, prefix + List(yamlScalar(ast)), Nil, depth)

  private def yamlScalar(ast: Yaml.Ast): Token =
    if ast.isString then string(ast.string)
    else if ast.isBoolean then keyword(ast.boolean.toString.tt)
    else if ast.isNull then keyword(t"null")
    else if ast.isLong then number(ast.long.toString.tt)
    else if ast.isDouble then number(ast.double.toString.tt)
    else if ast.isBcd then number(ast.bcd.toString.tt)
    else plain(t"")

  // ── CBOR ──────────────────────────────────────────────────────────────────────────────

  def cbor(ast: Cbor.Ast): Block = Block.Tree(List(cborNode(Nil, ast, 0)))

  private def cborNode(prefix: List[Token], ast: Cbor.Ast, depth: Int): Block.TreeNode =
    if ast.isMap then
      val children = indices(ast.entries).map: (index: Int) =>
        cborNode(List(cborKey(ast.key(index)), symbol(t": ")), ast.value(index), depth + 1)

      val label = prefix + List(symbol(t"{")) + count(ast.entries, t"entry", t"entries") + List(symbol(t"}"))
      node(Language.Cbor, label, children, depth)

    else if ast.isArray then
      val children = indices(ast.elements).map: (index: Int) =>
        cborNode(List(symbol(t"[$index] ")), ast.element(index), depth + 1)

      val label = prefix + List(symbol(t"[")) + count(ast.elements, t"element", t"elements") + List(symbol(t"]"))
      node(Language.Cbor, label, children, depth)

    else if ast.isTag then
      val tag = ast.tag
      val label = prefix + List(keyword(t"tag"), symbol(t"("), number(tag.tag.toString.tt), symbol(t")"))
      val tagged: Cbor.Ast = Cbor.Ast(tag.value.asInstanceOf[Cbor.CborTypes])
      node(Language.Cbor, label, List(cborNode(Nil, tagged, depth + 1)), depth)

    else
      node(Language.Cbor, prefix + List(cborScalar(ast)), Nil, depth)

  // A map's key may be any value: a text string is shown as a key, another scalar as itself,
  // and anything else by its shape.
  private def cborKey(ast: Cbor.Ast): Token =
    if ast.isTextString then term(ast.string.tt)
    else if ast.isMap then symbol(t"{…}")
    else if ast.isArray then symbol(t"[…]")
    else cborScalar(ast)

  private def cborScalar(ast: Cbor.Ast): Token =
    if ast.isTextString then string(t"\"${ast.string}\"")
    else if ast.isInteger then number(ast.long.toString.tt)
    else if ast.isFloat then number(ast.double.toString.tt)
    else if ast.isBoolean then keyword(ast.boolean.toString.tt)
    else if ast.nullary then keyword(t"null")
    else if ast.unset then keyword(t"undefined")
    else if ast.isByteString then string(hex(ast.byteString))
    else plain(t"")

  // RFC 8949 diagnostic notation, `h'…'`, cut short after sixteen bytes.
  private def hex(bytes: Array[Byte]^{}): Text =
    val shown: List[Text] = indices(bytes.length.min(16)).map: (index: Int) =>
      val digits = java.lang.Integer.toHexString(bytes.readable(index) & 0xff).nn.tt
      if digits.length == 1 then t"0$digits" else digits

    t"h'${shown.join}${if bytes.length > 16 then t"…" else t""}'"

  // ── XML ───────────────────────────────────────────────────────────────────────────────

  def xml(xml: Xml): Block = xml match
    case xylophone.Fragment(nodes*) =>
      val roots: List[xylophone.Node] = List.from(nodes)
      Block.Tree(roots.filter(significant).map { (node: xylophone.Node) => xmlNode(node, 0) })
    case node: xylophone.Node       => Block.Tree(List(xmlNode(node, 0)))

  // Whitespace between elements is layout, not content.
  private def significant(node: xylophone.Node): Boolean = node match
    case xylophone.TextNode(text) => text.trim.length > 0
    case _                        => true

  private def xmlNode(node0: xylophone.Node, depth: Int): Block.TreeNode = node0 match
    case xylophone.Element(label, attributes, children0) =>
      val pairs: List[(Text, Text)] = List.from(attributes.iterator)

      val attributes2: List[Token] = pairs.bind: (pair: (Text, Text)) =>
        val (key, value) = pair
        List(plain(t" "), term(key), symbol(t"="), string(t"\"$value\""))

      val opening: List[Token] = List(symbol(t"<"), typal(label)) + attributes2
      val closing: List[Token] = List(symbol(t"</"), typal(label), symbol(t">"))
      val children0b: List[xylophone.Node] = List.from(children0.readable)
      val children: List[xylophone.Node] = children0b.filter(significant)

      children match
        case Nil => node(Language.Xml, opening + List(symbol(t"/>")), Nil, depth)

        case xylophone.TextNode(text) :: Nil =>
          node(Language.Xml, opening + List(symbol(t">"), string(text.trim)) + closing, Nil, depth)

        case _ =>
          node(Language.Xml, opening + List(symbol(t">")), children.map { (child: xylophone.Node) => xmlNode(child, depth + 1) }, depth)

    case xylophone.TextNode(text) => node(Language.Xml, List(string(text.trim)), Nil, depth)
    case xylophone.Cdata(text)    => node(Language.Xml, List(symbol(t"<![CDATA["), string(text), symbol(t"]]>")), Nil, depth)
    case xylophone.Comment(text)  => node(Language.Xml, List(plain(t"<!--$text-->")), Nil, depth, Tone.Muted)
    case xylophone.Doctype(text)  => node(Language.Xml, List(keyword(t"<!DOCTYPE "), plain(text), keyword(t">")), Nil, depth, Tone.Muted)

    case xylophone.ProcessingInstruction(target, data) =>
      node(Language.Xml, List(symbol(t"<?"), term(target), plain(t" $data"), symbol(t"?>")), Nil, depth, Tone.Muted)

    case header: xylophone.Header =>
      val encoding: Text = header.encoding.lay(t"") { (encoding: Text) => t" encoding=\"$encoding\"" }
      val declaration: Text = t" version=\"${header.version}\"$encoding"
      node(Language.Xml, List(symbol(t"<?xml"), plain(declaration), symbol(t"?>")), Nil, depth, Tone.Muted)

  // ── TEL ───────────────────────────────────────────────────────────────────────────────

  // A document's top-level compounds are the roots; a compound's label is its keyword and
  // atoms, its remark kept faint; a comment is a muted leaf before what it comments on.
  def tel(tel: Tel): Block =
    val roots: List[Tel.Compound] = List.from(tel.childCompounds.readable)
    Block.Tree(roots.map { (compound: Tel.Compound) => telCompound(compound, 0) })

  private def telCompound(compound: Tel.Compound, depth: Int): Block.TreeNode =
    val atoms0: List[Tel.Atom] = List.from(compound.atoms.readable)
    val atoms: List[Token] = atoms0.bind { (atom: Tel.Atom) => List(plain(t" "), telAtom(atom)) }
    val remark: List[Token] = compound.remark.lay(Nil: List[Token]) { (remark: Text) => List(plain(t"  # $remark")) }
    val blocks: List[Tel.Block] = List.from(compound.children.readable)
    val children: List[Block.TreeNode] = blocks.bind { (block: Tel.Block) => telBlock(block, depth + 1) }
    node(Language.Tel, List(term(compound.keyword)) + atoms + remark, children, depth)

  private def telBlock(block: Tel.Block, depth: Int): List[Block.TreeNode] =
    val comments0: List[Tel.Comment] = List.from(block.comments.readable)
    val compounds: List[Tel.Compound] = List.from(block.compounds.readable)
    val comments: List[Block.TreeNode] = comments0.map { (comment: Tel.Comment) => node(Language.Tel, List(plain(t"# ${comment.text}")), Nil, depth, Tone.Muted) }
    comments + compounds.map { (compound: Tel.Compound) => telCompound(compound, depth) }

  private def telAtom(atom: Tel.Atom): Token = atom match
    case Tel.Atom.Inline(text, _)     => plain(text)
    case Tel.Atom.Source(text)        => string(text)
    case Tel.Atom.Literal(_, text)    => string(text)
