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

// Excluded from the umbrella: `Language`, which would outrank this package's own definition,
// since a wildcard import beats a package member declared in another file.
import soundness.{Language as _, *}

// A Markdown document as blocks, node for node, the one conversion every tool shares: both
// emphasis strengths become the model's one emphasis; inline HTML is dropped, as the parser
// comments it out, and a paragraph of nothing else is no paragraph; an HTML comment block is
// for the author, not the reader; an inline image becomes a link to it; a fence's content
// loses the newline that closed its last line, which is not a line. A tool with more to say —
// a documentation tool that compiles its fences, links its code and anchors its headings —
// says it through `Hooks`, each of which is given the line the node is on, from one.
object Markdowns:
  trait Hooks:
    // A fenced code block: plain, under its own language name, by default.
    def fence(info: List[Text], content: Text, line: Int): List[Block] =
      List(Markdowns.code(info, content, line))

    def code(code: Text, line: Int): Inline = Inline.Snippet(Language.Plain, List(Token.plain(code)))

    def link(destination: Text, content: List[Inline], line: Int): Inline =
      Inline.Link(Inline.Destination.External(destination), content)

    def image(destination: Text, content: List[Inline], line: Int): Inline =
      Inline.Link(Inline.Destination.External(destination), content)

    def heading(level: Int, content: List[Inline], line: Int): Block = Block.Heading(level, content)

    // An HTML block other than a comment is shown as it is.
    def html(html: Text, line: Int): List[Block] =
      if html.trim.starts(t"<!--") && html.trim.ends(t"-->") then Nil
      else List(Block.Code(Language(t"html"), Markdowns.lines(html)))

  object Hooks:
    val none: Hooks = new Hooks {}

  def convert(markdown: Markdown of Layout, hooks: Hooks = Hooks.none): List[Block] =
    all(markdown.children, hooks)

  // A run of Markdown phrasing — a frame's label, say — as inlines: the first paragraph the
  // text parses to, or the text itself when it has none.
  def inline(text: Text, hooks: Hooks = Hooks.none): List[Inline] =
    def first(nodes: List[Layout]): List[Inline] = nodes match
      case Layout.Paragraph(_, prose*) :: _ => phrasing(List.from(prose), hooks, 1)
      case _ :: tail                        => first(tail)
      case _                                => Inline.text(text)

    first(Parser.parse(text).children)

  // A fence's content as plain lines, numbered from `line` when it is given.
  def code(info: List[Text], content: Text, line: Optional[Int] = Unset): Block =
    val language: Language = info.head.lay(Language.Plain): (name: Text) => Language(name)
    Block.Code(language, lines(content), Nil, line.let(_ + 1))

  def lines(content: Text): List[Block.Line] =
    trimmed(content).cut(t"\n").map: (line: Text) => Block.Line(List(Token.plain(line)))

  def trimmed(content: Text): Text =
    if content.ends(t"\n") then content.keep(content.length - 1) else content

  // A heading's anchor from its text, as GitHub makes one: lowercased, every character which
  // is not a letter, a digit, a space or a hyphen dropped, and each space a hyphen.
  def slug(text: Text): Text =
    val kept: String = text.s.toLowerCase.nn.filter: c => c.isLetterOrDigit || c == ' ' || c == '-'
    Text(kept.replace(' ', '-').nn)

  // The first heading's text, of whatever level, which is what a document is called.
  def title(blocks: List[Block]): Optional[Text] = blocks match
    case Block.Heading(_, content, _) :: _ => Inline.plain(content)
    case _ :: tail                         => title(tail)
    case _                                 => Unset

  def plain(inlines: List[Inline]): Text = Inline.plain(inlines)

  // A fenced block of a document: its info string, its content and its line, from one.
  case class Fence(info: List[Text], content: Text, line: Int)

  // Every fenced block of a document, in the order the conversion meets them: depth first,
  // through block quotes and list items.
  def fences(markdown: Markdown of Layout): List[Fence] = gathered(markdown.children)

  private def gathered(nodes: List[Layout]): List[Fence] = nodes match
    case head :: tail => within(head) + gathered(tail)
    case _            => Nil

  private def within(node: Layout): List[Fence] = node match
    case Layout.CodeBlock(line, info, content)  => List(Fence(info, content, line.n1))
    case Layout.BlockQuote(_, layouts*)         => gathered(List.from(layouts))
    case Layout.BulletList(_, _, items*)        => listed(List.from(items))
    case Layout.OrderedList(_, _, _, _, items*) => listed(List.from(items))
    case _                                      => Nil

  private def listed(items: List[List[Layout]]): List[Fence] = items match
    case head :: tail => gathered(head) + listed(tail)
    case _            => Nil

  // `blocks` with each block `replace` answers for replaced, through quotations, groups and
  // list items: how a placeholder a fence left is filled with the fence's output.
  def rewrite(blocks: List[Block])(replace: Block => Optional[List[Block]]): List[Block] =
    blocks match
      case head :: tail => rewritten(head, replace) + rewrite(tail)(replace)
      case _            => Nil

  private def rewritten(block: Block, replace: Block => Optional[List[Block]]): List[Block] =
    replace(block).or:
      block match
        case Block.Quotation(content) => List(Block.Quotation(rewrite(content)(replace)))
        case Block.Group(content)     => List(Block.Group(rewrite(content)(replace)))

        case Block.Listing(ordered, items) =>
          val refilled: List[Block.Item] = items.map: (item: Block.Item) =>
            item.copy(content = rewrite(item.content)(replace))

          List(Block.Listing(ordered, refilled))

        case other =>
          List(other)

  // The anchors of one page, each unique on it: a slug is itself when no earlier heading took
  // it, else numbered from one until free, as GitHub numbers repeated headings; an anchor a
  // fence chose for itself is noted, so that a link to it is known to land.
  final class Anchors:
    private var taken: List[Text] = Nil

    def anchor(slug: Text): Text =
      def free(candidate: Text, n: Int): Text =
        if !taken.has(candidate) then candidate else free(t"$slug-${n.show}", n + 1)

      val chosen: Text = free(if slug == t"" then t"section" else slug, 1)
      taken = chosen :: taken
      chosen

    def anchored(id: Text): Unit = if !taken.has(id) then taken = id :: taken
    def all: List[Text] = taken.reverse

  private def all(nodes: List[Layout], hooks: Hooks): List[Block] = nodes match
    case head :: tail => layout(head, hooks) + all(tail, hooks)
    case _            => Nil

  private def phrasing(nodes: List[Prose], hooks: Hooks, line: Int): List[Inline] =
    nodes.map(prose(_, hooks, line))

  private def prose(node: Prose, hooks: Hooks, line: Int): Inline = node match
    case Prose.Textual(text)       => Inline.Textual(text)
    case Prose.Emphasis(children*) => Inline.Emphasis(phrasing(List.from(children), hooks, line))
    case Prose.Strong(children*)   => Inline.Strong(phrasing(List.from(children), hooks, line))
    case Prose.Code(code)          => hooks.code(code, line)
    case Prose.Softbreak           => Inline.Textual(t" ")
    case Prose.Linebreak           => Inline.Break()
    case Prose.HtmlInline(_)       => Inline.Textual(t"")

    case Prose.Link(destination, _, content*) =>
      hooks.link(destination, phrasing(List.from(content), hooks, line), line)

    case Prose.Image(destination, _, content*) =>
      hooks.image(destination, phrasing(List.from(content), hooks, line), line)

  // Inline HTML (an HTML comment the parser read inline, say) is for the author, not the
  // reader: a paragraph of nothing else is no paragraph.
  private def hidden(node: Prose): Boolean = node match
    case Prose.HtmlInline(_) => true
    case Prose.Softbreak     => true
    case _                   => false

  private def layout(node: Layout, hooks: Hooks): List[Block] = node match
    case Layout.Paragraph(_, prose*) if prose.nonEmpty && prose.forall(hidden) =>
      Nil

    case Layout.Paragraph(line, prose*) =>
      List(Block.Paragraph(phrasing(List.from(prose), hooks, line.n1)))

    case Layout.Heading(line, level, prose*) =>
      List(hooks.heading(level, phrasing(List.from(prose), hooks, line.n1), line.n1))

    case Layout.BlockQuote(_, layouts*)       => List(Block.Quotation(all(List.from(layouts), hooks)))
    case Layout.ThematicBreak(_)              => List(Block.Rule())
    case Layout.HtmlBlock(line, html)         => hooks.html(html, line.n1)
    case Layout.CodeBlock(line, info, content) => hooks.fence(info, content, line.n1)

    case Layout.BulletList(_, _, items*) =>
      val listed: List[Block.Item] =
        List.from(items).map: (item: List[Layout]) => Block.Item(all(item, hooks))

      List(Block.Listing(false, listed))

    case Layout.OrderedList(_, _, _, _, items*) =>
      val listed: List[Block.Item] =
        List.from(items).map: (item: List[Layout]) => Block.Item(all(item, hooks))

      List(Block.Listing(true, listed))
