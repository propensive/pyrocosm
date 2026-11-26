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

import soundness.*

import dysasymptotics.linearSize

// What a run of blocks can do: the actions it offers for selection, and whether anything in it
// animates. Both frontends recompute these from the live content on every paint, so they are
// always current, and a frontend validates an incoming action id against `of`.
object Actions:
  def of(blocks: List[Block]): List[Action] =
    blocks.bind: (block: Block) => ofBlock(block)

  private def one(action: Optional[Action]): List[Action] = action.lay(Nil: List[Action])(List(_))

  private def ofTree(node: Block.TreeNode): List[Action] =
    one(node.action) + node.children.bind: (child: Block.TreeNode) => ofTree(child)

  private def ofBlock(block: Block): List[Action] = block match
    case Block.Listing(_, items) =>
      items.bind: (item: Block.Item) => one(item.action) + of(item.content)

    case Block.Table(_, rows, _) => rows.bind: (row: Block.Row) => one(row.action)
    case Block.Tree(roots)       => roots.bind: (root: Block.TreeNode) => ofTree(root)

    case Block.Graph(vertices, _) =>
      vertices.bind: (vertex: Block.Vertex) => one(vertex.action)

    case Block.Quotation(content)        => of(content)
    case Block.Notice(_, _, content)     => of(content)
    case Block.Disclosure(_, content, _) => of(content)
    case Block.Group(content)            => of(content)
    case Block.Deck(leaves, _)           => leaves.bind: (leaf: Block.Leaf) => of(leaf.content)
    case Block.Record(entries, _)        => entries.bind: (entry: Block.Entry) => of(entry.value)
    case _                               => Nil

  // The actions the links in a run of blocks lead to: every internal destination of a link in
  // its phrasing, and of an item or a tree's node which leads rather than acts. They are not
  // among `of`, since a link is followed where it is read rather than selected in turn, but
  // following one is a press all the same, so a frontend which lets a link be followed
  // validates the press against these as well.
  def linked(blocks: List[Block]): List[Action] =
    blocks.bind: (block: Block) => linkedBlock(block)

  def linked(destination: Optional[Inline.Destination]): List[Action] = destination match
    case Inline.Destination.Internal(action) => List(action)
    case _                                   => Nil

  private def linkedPhrase(content: List[Inline]): List[Action] =
    content.bind: (inline: Inline) => linkedInline(inline)

  private def linkedInline(inline: Inline): List[Action] = inline match
    case Inline.Link(destination, content) => linked(destination) + linkedPhrase(content)
    case Inline.Phrase(content)            => linkedPhrase(content)
    case Inline.Emphasis(content)          => linkedPhrase(content)
    case Inline.Strong(content)            => linkedPhrase(content)
    case Inline.Toned(_, content)          => linkedPhrase(content)
    case Inline.Annotated(content, note)   => linkedPhrase(content) + linkedPhrase(note)
    case _                                 => Nil

  private def linkedTree(node: Block.TreeNode): List[Action] =
    linked(node.destination) + linkedPhrase(node.label) +
      node.children.bind: (child: Block.TreeNode) => linkedTree(child)

  private def linkedBlock(block: Block): List[Action] = block match
    case Block.Paragraph(content)     => linkedPhrase(content)
    case Block.Heading(_, content, _) => linkedPhrase(content)

    case Block.Listing(_, items) =>
      items.bind: (item: Block.Item) => linked(item.destination) + linked(item.content)

    case Block.Table(columns, rows, caption) =>
      val titles: List[Action] = columns.bind: (column: Block.Column) => linkedPhrase(column.title)

      val cells: List[Action] = rows.bind: (row: Block.Row) =>
        row.cells.bind { (cell: Block.Cell) => linkedPhrase(cell.content) } + linkedPhrase(row.note)

      caption.lay(Nil: List[Action])(linkedPhrase(_)) + titles + cells

    case Block.Record(entries, title) =>
      val keyed: List[Action] = entries.bind: (entry: Block.Entry) =>
        linkedPhrase(entry.key) + linked(entry.value)

      title.lay(Nil: List[Action])(linkedPhrase(_)) + keyed

    case Block.Notice(_, title, content) =>
      title.lay(Nil: List[Action])(linkedPhrase(_)) + linked(content)

    case Block.Disclosure(summary, content, _) => linkedPhrase(summary) + linked(content)
    case Block.Quotation(content)              => linked(content)
    case Block.Group(content)                  => linked(content)
    case Block.Gauge(_, caption)               => caption.lay(Nil: List[Action])(linkedPhrase(_))
    case Block.Tree(roots)                     => roots.bind: (root: Block.TreeNode) => linkedTree(root)

    case Block.Deck(leaves, _) =>
      leaves.bind: (leaf: Block.Leaf) => linked(leaf.content)

    case Block.Graph(vertices, _) =>
      vertices.bind: (vertex: Block.Vertex) => linkedPhrase(vertex.label)

    case Block.Chart(_, series) =>
      series.bind: (series: Block.Series) => linkedPhrase(series.label)

    case Block.Diagnostic(_, _, message, excerpt, detail, _) =>
      linked(message) + linked(excerpt) + linked(detail)

    case _ => Nil

  def animated(blocks: List[Block]): Boolean = blocks.exists: (block: Block) => animatedBlock(block)

  // How many leading blocks are settled: every block before the first that animates. A
  // transcript's settled entries are history, which an inline frontend commits to the
  // scrollback.
  def settled(blocks: List[Block]): Int =
    blocks.where { (block: Block) => animatedBlock(block) }.lay(blocks.size): (index: Ordinal) =>
      index.n0

  private def animatedBlock(block: Block): Boolean = block match
    case Block.Gauge(Status.Indeterminate(), _)            => true
    case Block.Gauge(Status.Standing(Standing.Running), _) => true

    case Block.Gauge(Status.Steps(steps), _) =>
      steps.exists(_.standing == Standing.Running)

    case Block.Quotation(content)        => animated(content)
    case Block.Notice(_, _, content)     => animated(content)
    case Block.Disclosure(_, content, _) => animated(content)
    case Block.Group(content)            => animated(content)
    case _                               => false
