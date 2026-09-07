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

import rudiments.*
import symbolism.*
import vacuous.*

// What a run of blocks can do: the actions it offers for selection, and whether anything in it
// animates. Both frontends recompute these from the live content on every paint, so they are
// always current, and a frontend validates an incoming action id against `of`.
object Actions:
  def of(blocks: List[Block]): List[Action] =
    blocks.bind { (block: Block) => ofBlock(block) }

  private def one(action: Optional[Action]): List[Action] = action.lay(Nil: List[Action])(List(_))

  private def ofTree(node: Block.TreeNode): List[Action] =
    one(node.action) + node.children.bind { (child: Block.TreeNode) => ofTree(child) }

  private def ofBlock(block: Block): List[Action] = block match
    case Block.Listing(_, items)         => items.bind { (item: Block.Item) => one(item.action) + of(item.content) }
    case Block.Table(_, rows, _)         => rows.bind { (row: Block.Row) => one(row.action) }
    case Block.Tree(roots)               => roots.bind { (root: Block.TreeNode) => ofTree(root) }
    case Block.Graph(vertices, _)        => vertices.bind { (vertex: Block.Vertex) => one(vertex.action) }
    case Block.Quotation(content)        => of(content)
    case Block.Notice(_, _, content)     => of(content)
    case Block.Disclosure(_, content, _) => of(content)
    case Block.Group(content)            => of(content)
    case Block.Record(entries, _)        => entries.bind { (entry: Block.Entry) => of(entry.value) }
    case _                               => Nil

  def animated(blocks: List[Block]): Boolean = blocks.exists { (block: Block) => animatedBlock(block) }

  private def animatedBlock(block: Block): Boolean = block match
    case Block.Gauge(Status.Indeterminate(), _)            => true
    case Block.Gauge(Status.Standing(Standing.Running), _) => true
    case Block.Gauge(Status.Steps(steps), _)               => steps.exists(_.standing == Standing.Running)
    case Block.Quotation(content)                          => animated(content)
    case Block.Notice(_, _, content)                       => animated(content)
    case Block.Disclosure(_, content, _)                   => animated(content)
    case Block.Group(content)                              => animated(content)
    case _                                                 => false
