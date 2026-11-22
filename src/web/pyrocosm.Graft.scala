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

import soundness.{Control as _, *}

import dysasymptotics.{linearAccess, linearSize}

// A panel whose trees alone have changed is sent as the nodes of them which changed, not as the
// whole of its content: a tree of thousands of nodes, of which the page shown marks one and
// opens the way to it, changes in a handful of them from one page to the next, and an
// application which draws it from the nodes it drew before (as it must, to draw it quickly)
// says which by their identity.
object Graft:
  // More nodes than this are sent as the content they are in.
  val limit: Int = 64

  // What turns `before` into `after`, if only nodes of the trees at their top differ, and few
  // enough: nothing, if they are otherwise different, and the whole content must be sent.
  def between(before: List[Block], after: List[Block], renderer: HtmlRenderer)
  :   Optional[List[Graft]] =

    def recur(todo: List[(Block, Block)], tree: Int, done: List[Graft]): Optional[List[Graft]] =
      todo match
        case Nil =>
          if done.size > limit then Unset else done

        case (Block.Tree(old), Block.Tree(next)) :: tail =>
          if old.size != next.size then Unset
          else recur(tail, tree + 1, done + nodes(old, next, tree, t"", renderer))

        case (old, next) :: tail =>
          if old == next then recur(tail, tree, done) else Unset

    if before.size != after.size then Unset else recur(before.zip(after), 0, Nil)

  // The grafts of the nodes at one level, each at its index beneath `path`.
  private def nodes
    ( before:   List[Block.TreeNode],
      after:    List[Block.TreeNode],
      tree:     Int,
      path:     Text,
      renderer: HtmlRenderer )
  :   List[Graft] =

    before.zip(after).indexed.bind: (pair: (Block.TreeNode, Block.TreeNode), index: Ordinal) =>
      val here: Text = if path == t"" then t"${index.n0}" else t"$path.${index.n0}"
      node(pair(0), pair(1), tree, here, renderer)

  // A node which is the very one drawn before has not changed, nor has anything beneath it; one
  // with as many children is itself shown again if it differs, and its children are looked at
  // in turn; any other is shown again whole.
  private def node
    ( before:   Block.TreeNode,
      after:    Block.TreeNode,
      tree:     Int,
      path:     Text,
      renderer: HtmlRenderer )
  :   List[Graft] =

    if before eq after then Nil
    else if before.children.size != after.children.size then
      List(Graft(tree, path, renderer.treeNode(after, path).show, true))
    else
      val same: Boolean =
        before.label == after.label && before.tone == after.tone && before.open == after.open &&
          before.action == after.action && before.destination == after.destination

      val own: List[Graft] =
        if same then Nil
        else List(Graft(tree, path, renderer.treeNode(after, path, true).show, false))

      own + nodes(before.children, after.children, tree, path, renderer)

// One node of a tree shown again, where the page has it: the tree it is of, by its place among
// those at the top of its panel; the node, by its path within the tree, as `data-node` has it;
// and its markup. A graft which is not `whole` is the node alone — its label, its tone and
// whether it is open — whose children are those the page already shows beneath it, which the
// script moves into the new node; a whole one is the node and everything beneath it.
case class Graft(tree: Int, path: Text, html: Text, whole: Boolean)
