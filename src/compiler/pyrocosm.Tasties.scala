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

import soundness.{Language as _, Token as _, *}

import hyperbole.Tasty

// An expression's typed AST, as hyperbole reifies it, as a tree: a node per node of the AST,
// labelled as code — its TASTy tag, its name in the accent of a term or a type and the role of
// a definition or a reference, its parameter marker and its type — and then the source it
// covers, subdued. Names and types are demangled by digression's rules. The root and its
// children start open, so the shape of the expression shows without the whole of it.
object Tasties:
  private val openDepth: Int = 1

  def tree(tree: Tasty.Tree): Block.Tree = Block.Tree(List(node(tree, 0)))

  def label(tree: Tasty.Tree): List[Inline] =
    val tag: Text = if tree.tag == ' ' then t"▪" else t"⟨${tree.tag.toString.tt}⟩"
    val accent: Token.Accent = if tree.term then Token.Accent.Term else Token.Accent.Typal
    val role: Token.Role = if tree.definitional then Token.Role.Binding else Token.Role.Usage

    val param: List[Token] = tree.param.lay(Nil: List[Token]): (param: Text) =>
      List(Token.plain(t" "), Token(param, Token.Accent.Modifier))

    val typed: List[Token] =
      if tree.typeName == t"" then Nil
      else
        List
          ( Token(t": ", Token.Accent.Symbol),
            Token(StackTrace.rewrite(tree.typeName.s), Token.Accent.Typal) )

    val tokens: List[Token] =
      List
        ( Token(tag, Token.Accent.Symbol),
          Token.plain(t" "),
          Token(StackTrace.rewrite(tree.name.s), accent, role) ) + param + typed

    val source: Text = tree.source.trim

    val covered: List[Inline] =
      if source == t"" then Nil
      else List(Inline.Textual(t" "), Inline.Toned(Tone.Muted, Inline.text(source)))

    Inline.Snippet(Language.Scala, tokens) :: covered

  private def node(tree: Tasty.Tree, depth: Int): Block.TreeNode =
    val children: List[Block.TreeNode] = tree.nodes.map(node(_, depth + 1))
    val open: Optional[Boolean] = if children.nil then Unset else depth <= openDepth
    Block.TreeNode(label(tree), children, Unset, Unset, open)
