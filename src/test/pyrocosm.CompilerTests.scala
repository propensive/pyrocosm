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

import dysasymptotics.{linearAccess, linearSize}

import hyperbole.Tasty
import mandible.*

// A class the bytecode tests read back from the test's own classpath.
class Sample:
  def add(a: Int, b: Int): Int = a + b

// The exhibitions of the JVM's artefacts: a TASTy tree as a tree, a method's bytecode as a
// table, each with its names demangled.
object CompilerTests extends Suite(m"Pyrocosm compiler tests"):
  def run(): Unit =
    suite(m"Tasties"):
      val leaf: Tasty.Tree =
        Tasty.Tree('I', t"scala.Int", t"x", t"x", t"x", Nil, Unset, true, false)

      val root: Tasty.Tree =
        Tasty.Tree('A', t"scala.Int", t"$$plus", t"x + 1", t"  x + 1", List(leaf), t"p", true, true)

      test(m"a definition's label shows its tag, decoded name, parameter, type and source"):
        Inline.plain(Tasties.label(root))

      . assert(_ == t"⟨A⟩ + p: scala.Int x + 1")

      test(m"a reference's label shows its tag, name, type and source"):
        Inline.plain(Tasties.label(leaf))

      . assert(_ == t"⟨I⟩ x: scala.Int x")

      test(m"a tree has a node per node of the AST, the root open"):
        Tasties.tree(root).roots.map { (node: Block.TreeNode) => (node.children.size, node.open) }

      . assert(_ == List((1, true)))

      test(m"a leaf is not collapsible"):
        Tasties.tree(root).roots.bind(_.children).map(_.open)

      . assert(_ == List(Unset))

    suite(m"Bytecodes"):
      given Classloader = Classloader[Sample]

      val table: Optional[Block.Table] =
        Classfile[Sample].let: classfile =>
          classfile.methods.seek(_.name == t"add").let(_.bytecode).let(Bytecodes.table(_))

      test(m"a method's bytecode has a row per instruction, each of four cells"):
        table.let { table => !table.rows.nil && table.rows.all(_.cells.size == 4) }.or(false)

      . assert(_ == true)

      // The last instruction's opcode cell: its one inline, the opcode's snippet.
      val opcode: Optional[Inline] =
        table.let(_.rows.last).let(_.cells.at(Ter)).let(_.content.at(Prim))

      test(m"the last instruction returns an int"):
        opcode.let { (inline: Inline) => Inline.plain(List(inline)) }

      . assert(_ == t"i·return")

      test(m"an opcode's mnemonic is a keyword"):
        opcode.let:
          case Inline.Snippet(_, token :: _) => token.accent
          case _                             => Unset

      . assert(_ == Token.Accent.Keyword)

      test(m"a class in the stack is demangled"):
        Bytecodes.frame(Bytecode.Frame.L(t"scala.Predef$$"))

      . assert(_ == t"scala.ΞPredef")

      test(m"an array frame is bracketed"):
        Bytecodes.frame(Bytecode.Frame.Array(Bytecode.Frame.I))

      . assert(_ == t"[I]")
