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

import mandible.Bytecode

// A method's JVM bytecode, as mandible disassembles it, as a table: a row per instruction, with
// the source position it was compiled from, its offset, the opcode and the operand stack the
// verifier expects before it. The stack is the first column to give way when the table is
// squeezed. A class named in the stack is demangled by digression's rules, as mandible already
// demangles the classes and methods an opcode names.
object Bytecodes:
  val columns: List[Block.Column] =
    List
      ( Block.Column(Inline.text(t"Source"), Block.Alignment.End, Block.Sizing.Rigid),
        Block.Column(Inline.text(t"Offset"), Block.Alignment.End, Block.Sizing.Rigid, true),
        Block.Column(Inline.text(t"Opcode"), Block.Alignment.Start, Block.Sizing.Rigid),
        Block.Column(Inline.text(t"Stack"), Block.Alignment.Start, Block.Sizing.Collapsible(1.0)) )

  // A verification-type frame: a primitive by its descriptor letter, an array in brackets, a
  // class by its demangled name.
  def frame(frame: Bytecode.Frame): Text = frame match
    case Bytecode.Frame.L(name)      => StackTrace.rewrite(name.s)
    case Bytecode.Frame.Array(inner) => t"[${this.frame(inner)}]"
    case other                       => other.show

  // An opcode as a snippet: its mnemonic as a keyword, what follows as its operands.
  def opcode(opcode: Bytecode.Opcode): Inline =
    val text: Text = opcode.show

    val tokens: List[Token] =
      text.where(_ == ' ').lay(List(Token(text, Token.Accent.Keyword))): (space: Ordinal) =>
        List
          ( Token(text.keep(space.n0), Token.Accent.Keyword),
            Token.plain(t" "),
            Token(text.skip(space.n0 + 1), Token.Accent.Term) )

    Inline.Snippet(Language.Plain, tokens)

  // An instruction's row: `file` is the class's source file, which locates its line.
  def row(file: Text, instruction: Bytecode.Instruction): Block.Row =
    val source: List[Inline] = instruction.line.lay(Nil: List[Inline]): (line: Int) =>
      List(Inline.Toned(Tone.Muted, Inline.text(t"$file:$line")))

    val stack: List[Inline] =
      instruction.stack.lay(Nil: List[Inline]): (frames: List[Bytecode.Frame]) =>
        Inline.text(frames.reverse.map(frame).join(t" ┊ "))

    Block.Row
      ( List
          ( Block.Cell(source),
            Block.Cell(Inline.text(instruction.offset.show)),
            Block.Cell(List(opcode(instruction.opcode))),
            Block.Cell(stack) ) )

  def table(bytecode: Bytecode): Block.Table =
    val file: Text = bytecode.sourceFile.or(t"")
    Block.Table(columns, bytecode.instructions.map(row(file, _)))
