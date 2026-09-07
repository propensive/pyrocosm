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

import anticipation.*
import delicious.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

// Named imports only: a wildcard import of harlequin would outrank this package's own `Token`
// (a wildcard import beats a same-package definition from another file).
import harlequin.{Java, ProgrammingLanguage, Scala, SourceCode}

// The exhibitions which need the compiler, and so exist only on the JVM: highlighted source from
// harlequin, and the semantic markup the compiler embeds in its diagnostics, parsed by
// delicious. The model carries the *results* of highlighting as its own `Token's, so these
// instances are the bridge from harlequin's vocabulary to the model's.
//
// Their subjects and the typeclass both live elsewhere, so the givens cannot sit in a
// companion; they are named, at package level, for import by name.

// harlequin's accents and roles are the model's, case for case.
extension (token: harlequin.Token)
  def model: Token =
    val accent = Token.Accent.valueOf(token.accent.toString)
    val role: Optional[Token.Role] = token.role.let { (role: harlequin.Role) => Token.Role.valueOf(role.toString) }
    Token(token.text, accent, role)

extension (code: SourceCode)
  def modelLines: List[Block.Line] =
    List.from(code.lines.readable).map { (line: List[harlequin.Token]) => Block.Line(line.map(_.model)) }

  // The model's language for the language that highlighted the source.
  def modelLanguage: Language = code.language match
    case Scala => Language.Scala
    case Java  => Language.Java

  // The tokens of every line, joined by newline tokens, for phrasing use.
  def flattened: List[Token] =
    val lines: scala.List[List[Token]] = code.modelLines.stdlib.map(_.tokens)

    lines.zipWithIndex.flatMap { (line, index) =>
      (if index == 0 then scala.Nil else scala.List(Token.plain("\n"))) ++ line.stdlib
    } .to(List)

given sourceCodePresentable: SourceCode is Presentable in Block = code =>
  Block.Code(code.modelLanguage, code.modelLines)

// A diagnostic's markup, as phrasing: code samples and types are highlighted as Scala terms and
// types; symbols and names appear as code; anything else is its text.
given markupPresentable: List[Markup] is Presentable in Inline = markup =>
  Inline.Phrase(markup.map(markupInline))

private def highlighted(text: Text, context: Scala.Context): Inline =
  Inline.Code(Language.Scala, Scala.highlight(text, context).flattened)

private def markupInline(markup: Markup): Inline = markup match
  case Markup.Textual(text)               => Inline.Textual(text)
  case Markup.Code(_, _)                  => highlighted(markup.plain, Scala.Context.Term)
  case Markup.Typed(_, _, _, _)           => highlighted(markup.plain, Scala.Context.Type)
  case Markup.Symbolic(_, _, _, _)        => highlighted(markup.plain, Scala.Context.Term)
  case Markup.Named(isType, _, _)         =>
    highlighted(markup.plain, if isType then Scala.Context.Type else Scala.Context.Term)
  case Markup.Spanned(_, _, children)     => Inline.Phrase(children.map(markupInline))
