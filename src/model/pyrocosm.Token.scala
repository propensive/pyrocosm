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
import vacuous.*

// Syntax highlighting is done by the compiler (harlequin), which only exists on the JVM, so the
// model carries the *result* of highlighting: tokens with their accents, exactly harlequin's
// vocabulary, but defined here so that the model stays platform-neutral. A `Presentable`
// instance for harlequin's `SourceCode` converts in a JVM-only module.
object Token:
  // An accent is a colour category and nothing more; one palette maps each to one colour.
  enum Accent:
    case Error, Number, String, Term, Typal, Keyword, Symbol, Parens, Modifier, Unparsed

  // Whether a term or type token is a definition or a use of one; a styling policy may set
  // bindings in italic, say, on top of the accent's colour.
  enum Role:
    case Binding, Usage

  def plain(text: Text): Token = Token(text, Accent.Unparsed)

case class Token(text: Text, accent: Token.Accent, role: Optional[Token.Role] = Unset)

// The language of a piece of code, which decides how it was tokenised and how it is labelled.
// Open, so that an application can name a language the model has never heard of.
case class Language(name: Text)

object Language:
  val Scala: Language = Language("scala")
  val Java: Language = Language("java")
  val Json: Language = Language("json")
  val Markdown: Language = Language("markdown")
  val Shell: Language = Language("shell")
  val Plain: Language = Language("plain")
