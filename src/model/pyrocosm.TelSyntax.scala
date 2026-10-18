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

// TEL as highlighted lines, by its lexical shape alone: a compound line is a keyword and its
// atoms, a comment is a line beginning with the sigil, and the pragma heads the document.
// Stratiform parses TEL but does not tokenise it for display, so this is written here, in the
// model, where a TEL document's excerpt is made for a diagnostic; it is a candidate for
// Soundness. It reads line by line and knows nothing of structure, so the payload of a source
// or literal atom (an indented block hosting another language) is highlighted as if it were
// compounds, which it resembles.
object TelSyntax:
  private val sigil: Char = '#'

  def lines(source: Text): List[Block.Line] =
    source.cut(t"\n").indexed.map { (line: Text, index: Ordinal) => Block.Line(tokens(line, index.n0 == 0)) }

  // One line's tokens. The pragma is the first line when it begins with `tel`; a document
  // separator is the sigil twice; a comment is the sigil after any indentation.
  def tokens(line: Text, first: Boolean = false): List[Token] =
    val text: String = line.s
    val indent: Int = text.takeWhile(_ == ' ').length
    val body: String = text.substring(indent).nn

    if body.isEmpty then Nil
    else if body == s"$sigil$sigil" then List(Token(line, Token.Accent.Symbol))
    else if body.charAt(0) == sigil then List(Token(line, Token.Accent.Unparsed))
    else
      val margin: List[Token] = if indent == 0 then Nil else List(Token.plain(t" "*indent))
      val words: List[Token] = atoms(body, first && body.startsWith("tel"))
      margin + words

  // The words of a compound: the keyword first, then its atoms, each a number or a term, with
  // the runs of spaces between them kept as plain tokens so the line's columns are unchanged.
  private def atoms(body: String, pragma: Boolean): List[Token] =
    var tokens: List[Token] = Nil
    var index: Int = 0
    var keyword: Boolean = true

    while index < body.length do
      val char = body.charAt(index)

      if char == ' ' then
        val start = index
        while index < body.length && body.charAt(index) == ' ' do index += 1
        tokens = Token.plain(body.substring(start, index).nn.tt) :: tokens
      else
        val start = index
        while index < body.length && body.charAt(index) != ' ' do index += 1
        val word: Text = body.substring(start, index).nn.tt
        val accent: Token.Accent =
          if keyword then Token.Accent.Keyword
          else if numeric(word) then Token.Accent.Number
          else if pragma then Token.Accent.Modifier
          else Token.Accent.Term

        tokens = Token(word, accent) :: tokens
        keyword = false

    tokens.reverse

  private def numeric(word: Text): Boolean =
    val text = word.s
    val digits = text.stripPrefix("-")
    digits.nonEmpty && digits.forall { (char: Char) => char.isDigit || char == '.' } && digits.exists(_.isDigit)
