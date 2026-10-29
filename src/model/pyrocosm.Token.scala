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

// Syntax highlighting is done by the compiler (harlequin), which only exists on the JVM, so the
// model carries the *result* of highlighting: tokens with their accents, exactly harlequin's
// vocabulary, but defined here so that the model stays platform-neutral. A `Presentable`
// instance for harlequin's `SourceCode` converts in a JVM-only module.
object Token:
  // An accent is a colour category and nothing more; one palette maps each to one colour.
  enum Accent:
    case Error, Number, String, Term, Typal, Keyword, Symbol, Parens, Modifier, Unparsed, Command

  // Whether a term or type token is a definition or a use of one; a styling policy may set
  // bindings in italic, say, on top of the accent's colour.
  enum Role:
    case Binding, Usage

  def plain(text: Text): Token = Token(text, Accent.Unparsed)

  // The one edit that turns `before` into `after`: the longest common prefix and suffix are
  // kept, and what lies between them is the splice. One keystroke, paste or recall is one.
  def splice(before: Text, after: Text): Control.Field.Splice =
    val a: String = before.s
    val b: String = after.s
    val shortest: Int = a.length.min(b.length)
    var prefix: Int = 0
    while prefix < shortest && a.charAt(prefix) == b.charAt(prefix) do prefix += 1
    var suffix: Int = 0

    while suffix < shortest - prefix &&
      a.charAt(a.length - 1 - suffix) == b.charAt(b.length - 1 - suffix)
    do suffix += 1

    val replacement: Text = b.substring(prefix, b.length - suffix).nn.tt
    Control.Field.Splice(prefix, a.length - prefix - suffix, replacement)

  private def wordish(char: Char): Boolean = char.isLetterOrDigit || char == '_'
  private def wordy(text: Text): Boolean = text.length > 0 && text.all(wordish(_))

  // Whether a token of the accent grows when identifier characters are typed inside or at the
  // end of it: a term, a type, a number or a keyword does; a symbol, a paren or a string's
  // quote does not.
  private def extendable(accent: Accent): Boolean = accent match
    case Accent.Term | Accent.Typal | Accent.Number | Accent.Keyword | Accent.Unparsed => true
    case _                                                                             => false

  // Whether anything at all typed inside the token belongs to it: the inside of a string, and
  // unparsed text, are coloured as they are whatever is typed there.
  private def absorbent(accent: Accent): Boolean = accent match
    case Accent.String | Accent.Unparsed | Accent.Error => true
    case _                                              => false

  // The tokens of a text, as they stand after one edit of it: what a frontend paints while the
  // highlighting of the edited text is awaited, so a keystroke never shows the line plain.
  // The removed range is cut from the tokens it covers (a token wholly removed goes), and the
  // inserted text joins the token it lands in or ends — identifier characters extend a term,
  // a type, a number or a keyword; anything extends the inside of a string — or else stands as
  // an unparsed token of its own, to be coloured when the highlighting arrives. The result
  // always spells out the edited text exactly.
  def rebase(tokens: List[Token], splice: Control.Field.Splice): List[Token] =
    val start: Int = splice.start
    val end: Int = start + splice.removed

    // The removal: each token keeps whatever of it lies outside the range.
    val cut: List[Token] =
      var offset: Int = 0

      tokens.bind: (token: Token) =>
        val a: Int = offset
        val b: Int = a + token.text.length
        offset = b

        if b <= start || a >= end then List(token) else
          val kept: String =
            token.text.s.substring(0, (start - a).max(0)).nn +
              token.text.s.substring((end - a).min(token.text.length)).nn

          if kept.isEmpty then Nil else List(token.copy(text = kept.tt))

    if splice.inserted == t"" then cut else
      val inserted: Text = splice.inserted
      val word: Boolean = wordy(inserted)

      def within(token: Token): Boolean =
        absorbent(token.accent) || (word && extendable(token.accent))

      def grows(token: Token): Boolean =
        word && extendable(token.accent) && token.text.length > 0 &&
          wordish(token.text.s.charAt(token.text.length - 1))

      def precedes(token: Token): Boolean =
        word && extendable(token.accent) && token.text.length > 0 && wordish(token.text.s.charAt(0))

      // The tokens before the insertion point, reversed, and those from it onwards.
      def place(before: List[Token], after: List[Token], offset: Int): List[Token] = after match
        case token :: rest =>
          val a: Int = offset
          val b: Int = a + token.text.length

          if start == a then
            // At a boundary: the token before grows if it can, else this one takes the text at
            // its head if it can, else the text stands between them.
            before match
              case last :: others if grows(last) =>
                (last.copy(text = t"${last.text}$inserted") :: others).reverse + after

              case _ =>
                if precedes(token)
                then before.reverse + (token.copy(text = t"$inserted${token.text}") :: rest)
                else before.reverse + (plain(inserted) :: after)
          else if start < b then
            // Strictly inside the token: it absorbs the text, or is split around it.
            val head: String = token.text.s.substring(0, start - a).nn
            val tail: String = token.text.s.substring(start - a).nn

            def split: List[Token] =
              token.copy(text = head.tt) :: plain(inserted) :: token.copy(text = tail.tt) :: rest

            if within(token)
            then before.reverse + (token.copy(text = t"$head$inserted$tail") :: rest)
            else before.reverse + split
          else
            place(token :: before, rest, b)

        case _ =>
          // At the very end: the last token grows, or a new one follows it.
          before match
            case last :: rest if grows(last) =>
              (last.copy(text = t"${last.text}$inserted") :: rest).reverse

            case _ => (plain(inserted) :: before).reverse

      place(Nil, cut, 0)

// A token may link, as an identifier in a signature links to the page which documents it; a
// renderer without links shows it as any other token. A `chained` token continues the
// qualified name the tokens before it began — `.` and `example` and `.` and `Foo`, after
// `com` — so that a renderer can show each segment as a link of its own and the name up to it
// as one thing, as `com.example` is when `example` is pointed at.
case class Token
  ( text:    Text,
    accent:  Token.Accent,
    role:    Optional[Token.Role]         = Unset,
    link:    Optional[Inline.Destination] = Unset,
    chained: Boolean                      = false )

object Language:
  val Scala: Language = Language("scala")
  val Java: Language = Language("java")
  val Json: Language = Language("json")
  val Xml: Language = Language("xml")
  val Tel: Language = Language("tel")
  val Cbor: Language = Language("cbor")
  val Yaml: Language = Language("yaml")
  val Markdown: Language = Language("markdown")
  val Shell: Language = Language("shell")
  val Plain: Language = Language("plain")

// The language of a piece of code, which decides how it was tokenised and how it is labelled.
// Open, so that an application can name a language the model has never heard of.
case class Language(name: Text)
