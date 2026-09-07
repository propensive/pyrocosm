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
import clavichord.*
import gossamer.*
import quantitative.*
import rudiments.*
import spectacular.*
import stratiform.*
import vacuous.*

// Phrasing content: the things which occur *within* a line of text. Comparable to Markdown's
// inline nodes, with a single kind of emphasis, and richer in the ways an application needs —
// keystrokes, highlighted code, mathematics, quantities, references and semantic symbols — so
// that a renderer can give each the treatment its medium affords.
object Inline:
  // Where a link leads: out to a URL, or inward to an action the application handles.
  enum Destination:
    case External(url: Text)
    case Internal(action: Action)

  // Plain text as a one-element phrase, the commonest case.
  def text(text: Text): List[Inline] = List(Textual(text))

  object Amount:
    // A quantity's units exist only at the type level, so they are captured as text when the
    // phrase is made; that is what lets an amount travel over the wire and still render with
    // the right units and scaling.
    inline def of[units <: Measure](quantity: Quantity[units]): Amount =
      Amount(quantity.value, Quantity.units[units])

  // The unstyled text of a phrase, for a plain renderer, a title attribute, or a width estimate.
  def plain(content: List[Inline]): Text = content.map { (inline: Inline) => inline.plain }.join

  // The TEL codecs, anchored here so that they resolve from any package. They are derived in
  // `Codecs`, a sibling object, because a derived decoder fails to typecheck under capture
  // checking for an enum with a singleton case when an `Encodable` given for the same type is in
  // lexical scope; the companion holds only aliases.
  given telEncodable: Inline is Tel.Encodable = Codecs.inlineEncodable
  given telDecodable: Inline is Tel.Decodable = Codecs.inlineDecodable

enum Inline:
  case Textual(text: Text)
  case Phrase(content: List[Inline])                // a run of phrasing which belongs together
  case Emphasis(content: List[Inline])
  case Toned(tone: Tone, content: List[Inline])
  case Code(language: Language, tokens: List[Token])
  case Keystroke(keypress: Keypress)
  case Link(destination: Inline.Destination, content: List[Inline])
  case Math(math: archimedes.Math)
  case Symbol(glyph: Glyph)
  case Reference(id: Text)                          // an identifier or hash, e.g. a test id
  case Amount(value: Double, units: Text)           // a quantity in base units; the renderer scales it
  case Figure(value: Double, precision: Optional[Int] = Unset)

  // A fieldless product, not a singleton: deriving the TEL decoder for this enum with a singleton
  // case fails under capture checking (see the `Min` reproduction in the Soundness issue). Revert
  // to `case Break` when that is fixed.
  case Break()

  def plain: Text = this match
    case Textual(text)        => text
    case Phrase(content)      => Inline.plain(content)
    case Emphasis(content)    => Inline.plain(content)
    case Toned(_, content)    => Inline.plain(content)
    case Code(_, tokens)      => tokens.map { (token: Token) => token.text }.join
    case Keystroke(keypress)  => keypress.show
    case Link(_, content)     => Inline.plain(content)
    case Math(_)              => "…"
    case Symbol(glyph)        => glyph.toString.tt
    case Reference(id)        => id
    case Amount(value, units) => t"${value.toString} $units"
    case Figure(value, _)     => value.toString.tt
    case Break()              => "\n"
