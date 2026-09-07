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

import scala.caps

import anticipation.*
import archimedes.*
import clavichord.*
import contingency.*
import distillate.*
import gossamer.*
import prepositional.*
import spectacular.*
import stratiform.*
import vacuous.*

// TEL codecs for the two leaf types with no serial form of their own. Each is a scalar: a
// keypress is its rendering (`[⌃]+[C]`), and a mathematical expression is its Ergo shorthand.
// Named, at package level, because their subjects are owned elsewhere; the anchored `Inline`
// and `Block` codecs in the companions resolve them lexically, so a consumer never needs to.

given keypressTelEncodable: Keypress is Tel.Encodable =
  Tel.Encodable(() => Morphology.Str, Tel.Nature.Scalar): keypress =>
    Tel.scalar(keypress.show)

// The decoders are vouched pure, as flame anchors its wire codecs: a derived decoder demands pure
// field instances, and one that raises `Tel.Error` through a captured tactic is not. Each is
// minted under `unsafely`, so a malformed scalar throws rather than raises; every Pyrocosm
// decode site wraps the read in `safely`, which catches it.
given keypressTelDecodable: Keypress is Tel.Decodable =
  unsafely:
    caps.unsafe.unsafeAssumePure:
      Tel.Decodable(() => Morphology.Str, Tel.Nature.Scalar): tel =>
        val text = tel.primaryAtom
        Keypresses.parse(text).or(abort(Tel.Error(Tel.Error.Reason.NotScalar(text, "keypress"))))

// An expression outside the Ergo subset (an `<mtext>`, say) has no shorthand and encodes as
// nothing rather than failing the whole document; the MathML tree survives in memory, only its
// wire form is lossy.
given mathTelEncodable: archimedes.Math is Tel.Encodable =
  Tel.Encodable(() => Morphology.Str, Tel.Nature.Scalar): math =>
    Tel.scalar(safely(Ergo.serialize(math)).or(""))

given mathTelDecodable: archimedes.Math is Tel.Decodable =
  unsafely:
    caps.unsafe.unsafeAssumePure:
      Tel.Decodable(() => Morphology.Str, Tel.Nature.Scalar): tel =>
        val text = tel.primaryAtom
        safely(Ergo.parse(text)).or(abort(Tel.Error(Tel.Error.Reason.NotScalar(text, "ergo"))))

// The primitives, laundered the same way (their declared types name their tactic capture).
given textTelDecodable: Text is Tel.Decodable = unsafely(caps.unsafe.unsafeAssumePure(Tel.textDecodable))
given intTelDecodable: Int is Tel.Decodable = unsafely(caps.unsafe.unsafeAssumePure(Tel.intDecodable))
given longTelDecodable: Long is Tel.Decodable = unsafely(caps.unsafe.unsafeAssumePure(Tel.longDecodable))
given doubleTelDecodable: Double is Tel.Decodable = unsafely(caps.unsafe.unsafeAssumePure(Tel.doubleDecodable))
given booleanTelDecodable: Boolean is Tel.Decodable = unsafely(caps.unsafe.unsafeAssumePure(Tel.booleanDecodable))

// The derived codecs for the model types, aliased by their companions (see the note in
// `Inline`). The leaf codecs above are found lexically from here, and the recursion through
// `List[Inline]` and `List[Block]` is resolved by the derivation itself.
object Codecs:
  import contingency.strategies.throwUnsafely

  val inlineEncodable: Inline is Tel.Encodable = Tel.EncodableDerivation.derived[Inline]
  val inlineDecodable: Inline is Tel.Decodable = Tel.DecodableDerivation.derived[Inline]
  val blockEncodable: Block is Tel.Encodable = Tel.EncodableDerivation.derived[Block]
  val blockDecodable: Block is Tel.Decodable = Tel.DecodableDerivation.derived[Block]

// Every enum of singleton cases reachable from a derived codec has a scalar codec here, keyed by
// its kebab-cased case name (`arrow-right`), for two reasons: it is better TEL than a nested
// select, and deriving a decoder for a sum with a singleton case fails under capture checking
// in an order-sensitive way (the same Soundness issue as `Inline.Break`).
private def scalar[value](encode: value -> Text): value is Tel.Encodable =
  Tel.Encodable(() => Morphology.Str, Tel.Nature.Scalar) { value => Tel.scalar(encode(value)) }

private def parsed[value](expected: Text)(decode: Text -> Optional[value]): value is Tel.Decodable =
  unsafely:
    caps.unsafe.unsafeAssumePure:
      Tel.Decodable(() => Morphology.Str, Tel.Nature.Scalar): tel =>
        val text = tel.primaryAtom
        decode(text).or(abort(Tel.Error(Tel.Error.Reason.NotScalar(text, expected))))

// `ArrowRight` to `arrow-right`, as stratiform keys variants.
private def kebab(value: Any): Text =
  val name = value.toString
  val builder = StringBuilder()
  var index = 0

  while index < name.length do
    val char = name.charAt(index)
    if char.isUpper && index > 0 then builder.append('-')
    builder.append(char.toLower)
    index += 1

  builder.toString.tt

private def byName[value](values: scala.Array[value])(text: Text): Optional[value] =
  var index = 0
  var found: Optional[value] = Unset

  while found.absent && index < values.length do
    if kebab(values(index)) == text then found = values(index)
    index += 1

  found

given toneTelEncodable: Tone is Tel.Encodable = scalar(kebab)
given toneTelDecodable: Tone is Tel.Decodable = parsed("tone")(byName(Tone.values))
given glyphTelEncodable: Glyph is Tel.Encodable = scalar(kebab)
given glyphTelDecodable: Glyph is Tel.Decodable = parsed("glyph")(byName(Glyph.values))
given standingTelEncodable: Standing is Tel.Encodable = scalar(kebab)
given standingTelDecodable: Standing is Tel.Decodable = parsed("standing")(byName(Standing.values))
given accentTelEncodable: Token.Accent is Tel.Encodable = scalar(kebab)
given accentTelDecodable: Token.Accent is Tel.Decodable = parsed("accent")(byName(Token.Accent.values))
given roleTelEncodable: Token.Role is Tel.Encodable = scalar(kebab)
given roleTelDecodable: Token.Role is Tel.Decodable = parsed("role")(byName(Token.Role.values))
given alignmentTelEncodable: Block.Alignment is Tel.Encodable = scalar(kebab)

given alignmentTelDecodable: Block.Alignment is Tel.Decodable =
  parsed("alignment")(byName(Block.Alignment.values))

given noteStyleTelEncodable: Block.Note.Style is Tel.Encodable = scalar(kebab)

given noteStyleTelDecodable: Block.Note.Style is Tel.Decodable =
  parsed("note style")(byName(Block.Note.Style.values))

given chartKindTelEncodable: Block.Chart.Kind is Tel.Encodable = scalar(kebab)

given chartKindTelDecodable: Block.Chart.Kind is Tel.Decodable =
  parsed("chart kind")(byName(Block.Chart.Kind.values))

// A column sizing is a scalar too: its name, or `collapsible` with its priority after a colon.
given sizingTelEncodable: Block.Sizing is Tel.Encodable = scalar:
  case Block.Sizing.Collapsible(priority) => t"collapsible:${priority.toString}"
  case other                              => kebab(other)

given sizingTelDecodable: Block.Sizing is Tel.Decodable = parsed("sizing"): text =>
  text match
    case t"stretch"   => Block.Sizing.Stretch
    case t"rigid"     => Block.Sizing.Rigid
    case t"paragraph" => Block.Sizing.Paragraph

    case other =>
      if other.starts(t"collapsible:")
      then safely(other.s.substring(12).nn.tt.as[Double]).let(Block.Sizing.Collapsible(_))
      else Unset
