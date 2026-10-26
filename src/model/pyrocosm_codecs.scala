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

import soundness.*

import stratiform.TelSchematic

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

// An empty text is written as a compound with no atom at all (which reads back as the empty
// string), rather than as an empty inline atom: stratiform writes the latter's preceding
// space, which its own parser refuses as a trailing space, so a document holding an empty
// token, cell or phrase could never be read back.
given textTelEncodable: Text is Tel.Encodable =
  Tel.Encodable(() => Morphology.Str, Tel.Nature.Scalar): text =>
    if text == t"" then Tel.make(Tel.Compound(t"", Array.empty, Unset, Array.empty)) else Tel.scalar(text)

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

  // A figure's serial form (see `figureTelEncodable`), derived here where the list codecs
  // resolve, as the model types' are.
  val snapshotEncodable: Figure.Snapshot is Tel.Encodable =
    Tel.EncodableDerivation.derived[Figure.Snapshot]

  val snapshotDecodable: Figure.Snapshot is Tel.Decodable =
    Tel.DecodableDerivation.derived[Figure.Snapshot]

  val snapshotSchematic: Figure.Snapshot is TelSchematic over Tels.Type =
    stratiform.TelsDerivation.derived[Figure.Snapshot]

// A figure serialises as its snapshot — id, alt and the whole current drawing — and reads back
// as a figure with the same id, so a round trip compares equal.
given figureTelEncodable: Figure is Tel.Encodable =
  Tel.Encodable(() => Codecs.snapshotEncodable.shape(), Codecs.snapshotEncodable.nature): figure =>
    Codecs.snapshotEncodable.encoded(figure.snapshot)

given figureTelDecodable: Figure is Tel.Decodable =
  unsafely:
    caps.unsafe.unsafeAssumePure:
      Tel.Decodable(() => Codecs.snapshotDecodable.shape(), Codecs.snapshotDecodable.nature): tel =>
        Figure.restore(Codecs.snapshotDecodable.decoded(tel))

// Every enum of singleton cases reachable from a derived codec has a scalar codec here, keyed by
// its kebab-cased case name (`arrow-right`): a bare word is better TEL than the nested select a
// derived codec would write, and it is the form the bus's schema declares for them too. (A
// derived decoder for such an enum once failed under capture checking — Soundness #2152 fixed
// that — but the form is the reason these remain.)
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

// The schemas of the types above whose codecs are scalars, as stratiform declares its own
// primitives', so that a derived schema — which BinTEL is written under — reaches every leaf
// of the model as the codecs write it: a word, not a structure.
private def scalarSchema[value]: value is TelSchematic over Tels.Type =
  () => Tels.Scalar(Array.empty)

given keypressTelSchematic: Keypress is TelSchematic over Tels.Type = scalarSchema
given mathTelSchematic: archimedes.Math is TelSchematic over Tels.Type = scalarSchema
given sideTelSchematic: Block.Side is TelSchematic over Tels.Type = scalarSchema
given toneTelSchematic: Tone is TelSchematic over Tels.Type = scalarSchema
given glyphTelSchematic: Glyph is TelSchematic over Tels.Type = scalarSchema
given standingTelSchematic: Standing is TelSchematic over Tels.Type = scalarSchema
given accentTelSchematic: Token.Accent is TelSchematic over Tels.Type = scalarSchema
given roleTelSchematic: Token.Role is TelSchematic over Tels.Type = scalarSchema
given alignmentTelSchematic: Block.Alignment is TelSchematic over Tels.Type = scalarSchema
given noteStyleTelSchematic: Block.Note.Style is TelSchematic over Tels.Type = scalarSchema
given chartKindTelSchematic: Block.Chart.Kind is TelSchematic over Tels.Type = scalarSchema
given sizingTelSchematic: Block.Sizing is TelSchematic over Tels.Type = scalarSchema

// A figure is written as its snapshot, so its schema is the snapshot's, in every part.
given figureTelSchematic: Figure is TelSchematic over Tels.Type =
  val snapshot: Figure.Snapshot is TelSchematic over Tels.Type = Codecs.snapshotSchematic

  new TelSchematic:
    type Self = Figure
    def schema(): Tels.Type = snapshot.schema()
    override def fieldType: Tels.Type = snapshot.fieldType
    override def selectDefinitions: List[Tels.SelectDefinition] = snapshot.selectDefinitions

    override def definitions(seen: scala.collection.immutable.Set[Text])
    :   TelSchematic.Definitions =

      snapshot.definitions(seen)

    override def layers(seen: scala.collection.immutable.Set[Text]): List[Tels.Layer] =
      snapshot.layers(seen)

// The model's sums, each with the schema stratiform derives, but typed at a member position as
// TEL's specification types a sum there: a struct holding a `SelectRef` to it (§20.3), which is
// how the derived codecs write a sum-typed field — its keyword, with one variant beneath it.
// Stratiform's derivation types such a member as a `Reference` to the select instead, which
// type assignment rejects as a kind mismatch (E217; propensive/soundness#2319). These go once
// stratiform derives the member form itself.
given inlineTelSchematic: Inline is TelSchematic over Tels.Type = atMember(InlineSchema.derived)
given blockTelSchematic: Block is TelSchematic over Tels.Type = atMember(BlockSchema.derived)
given statusTelSchematic: Status is TelSchematic over Tels.Type = atMember(StatusSchema.derived)

given destinationTelSchematic: Inline.Destination is TelSchematic over Tels.Type =
  atMember(DestinationSchema.derived)

given transitionKindTelSchematic: Block.Transition.Kind is TelSchematic over Tels.Type =
  atMember(TransitionKindSchema.derived)

// A sum's type at a member position: a struct holding a `SelectRef` to it.
private def selectMember(name: Text): Tels.Type =
  val reference = Tels.SelectRef(Tels.Polarity.Implicit, Tels.Polarity.Implicit, name)
  Tels.Struct(Array(reference), Array.empty)

// Definitions with each member typed as a `Reference` to one of their selects retyped as the
// member form: this reaches the members a sum's own derivation types for itself (a recursive
// `List[Inline]` within `Inline`), where the wrapping givens above are not consulted.
private def membered(definitions: TelSchematic.Definitions): TelSchematic.Definitions =
  val selects: List[Text] = definitions.selects.map(_.name)

  def retyped(member: Tels.Member): Tels.Member = member match
    case field: Tels.Field => field.fieldType match
      case Tels.Reference(name) if selects.has(name) => field.copy(fieldType = selectMember(name))
      case _                                         => field

    case other =>
      other

  val records: List[Tels.RecordDefinition] =
    definitions.records.map: (record: Tels.RecordDefinition) =>
      record.copy(members = Array.from(record.members.stdlib.toList.map(retyped)))

  TelSchematic.Definitions(records, definitions.selects)

// A derived sum's schematic, typed at a member position as the member form.
private def atMember[value](derived: => value is TelSchematic over Tels.Type)
:   value is TelSchematic over Tels.Type =

  caps.unsafe.unsafeAssumePure:
    new TelSchematic:
      type Self = value
      private lazy val inner: value is TelSchematic over Tels.Type = derived

      def schema(): Tels.Type = inner.schema()

      override def fieldType: Tels.Type = inner.schema() match
        case Tels.Reference(name) => selectMember(name)
        case other                => other

      override def polarity: Tels.Polarity = inner.polarity
      override def repeatable: Tels.Polarity = inner.repeatable
      override def selectDefinitions: List[Tels.SelectDefinition] = inner.selectDefinitions

      override def definitions(seen: scala.collection.immutable.Set[Text])
      :   TelSchematic.Definitions =

        membered(inner.definitions(seen))

      override def layers(seen: scala.collection.immutable.Set[Text]): List[Tels.Layer] =
        inner.layers(seen)


given sideTelEncodable: Block.Side is Tel.Encodable = scalar(kebab)
given sideTelDecodable: Block.Side is Tel.Decodable = parsed("side")(byName(Block.Side.values))
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
