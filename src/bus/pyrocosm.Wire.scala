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

import alphabets.hexLowerCase
import codepages.utf8Codepage
import stratiform.TelSchematic

// One tool's daemon, as a node of the swarm: the machine it runs on — its hostname, and the
// fingerprint of the machine's certificate, by which two nodes know they share a machine — the
// tool, its version, and the process, whose id and start make the node unique across restarts.
// `hosts` are the addresses the machine may be reached at, nearest first, for a node on
// another machine to make a link to its web front-end.
case class Node
  ( machine:  Text,
    identity: Text,
    tool:     Text,
    version:  Text,
    pid:      Long,
    started:  Instant over Unix,
    hosts:    List[Text] ):

  // What an envelope names as its origin, and what a node is known by everywhere.
  def key: Text = t"$identity/$tool/${pid.show}/${started.long.show}"

  // Whether `other` runs on this node's machine.
  def shares(other: Node): Boolean = identity == other.identity

// A port a node serves on, by the keyword its configuration starts it with: `serve` for the web
// front-end, whose `url` is where a browser finds it; `listen` for fume's worker, and so on.
case class Service(name: Text, port: Int, url: Optional[Text] = Unset)

// What a node says of itself, when it joins and every half-minute after: which node it is,
// what it serves, what it is capable of, what machine it runs on and how loaded, and whether it
// is its machine's gateway, which relays between the machine's nodes and the others.
case class Advert
  ( node:         Node,
    services:     List[Service],
    capabilities: List[Text],
    os:           Text,
    arch:         Text,
    cores:        Int,
    load:         Optional[Double],
    gateway:      Boolean ):

  // Where the node's web front-end is, if it serves one.
  def url: Optional[Text] = services.filter(_.name == t"serve").prim.let(_.url).or(Unset)

object Bus:
  // The derivations below need a tactic for a malformed document; a failure is an exception
  // here, and an absent result to a caller of `decode`.
  import Channel.derivation.throwing

  // What one node tells every other: that it is there (an advert, repeated while it is), that
  // it is going, that one of its activities has changed or ended, or something of its tool's
  // own, as a self-contained BinTEL document any node can read as TEL and the tool's own
  // instances read as its type. Nothing larger than `limit` is carried.
  enum Event:
    case Advertised(advert: Advert)
    case Left(node: Node)
    case Updated(node: Node, activity: Text)
    case Ended(node: Node, activity: Text)
    case Tool(tool: Text, kind: Text, payload: Data)

  // The most bytes a tool's own payload may be.
  val limit: Int = 16384

  // An action has no meaning on another node, so an internal destination is dropped before an
  // activity travels; the codec still needs the type, which is read back as a fresh action.
  given actionEncodable: Action is Tel.Encodable =
    Tel.Encodable(() => Morphology.Str, Tel.Nature.Scalar): action => Tel.scalar(action.id)

  given actionDecodable: Action is Tel.Decodable =
    unsafely:
      scala.caps.unsafe.unsafeAssumePure:
        Tel.Decodable(() => Morphology.Str, Tel.Nature.Scalar): tel => Action(tel.primaryAtom)

  // An activity travels as its TEL text, under the model's own codecs for its inlines and
  // status — which evolve with the model — rather than under the bus's schema, which does not.
  private lazy val activityEncodable: Activity is Tel.Encodable = Tel.EncodableDerivation.derived[Activity]
  private lazy val activityDecodable: Activity is Tel.Decodable = Tel.DecodableDerivation.derived[Activity]

  def encode(activity: Activity): Text =
    val portable: Activity = activity.destination match
      case Inline.Destination.Internal(_) => activity.copy(destination = Unset)
      case _                              => activity

    activityEncodable.encoded(portable).show

  def decode(text: Text): Optional[Activity] =
    try text.read[Tel].as[Activity](using activityDecodable) catch case _: Exception => Unset

  // A tool's own value as a payload: a BinTEL document under the schema derived from its type,
  // which the tool's other instances read back with `parse`. A node without the type sees only
  // the event's `kind`; carrying the schema within the document, so that any node could show
  // it as TEL, waits on stratiform exposing a schema as a document.
  inline def payload[value: Encodable in Tel](value: value)
    ( using schematic: value is TelSchematic over Tels.Type )
  :   Data =

    import Channel.derivation.throwing
    unsafely(value.bintel)

  // A payload as the type it was made from, if it is one.
  inline def parse[value: Tel.Decodable](payload: Data)
    ( using schematic: value is TelSchematic over Tels.Type )
  :   Optional[value] =

    import Channel.derivation.throwing
    try Bintel.read[value](payload) catch case _: Exception => Unset

// An event as it travels: who originated it, its number among that node's events — by which a
// node which hears it twice, by two routes, acts on it once — when it was sent, and how many
// gateways have relayed it, which bounds how far it goes. On the wire each kind of event is a
// message of its own carrying these four fields (`Wire.carry`), since a record holding a
// select — an envelope holding an event — is not a schema stratiform's resolver accepts.
case class Envelope
  ( origin:   Text,
    sequence: Long,
    sent:     Instant over Unix,
    hops:     Int,
    event:    Bus.Event )

object Wire:
  // An instant, bytes and an identifier are scalars, as the remote module writes them.
  import Scalars.given

  // The model's leaf types with scalar codecs of their own (`pyrocosm_codecs.scala`), which the
  // schema must know as scalars too.
  given keypressSchematic: Keypress is TelSchematic over Tels.Type = () => Tels.Scalar(Array.empty)
  given mathSchematic: archimedes.Math is TelSchematic over Tels.Type = () => Tels.Scalar(Array.empty)
  given toneSchematic: Tone is TelSchematic over Tels.Type = () => Tels.Scalar(Array.empty)
  given glyphSchematic: Glyph is TelSchematic over Tels.Type = () => Tels.Scalar(Array.empty)
  given standingSchematic: Standing is TelSchematic over Tels.Type = () => Tels.Scalar(Array.empty)
  given accentSchematic: Token.Accent is TelSchematic over Tels.Type = () => Tels.Scalar(Array.empty)
  given roleSchematic: Token.Role is TelSchematic over Tels.Type = () => Tels.Scalar(Array.empty)

  given instantEncodable: (Instant over Unix) is Tel.Encodable =
    Tel.Encodable(() => Morphology.Whole, Tel.Nature.Scalar): instant =>
      Tel.scalar(instant.long.show)

  given instantDecodable: Tactic[Tel.Error] => (Instant over Unix) is Tel.Decodable =
    Tel.Decodable(() => Morphology.Whole, Tel.Nature.Scalar): tel =>
      Instant.of[Unix](summon[Long is Tel.Decodable].decoded(tel))

  given dataEncodable: Data is Tel.Encodable =
    Tel.Encodable(() => Morphology.Str, Tel.Nature.Scalar): data => Tel.scalar(data.serialize[Hex])

  given dataDecodable: Tactic[Tel.Error] => Data is Tel.Decodable =
    Tel.Decodable(() => Morphology.Str, Tel.Nature.Scalar): tel =>
      unsafely(summon[Text is Tel.Decodable].decoded(tel).deserialize[Hex])

  // Everything below is derived from the `Wire` enum, which is why it is in a module compiled
  // without capture checking, as fume keeps its `Relay`: stratiform's derivations expand to
  // instances the capture checker sees as fresh where a pure instance is required. A failure
  // inside any of them is an exception here, and an absent result to a caller.
  import Channel.derivation.throwing

  // The protocol's schema, derived from the enum.
  lazy val schema: Tels = Tels.tels[Wire](t"pyrocosm-bus")

  // The hash of the schema's base, by which the schema is known.
  lazy val signature: Data = SchemaSignature.componentHashes(schema, Tels.Axiom.tels)(0)

  // What this build can read: an acceptance (BinTEL §8.4) of the protocol's one form. A later
  // build which adds a message is a second form, named ahead of this one.
  private lazy val accepting: Tel.Acceptance.Typed[Tuple1[Wire]] = Tel.Acceptance[Tuple1[Wire]]()

  def acceptance: Tel.Acceptance = accepting.acceptance

  // The acceptance as it is sent, a framed BinTEL document, at the start of every link.
  lazy val offer: Data = acceptance.framed

  // The acceptance the other end sent, if that is what `data` is.
  def offered(data: Data): Optional[Tel.Acceptance] =
    try Tel.Acceptance(data) catch case _: Exception => Unset

  // A message as the other end has said it can read it: a framed BinTEL document under the
  // first form of the protocol its acceptance names that this build can serve. Absent if it
  // names none.
  def write(message: Wire, theirs: Tel.Acceptance): Optional[Data] =
    try message.fulfil(theirs).let(_.document) catch case _: Exception => Unset

  // A message read from a document written to this build's acceptance. Absent if the document
  // is of no form this build accepts.
  def read(data: Data): Optional[Wire] =
    try accepting.read(data) catch case _: Exception => Unset

  // An envelope as the message which bears it, and back.
  def carry(envelope: Envelope): Wire =
    val Envelope(origin, sequence, sent, hops, event) = envelope

    event match
      case Bus.Event.Advertised(advert)     => Wire.Advertised(origin, sequence, sent, hops, advert)
      case Bus.Event.Left(node)             => Wire.Left(origin, sequence, sent, hops, node)
      case Bus.Event.Updated(node, text)    => Wire.Updated(origin, sequence, sent, hops, node, text)
      case Bus.Event.Ended(node, id)        => Wire.Ended(origin, sequence, sent, hops, node, id)
      case Bus.Event.Tool(tool, kind, data) => Wire.Tool(origin, sequence, sent, hops, tool, kind, data)

  def envelope(wire: Wire): Optional[Envelope] = wire match
    case Wire.Advertised(origin, sequence, sent, hops, advert)  => Envelope(origin, sequence, sent, hops, Bus.Event.Advertised(advert))
    case Wire.Left(origin, sequence, sent, hops, node)          => Envelope(origin, sequence, sent, hops, Bus.Event.Left(node))
    case Wire.Updated(origin, sequence, sent, hops, node, text) => Envelope(origin, sequence, sent, hops, Bus.Event.Updated(node, text))
    case Wire.Ended(origin, sequence, sent, hops, node, id)     => Envelope(origin, sequence, sent, hops, Bus.Event.Ended(node, id))
    case Wire.Tool(origin, sequence, sent, hops, tool, kind, d) => Envelope(origin, sequence, sent, hops, Bus.Event.Tool(tool, kind, d))
    case _                                                      => Unset

  // What Pyrocosm's channel carries for the swarm: documents, opaquely. Pyrocosm's handshake
  // compares the fingerprint of a channel's schema and refuses any difference; the swarm's
  // channel is given the acceptance schema, which never differs, so that it is the acceptances
  // exchanged over the channel, and not that comparison, which decide what two nodes can say.
  lazy val codec: Channel.Codec[Data] =
    Channel.Codec(t"pyrocosm", Tels.Axiom.acceptance, data => data, data => data)

  // The tool every node presents itself as, whatever tool it is; and the port a machine's
  // gateway accepts other machines on by default. 8090 to 8092 are fume's.
  val tool: Text = t"pyrocosm"
  val port: Int = 8093

// What one node says to another over a link. A `beat` each second is how each end of a link
// knows the other is still there, and says how loaded it is; a `ping` is answered by a `pong`,
// for `<tool> swarm ping`; a `snapshot` is what each end sends first, once the acceptances are
// exchanged: what it is, and what it knows of every other node; and the rest each bear one
// event of the bus, with its envelope's fields, which a gateway passes on to the links the
// event has not yet travelled.
enum Wire:
  case Beat(sent: Instant over Unix, load: Optional[Double])
  case Ping(id: Uuid, sent: Instant over Unix, note: Text)
  case Pong(id: Uuid, received: Instant over Unix, from: Text)
  case Snapshot(self: Advert, known: List[Advert])
  case Advertised(origin: Text, sequence: Long, sent: Instant over Unix, hops: Int, advert: Advert)
  case Left(origin: Text, sequence: Long, sent: Instant over Unix, hops: Int, node: Node)
  case Updated(origin: Text, sequence: Long, sent: Instant over Unix, hops: Int, node: Node, activity: Text)
  case Ended(origin: Text, sequence: Long, sent: Instant over Unix, hops: Int, node: Node, activity: Text)
  case Tool(origin: Text, sequence: Long, sent: Instant over Unix, hops: Int, tool: Text, kind: Text, payload: Data)
