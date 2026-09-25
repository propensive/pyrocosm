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

import stratiform.TelSchematic

import alphabets.hexLowerCase
import logging.silentLogging

// A message channel between two Pyrocosm tools over a `coaxial.Duplex`: length-prefixed frames,
// each either a BinTEL document of the tool's own message type or a run of raw bytes that
// belongs to the message before it (a jar being shipped, a test event frame relayed verbatim),
// so bulk payloads never pass through a codec. The wire layout is
//
//   length : 4 bytes, big-endian, of what follows
//   tag    : 1 byte — 0 a message, 1 raw bytes, 2 a handshake document
//   payload
//
// A tool's message enum is encoded under its derived BinTEL schema (as Probably's test events
// are), and the schema's fingerprint — a BLAKE3 hash of its structural rendering — names the
// protocol in the handshake, so two tools whose message layouts differ refuse each other at the
// first frame rather than misreading fields. The handshake documents (`Peer.Handshake`) are
// Pyrocosm's own, and carry their own schema's fingerprint ahead of the document for the same
// reason. TEL's acceptance mechanism will later let a schema evolve compatibly; this first
// version compares fingerprints exactly.
object Channel:
  private val messageTag: Byte = 0
  private val rawTag: Byte = 1
  private val handshakeTag: Byte = 2

  // What the peer sent: a decoded message, raw bytes, a handshake document (with the
  // fingerprint it carried), or the end of the connection.
  enum Frame[+message]:
    case Message(message: message)
    case Raw(data: Data)
    case Handshake(fingerprint: Data, document: Data)
    case Closed

  // A canonical structural rendering of a derived schema — name, document structure, and
  // select definitions with their variants, in declaration order — hashed with BLAKE3: a
  // fingerprint of the WIRE LAYOUT, as `probably.Streamer` computes for test events.
  def fingerprint(schema: Tels): Data =
    def renderType(kind: Tels.Type): Text = kind match
      case Tels.Struct(members, _) =>
        val rendered = members.to[List].map { (member: Tels.Member) => renderMember(member) }
        t"{${rendered.join(t";")}}"

      case Tels.Scalar(_, encoding, _) => t"scalar(${encoding.or(t"")})"
      case Tels.Flag                   => t"flag"
      case Tels.Reference(name)        => t"ref($name)"

    def renderMember(member: Tels.Member): Text = member match
      case Tels.Field(required, repeatable, keyword, fieldType, _, _, _) =>
        t"$keyword:${required.toString}:${repeatable.toString}:${renderType(fieldType)}"

      case Tels.SelectRef(required, repeatable, reference) =>
        t"select:$reference:${required.toString}:${repeatable.toString}"

      case Tels.Exclude(keyword) =>
        t"exclude:$keyword"

    def renderSelect(select: Tels.SelectDefinition): Text =
      val variants =
        select.variants.to[List].map: (variant: Tels.Variant) =>
          t"${variant.keyword}=${renderType(variant.variantType)}"

      t"${select.name}[${variants.join(t",")}]"

    val selects =
      schema.selects.to[List].map { (select: Tels.SelectDefinition) => renderSelect(select) }

    val rendering: Text = t"${schema.name}|${renderType(schema.document)}|${selects.join(t"|")}"

    Blake3.hashOf(rendering.sysData, 32)

  // The codec of a tool's message type: its derived schema, the schema's fingerprint, and the
  // BinTEL encoding and decoding under it. Made where the type is concrete, since the schema
  // and both codecs are derived inline.
  class Codec[message]
    ( val name: Text, val schema: Tels, encoder: message => Data, decoder: Data => message ):

    lazy val fingerprint: Data = Channel.fingerprint(schema)
    lazy val protocol: Text = fingerprint.serialize[Hex]
    def encode(message: message): Data = encoder(message)
    def decode(data: Data): message = decoder(data)

  // A codec is made at the CONCRETE message type, as Probably's `Streamer` makes its own:
  //
  //   lazy val codec: Channel.Codec[Message] =
  //     import Channel.derivation.throwing
  //     val schema = Tels.tels[Message](t"my-tool")
  //     Channel.Codec(t"my-tool", schema, m => Channel.encode(m, schema), Channel.decode[Message](_))
  //
  // A generic inline constructor taking the derived instances as `using` parameters was tried
  // and defeats the compiler's implicit-scope computation for the derivations, so the three
  // derived calls are spelled at each tool's site instead, with these two helpers.
  inline def encode[message: Encodable in Tel](message: message, schema: Tels): Data =
    unsafely(message.tel.bintel(schema))

  // A decode failure throws, and a caller wraps it in `safely`.
  inline def decode[message: Tel.Decodable](data: Data)
    ( using message is TelSchematic over Tels.Type )
  :   message =

    unsafely(Bintel.read[message](data))

  // The derived codecs of a message type resolve, where they are derived, a `Tactic` for each
  // failure the derivation can raise (a document fitting no variant, a malformed scalar, …).
  // Under capture checking the throwing strategy is a tracked capability, and a derived
  // instance must be pure, so a codec's site imports this vouched-pure throwing tactic
  // instead: `import Channel.derivation.throwing`. A decode failure then surfaces as an
  // exception, which `safely` around `decode` absorbs.
  object derivation:
    given throwing: [error <: Hazard] => Tactic[error] =
      scala.caps.unsafe.unsafeAssumePure[Tactic[error]](ThrowTactic[error, Any]())

  def same(left: Data, right: Data): Boolean = left.readable.sameElements(right.readable)

  private def join(parts: List[Data]): Data =
    val total: Int = parts.fold(0)(_ + _.length)
    val out = Array.allocate[Byte](total)
    var offset: Int = 0

    parts.each: part =>
      out.place(part, 0, offset, part.length)
      offset += part.length

    Array.freeze(out)

  def slice(data: Data, from: Int, until: Int): Data =
    val out = Array.allocate[Byte]((until - from).max(0))
    if until > from then out.place(data, from, 0, until - from)
    Array.freeze(out)

// One end of a connection, speaking `codec`'s messages. Reads block; writes are serialised by
// a mutex so frames from concurrent senders never interleave. The `Duplex` contract is a
// single reader, so `receive` must be called from one thread at a time.
class Channel[message](codec: Channel.Codec[message], duplex: Duplex):
  private val writer: Mutex = Mutex()

  // The duplex's one read endpoint, opened once for the channel's life.
  private val source: zephyrine.Stream[Data] over zephyrine.Credit = duplex.source

  // Bytes read from the duplex but not yet consumed.
  @scala.caps.unsafe.untrackedCaptures
  private var buffer: Data = Array.empty[Byte]

  private def header(tag: Byte, length: Int): Data =
    val total: Int = length + 1

    Array[Byte]
      ( ((total >> 24) & 0xff).toByte,
        ((total >> 16) & 0xff).toByte,
        ((total >> 8) & 0xff).toByte,
        (total & 0xff).toByte,
        tag )

  private def frame(tag: Byte, payload: Data): Unit = writer:
    val out = Array.allocate[Byte](5 + payload.length)
    out.place(header(tag, payload.length), 0, 0, 5)
    out.place(payload, 0, 5, payload.length)
    duplex.send(Stream(Array.freeze(out)))

  def send(message: message): Unit = frame(Channel.messageTag, codec.encode(message))
  def sendRaw(data: Data): Unit = frame(Channel.rawTag, data)

  // A message and the raw bytes it announces, under one hold of the writer, so another
  // sender's frame can never come between them.
  def send(message: message, raw: Data): Unit = writer:
    frame(Channel.messageTag, codec.encode(message))
    frame(Channel.rawTag, raw)

  // A handshake document, prefixed by its schema's fingerprint.
  def sendHandshake(fingerprint: Data, document: Data): Unit =
    val out = Array.allocate[Byte](fingerprint.length + document.length)
    out.place(fingerprint, 0, 0, fingerprint.length)
    out.place(document, 0, fingerprint.length, document.length)
    frame(Channel.handshakeTag, Array.freeze(out))

  // Pulls at least one more chunk from the duplex into the buffer; `false` at the end of the
  // stream.
  private def pull(): Boolean =
    source.refill(Credit(65536)) match
      case count: Int if count > 0 =>
        val chunk: Data = source.lend { region => range => region.materialize(range.capped(count)) }
        source.skip(count)
        buffer = Channel.join(List(buffer, chunk))
        true

      case count: Int =>
        pull()

      case _ =>
        false

  // Exactly `count` bytes from the head of the stream, or `Unset` if it ends first.
  private def take(count: Int): Optional[Data] =
    if buffer.length >= count then
      val taken: Data = Channel.slice(buffer, 0, count)
      buffer = Channel.slice(buffer, count, buffer.length)
      taken
    else if pull() then take(count)
    else Unset

  private def readInt(data: Data): Int =
    def byte(index: Int): Int = data.readable(index) & 0xff
    (byte(0) << 24) | (byte(1) << 16) | (byte(2) << 8) | byte(3)

  def receive(): Channel.Frame[message] =
    take(4).lay(Channel.Frame.Closed): lengthBytes =>
      val length: Int = readInt(lengthBytes)

      if length < 1 then Channel.Frame.Closed else
        take(length).lay(Channel.Frame.Closed): body =>
          val tag: Byte = body.readable(0)
          val payload: Data = Channel.slice(body, 1, body.length)

          tag match
            case Channel.messageTag => Channel.Frame.Message(codec.decode(payload))
            case Channel.rawTag     => Channel.Frame.Raw(payload)

            case Channel.handshakeTag =>
              val split: Int = payload.length.min(32)
              Channel.Frame.Handshake
                ( Channel.slice(payload, 0, split), Channel.slice(payload, split, payload.length) )

            case _ =>
              Channel.Frame.Closed

  def close(): Unit = duplex.close()
