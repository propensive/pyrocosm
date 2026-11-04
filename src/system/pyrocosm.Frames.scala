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

// Length-prefixed frames, as every stream of records a tool keeps or sends is cut: four
// big-endian bytes of length, then the record, which is obligatory's `LengthPrefix`. `split`
// takes the chunks a stream arrived in, however the frames fell across them, and yields
// whole frames; a trailing partial frame is dropped.
object Frames:
  def frame(record: Data): Data = LengthPrefix.encode(record)

  // The whole frames in `chunks`, in order.
  def split(chunks: List[Data]): List[Data] =
    safely(chunks.to[Chain].frames[LengthPrefix].to[List]).or(Nil)

  def split(data: Data): List[Data] = split(List(data))

  // `parts`, concatenated.
  def join(parts: List[Data]): Data =
    val total: Int = parts.fold(0)(_ + _.length)
    val out = Array.allocate[Byte](total)
    var offset: Int = 0

    parts.each: part =>
      out.place(part, 0, offset, part.length)
      offset += part.length

    Array.freeze(out)

  // The bytes of `data` from `from` until `until`.
  def slice(data: Data, from: Int, until: Int): Data =
    val length: Int = (until - from).max(0)
    val out = Array.allocate[Byte](length)
    if length > 0 then out.place(data, from, 0, length)
    Array.freeze(out)
