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

import codepages.utf8Codepage

object Recordable:
  // A kind whose body is the type's TEL text, as the rest of the ecosystem's documents.
  def apply[value](kind0: Text)
    ( using encodable: value is Tel.Encodable, decodable: value is Tel.Decodable )
  :   value is Recordable =

    of[value](kind0)
      ( value => value.in[Tel].show.sysData,
        data => safely(data.utf8.read[Tel].as[value]) )

  // A kind of any wire form, from its codec: a BinTEL body, say, whose derivation site sits in
  // an unchecked module, as `Recordable.of[Verdict](t"verdict")(_.bintel, Bintel.read(_))`.
  def of[value](kind0: Text)(encode0: value -> Data, decode0: Data -> Optional[value])
  :   value is Recordable =

    new Recordable:
      type Self = value
      def kind: Text = kind0
      def encode(value: value): Data = encode0(value)
      def decode(data: Data): Optional[value] = decode0(data)

// What kind of metadata a type is: the name of the notes ref that holds it (`bench` is
// `refs/notes/pyrocosm/bench`) and its wire form — TEL text, as the rest of the ecosystem's
// documents, for an instance made from the type's TEL codecs with `Recordable[Bench]("bench")`,
// or whatever bytes `Recordable.of` is given.
trait Recordable extends Typeclass:
  def kind: Text
  def encode(value: Self): Data
  def decode(data: Data): Optional[Self]
