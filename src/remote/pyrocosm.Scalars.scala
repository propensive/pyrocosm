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
import stratiform.TelSchematic

// The scalar forms of the leaf types the remote module's documents carry, and every tool's
// message types reuse: an instant is written as the milliseconds since the Unix epoch, a
// fingerprint or any other bytes as hex, and an identifier as its text. Imported with
// `import Scalars.given` wherever a document holding one is derived.
object Scalars:
  given instantSchematic: (Instant over Unix) is TelSchematic over Tels.Type =
    () => Tels.Scalar(Array.empty)

  given dataSchematic: Data is TelSchematic over Tels.Type = () => Tels.Scalar(Array.empty)
  given uuidSchematic: Uuid is TelSchematic over Tels.Type = () => Tels.Scalar(Array.empty)

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
