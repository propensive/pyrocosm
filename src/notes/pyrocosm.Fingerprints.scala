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

// The alias is root-qualified: resolving it through an import would need the package's top-level
// definitions, which include the export of this very type, a cyclic reference.
import anticipation.*
import gossamer.*
import spectacular.*
import vacuous.*

// A Pyrocosm hash: the hash of a git tree object holding a commit's tree with a `Filter`'s
// exclusions removed. Several commits (rebases, doc-only changes, squash merges) share one
// fingerprint, and one commit has as many fingerprints as filters have been applied to it, so a
// fingerprint never records which filter produced it: either a note exists for it or it does
// not. Opaque, held in an object and exported, as `Commit` is.
object Fingerprints:
  opaque type Fingerprint = _root_.anticipation.Text

  object Fingerprint:
    def unsafe(text: Text): Fingerprint = text
    def parse(text: Text): Optional[Fingerprint] = if Hashes.valid(text) then text else Unset

    given showable: Fingerprint is Showable = fingerprint => fingerprint
    given inspectable: Fingerprint is Inspectable = fingerprint => t"Fingerprint($fingerprint)"

  extension (fingerprint: Fingerprint)
    def text: Text = fingerprint
