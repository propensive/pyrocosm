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

// Excluded from the umbrella: `Filter` (panopticon), as in `Notes`.
import soundness.{Filter as _, *}

// The tool-facing verbs. "The bench data for this commit" is `commit.record[Bench]`: the commit's
// fingerprints, newest first, are tried against the `bench` ref, and the first that has a note
// wins. "Record this run's bench data" is `commit.record[Bench](filter, data)`: the fingerprint
// is computed, the note written on it, and the fingerprint bound to the commit.
extension (commit: Commit)
  def record[value: Recordable](using notes: Notes): Optional[value] raises Notes.Error =
    def recur(fingerprints: List[Fingerprint]): Optional[value] = fingerprints match
      case head :: tail => notes.read[value](head).or(recur(tail))
      case _            => Unset

    recur(notes.fingerprints(commit))

  // Every record of the kind across the commit's fingerprints, newest first. (`records` is taken by
  // the umbrella.)
  def recordings[value: Recordable](using notes: Notes): List[value] raises Notes.Error =
    def recur(fingerprints: List[Fingerprint], done: List[value]): List[value] = fingerprints match
      case head :: tail => notes.read[value](head).lay(recur(tail, done)) { value => recur(tail, value :: done) }
      case _            => done.reverse

    recur(notes.fingerprints(commit), Nil)

  def record[value: Recordable](filter: Filter, value: value)(using notes: Notes)
  :   Fingerprint raises Notes.Error =

    val fingerprint = notes.fingerprint(commit, filter)
    commit.record[value](fingerprint, value)
    fingerprint

  def record[value: Recordable](fingerprint: Fingerprint, value: value)(using notes: Notes)
  :   Unit raises Notes.Error =

    notes.write[value](fingerprint, value)
    notes.bind(commit, fingerprint)
