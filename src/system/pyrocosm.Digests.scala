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
import providers.soundnessProvider

// Content digests, as every tool keys its caches and names its blobs: Blake3, as TEL's own
// value hashes are, written as lower-case hex. A list of parts is hashed with each part
// prefixed by its length, so that no two lists make the same text; a directory by its regular
// files, each by its path relative to the directory and its bytes, in the order of their
// paths; a classpath by its entries, in order.
object Digests:
  def of(data: Data): Text = data.digest[Blake3].serialize[Hex]

  def of(parts: Text*): Text =
    val prefixed: Text =
      List(parts*).map { (part: Text) => t"${part.length.show}:$part" }.join(t"")

    of(prefixed.sysData)

  // A file's bytes, or a directory's files by their relative paths and bytes, or a marker for
  // a path which is neither.
  def tree(path: Path on Linux): Text =
    if Filing.directory(path) then
      val root: Text = path.encode

      val files: List[Path on Linux] =
        Filing.beneath(path).stdlib.sortBy(_.encode.s).to(List)

      // Each file as one part, its relative path and its digest separated by a byte no path
      // contains, so that a change to either changes the whole.
      val parts: List[Text] =
        files.map: file =>
          t"${relative(root, file.encode)}\u0000${Filing.data(file).lay(t"absent")(of(_))}"

      of(parts*)
    else Filing.data(path).lay(of(t"absent", path.encode))(of(_))

  def classpath(entries: List[Text]): Text =
    of(entries.map { (entry: Text) => tree(unsafely(entry.as[Path on Linux])) }*)

  // `path` with `root/` removed from its front, if it is beneath it.
  private[pyrocosm] def relative(root: Text, path: Text): Text =
    if path.starts(t"$root/") then path.skip(root.length + 1) else path
