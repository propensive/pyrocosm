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

import scala.collection.concurrent.TrieMap

import soundness.*

import filesystemBackends.javaBaseFilesystem

// A value loaded from a file and kept until the file changes: the file is stat-checked on
// every call — one `stat` yields both the modification time and the size, so the check costs
// one filesystem operation — and `load` is called again only when either differs. A daemon
// reads its configuration, an index, a document this way, so that an edit is honoured by the
// very next command and nothing is re-read between edits. `Unset` is cached too: an absent or
// unreadable file is asked about once per change, not once per call.
final class Memo[value](load: Path on Linux => Optional[value]):
  private case class Cached(modified: Long, size: Long, value: Optional[value])
  private val cache: TrieMap[Text, Cached] = TrieMap()

  def apply(path: Path on Linux): Optional[value] =
    val key: Text = path.encode

    safely(summon[FilesystemBackend on Linux].stat(path, true)) match
      case stat: Stat =>
        cache.get(key) match
          case Some(cached) if cached.modified == stat.modified && cached.size == stat.size =>
            cached.value

          case _ =>
            val loaded: Optional[value] = load(path)
            cache(key) = Cached(stat.modified, stat.size, loaded)
            loaded

      case _ =>
        cache.remove(key)
        Unset

  def invalidate(path: Path on Linux): Unit = cache.remove(path.encode)
