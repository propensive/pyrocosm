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

import filesystemBackends.javaBaseFilesystem
import logging.silentLogging

// A content-addressed store: one file per blob, named by the Blake3 digest of its bytes with
// `suffix` after it, under one directory. Every tool that keeps bytes by their digest — the
// blob cache a worker fills from a controller, a documentation tool's compiled objects —
// keeps them this way, so that a blob is written once, verified as it is written, and found
// by any tool that knows its digest.
object ObjectStore:
  // The store at `directory`, created if absent; `Unset` if it cannot be.
  def at(directory: Path on Linux, suffix: Text): Optional[ObjectStore] =
    safely:
      if !directory.existent() then directory.create[Directory](CreateFlag.Parents)
      ObjectStore(directory, suffix)

final class ObjectStore(val directory: Path on Linux, val suffix: Text):
  def path(digest: Text): Path on Linux = unsafely(directory / t"$digest$suffix")
  def has(digest: Text): Boolean = path(digest).existent()
  def missing(digests: List[Text]): List[Text] = digests.filter(!has(_))

  // Stores `data` under its own digest, unless it is there already; the digest.
  def put(data: Data): Text =
    val digest: Text = Digests.of(data)
    if !has(digest) then Filing.replace(path(digest), data)
    digest

  // Stores `data` under `digest` if the bytes really have it; `false` otherwise.
  def put(digest: Text, data: Data): Boolean =
    if Digests.of(data) != digest then false else has(digest) || Filing.replace(path(digest), data)

  def get(digest: Text): Optional[Data] = if has(digest) then Filing.data(path(digest)) else Unset

  // The digests stored.
  def digests: List[Text] =
    def named(name: Text): Text = name.keep(name.length - suffix.length)
    safely(directory.children.to[List]).or(Nil).map(_.name).filter(_.ends(suffix)).map(named(_))

  // Deletes every blob whose digest is not in `keep`; how many were deleted.
  def prune(keep: Set[Text]): Int =
    val stale: List[Text] = digests.filter(!keep.has(_))
    stale.each: digest => Filing.wipe(path(digest))
    stale.stdlib.length
