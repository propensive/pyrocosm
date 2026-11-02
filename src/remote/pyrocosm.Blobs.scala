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
import filesystemOptions.dereferenceSymlinks
import filesystemTraversal.preOrderTraversal
import systems.javaBaseSystem

// Content-addressed shipping of classpath entries between machines. A jar is named by the
// Blake3 digest of its bytes (`Digests`); a directory of classes is first bundled into a jar
// deterministically — entries sorted by path, epoch timestamps — so that the same classes give
// the same digest however they were laid down. A machine keeps every blob it has received under
// `$XDG_CACHE_HOME/pyrocosm/blobs/<digest>.jar`, shared by every tool on it, so a worker asks
// only for the digests it lacks and a controller sends only those.
object Blobs:
  // One classpath entry as the wire describes it: its digest, whether it was a directory at
  // the sender (so the receiver knows the blob is a bundle), and its name for messages.
  case class Entry(digest: Text, directory: Boolean, name: Text)

  // A frame's worth of a blob in transit.
  val chunk: Int = 1024*1024

  def digest(data: Data): Text = Digests.of(data)

  private def relative(root: Path on Linux, path: Path on Linux): Text =
    val prefix: Text = root.encode
    val whole: Text = path.encode

    if whole.starts(prefix)
    then whole.s.substring((prefix.s.length + 1).min(whole.s.length)).nn.tt
    else whole

  // A directory's regular files as one jar, in a canonical order with no timestamps.
  def bundle(directory: Path on Linux): Data raises Io.Error =
    val files: List[Path on Linux] =
      directory.descendants.to[List].filter(_.entry() != Directory).stdlib
      . sortBy(relative(directory, _).s)
      . to(List)

    val entries: List[Zip.Entry] =
      files.map: file =>
        val name: Text = relative(directory, file)

        val ref: Path on Zip =
          unsafely:
            name.cut(t"/").stdlib.foldLeft(% : Path on Zip): (path, segment) =>
              path / Name[Zip](segment)

        Zip.Entry(ref, file.read[Data])

    unsafely(Zipfile(entries).serialize).memoize

  // The entry and the bytes for a classpath path: a jar is read as it is, a directory bundled.
  def prepare(path: Path on Linux, name: Text): (Entry, Data) raises Io.Error =
    if path.entry() == Directory then
      val data: Data = bundle(path)
      (Entry(digest(data), true, name), data)
    else
      val data: Data = path.read[Data]
      (Entry(digest(data), false, name), data)

  // The offsets at which `data` splits into frames of at most `chunk` bytes; a single empty
  // chunk for empty data, so the receiver always sees a last chunk.
  def chunks(data: Data): List[(Int, Data)] =
    if data.length == 0 then List((0, data)) else
      def recur(offset: Int, acc: List[(Int, Data)]): List[(Int, Data)] =
        if offset >= data.length then acc.reverse
        else
          val until: Int = (offset + chunk).min(data.length)
          recur(until, (offset, Frames.slice(data, offset, until)) :: acc)

      recur(0, Nil)

  // The cache directory, created if absent; `Unset` if the environment names no cache home or
  // it cannot be created.
  def store(using Environment): Optional[Store] =
    safely(Xdg.cacheHome[Path on Linux] / "pyrocosm" / "blobs").let(ObjectStore.at(_, t".jar"))

  // The blob cache is a content-addressed store of `.jar` files.
  type Store = ObjectStore

  // Reassembles blobs from chunks as they arrive, in any interleaving, and stores each when
  // its last chunk has come. `receive` answers whether a completed blob was good.
  class Assembly(store: Store):
    private val mutex: Mutex = Mutex()

    @scala.caps.unsafe.untrackedCaptures
    private var parts: Map[Text, List[Data]] = Map()

    def receive(digest: Text, data: Data, last: Boolean): Optional[Boolean] = mutex:
      val sofar: List[Data] = data :: parts(digest).or(Nil)

      if !last then
        parts = parts.define(digest, sofar)
        Unset
      else
        parts = parts.omit(digest)
        val whole: Data = Frames.join(sofar.reverse)
        store.put(digest, whole)
