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
import filesystemBackends.javaBaseFilesystem
import filesystemOptions.dereferenceSymlinks
import filesystemTraversal.preOrderTraversal
import logging.silentLogging
import providers.javaBaseProvider
import systems.javaBaseSystem

// Content-addressed shipping of classpath entries between machines. A jar is named by the
// SHA-256 of its bytes; a directory of classes is first bundled into a jar deterministically
// — entries sorted by path, epoch timestamps — so that the same classes give the same digest
// however they were laid down. A machine keeps every blob it has received under
// `$XDG_CACHE_HOME/pyrocosm/blobs/<digest>.jar`, shared by every tool on it, so a worker asks
// only for the digests it lacks and a controller sends only those.
object Blobs:
  // One classpath entry as the wire describes it: its digest, whether it was a directory at
  // the sender (so the receiver knows the blob is a bundle), and its name for messages.
  case class Entry(digest: Text, directory: Boolean, name: Text)

  // A frame's worth of a blob in transit.
  val chunk: Int = 1024*1024

  def digest(data: Data): Text = data.digest[Sha2[256]].serialize[Hex]

  private def relative(root: Path on Linux, path: Path on Linux): Text =
    val prefix: Text = root.encode
    val whole: Text = path.encode
    if whole.starts(prefix) then whole.s.substring((prefix.s.length + 1).min(whole.s.length)).nn.tt else whole

  // A directory's regular files as one jar, in a canonical order with no timestamps.
  def bundle(directory: Path on Linux): Data raises Io.Error =
    val files: List[Path on Linux] =
      directory.descendants.to[List].filter(_.entry() != Directory).stdlib.sortBy(relative(directory, _).s).to(List)

    val entries: List[Zip.Entry] =
      files.map: file =>
        val name: Text = relative(directory, file)
        val ref: Path on Zip =
          unsafely(name.cut(t"/").stdlib.foldLeft(% : Path on Zip) { (path, segment) => path / Name[Zip](segment) })
        Zip.Entry(ref, file.read[Data])

    Zipfile(entries).serialize.memoize

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
          recur(until, (offset, Channel.slice(data, offset, until)) :: acc)

      recur(0, Nil)

  // The cache directory, created if absent; `Unset` if the environment names no cache home or
  // it cannot be created.
  def store(using Environment): Optional[Store] =
    safely(Xdg.cacheHome[Path on Linux] / "pyrocosm" / "blobs").let: directory =>
      safely:
        if !directory.existent() then directory.create[Directory](CreateFlag.Parents)
        Store(directory)

  class Store(val directory: Path on Linux):
    def path(digest: Text): Path on Linux = unsafely(directory / t"$digest.jar")
    def has(digest: Text): Boolean = path(digest).existent()
    def missing(digests: List[Text]): List[Text] = digests.filter(!has(_))

    // Stores `data` under `digest` if the bytes really have it; `false` otherwise.
    def put(digest: Text, data: Data): Boolean =
      if Blobs.digest(data) != digest then false
      else safely(path(digest).write(data)).present

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
        val whole: Data = join(sofar.reverse)
        store.put(digest, whole)

  private def join(parts: List[Data]): Data =
    val total: Int = parts.fold(0)(_ + _.length)
    val out = Array.allocate[Byte](total)
    var offset: Int = 0

    parts.each: part =>
      out.place(part, 0, offset, part.length)
      offset += part.length

    Array.freeze(out)
