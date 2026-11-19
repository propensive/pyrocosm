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

import java.util.concurrent as juc

import soundness.*

import alphabets.hexLowerCase
import filesystemBackends.javaBaseFilesystem
import systems.javaBaseSystem

// Data one tool keeps for the others, on this machine: a RaTEL image in a file, which its
// publisher replaces whole and every other tool maps into memory, so that looking one record up
// reads that record and nothing else, from pages the processes share. Nothing connects the two
// tools: there is no socket, no handshake and no message, and a reader is served whether or not
// the publisher is running, with whatever it last published.
//
// A share belongs to a tool and a project, and has a name: it is the file
// `$XDG_CACHE_HOME/<tool>/<digest of the project's root>/share/<name>.ratel`, under the cache
// the tool already keeps for the project (`Tool.projectCache`). The project is the publishing
// tool's — the directory holding its `.pyrocosm/<tool>/config.tel` — which a reader finds from
// its own working directory with `root`.
//
// The image carries its schema, so a reader needs none of the publisher's types: it opens the
// image as it is and asks for members by keyword, and a publisher may add members without
// breaking a reader which does not know them. A member marked `@key` is what `find` descends
// an index for.
//
// A reader is always current without being told of a change: `read` asks the filesystem
// whether the name leads to a new file — one `stat`, and at most one every `Shares.interval`
// — and maps the new one if it does. A published file is never changed (galilei's `claim` and
// `observe`), so a block reading the old one is undisturbed.
object Shares:
  // How long a reader goes without asking whether a newer file has been published.
  val interval: Long = 50L

  // The cache a tool keeps for the project at `root`: `$XDG_CACHE_HOME/<tool>/<digest>`.
  def cache(tool: Text, root: Text)(using Environment): Optional[Path on Linux] =
    safely:
      Xdg.cacheHome[Path on Linux] / Name[Linux](tool) / Name[Linux](Digests.of(root).keep(12))

  // The file a share is published in.
  def path(tool: Text, root: Text, name: Text)(using Environment): Optional[Path on Linux] =
    cache(tool, root).let: directory =>
      safely(directory / Name[Linux](t"share") / Name[Linux](t"$name.ratel"))

  // The root of `tool`'s project at or above `directory`: the directory holding the nearest
  // `.pyrocosm/<tool>/config.tel`, as the tool itself finds it (`Tool.project`), or `directory`
  // if there is none, which is the root the tool takes when run there.
  def root(tool: Text, directory: Text): Text =
    def recur(directory: Path on Linux): Optional[Path on Linux] =
      val file: Optional[Path on Linux] =
        safely:
          directory / Name[Linux](t".pyrocosm") / Name[Linux](tool) / Name[Linux](t"config.tel")

      if file.lay(false)(Filing.exists(_)) then directory
      else safely(directory.parent).let(recur(_))

    safely(directory.as[Path on Linux]).let(recur(_)).let(_.encode).or(directory)

  // The value hash of an image, which is its identity: two images of one value under one
  // schema have the same hash, whatever their bytes.
  private def identity(hash: Optional[Digest in Blake3], signature: Data): Optional[Text] =
    hash.let(_.serialize[Hex]).let: hex => t"$hex:${Digests.of(signature)}"

  private def identity(image: Ratel): Optional[Text] =
    safely(image.open[Ratel]()(identity(ratel.valueHash, ratel.signature)))

  private def identity(path: Path on Linux): Optional[Text] =
    safely(path.open[Ratel]()(identity(ratel.valueHash, ratel.signature)))

  // Publishes `value` at `path`, as the image of its type: written beside the file and moved
  // onto it, so that a reader sees the last one or this one, whole. A value which is already
  // what is published is not written again, so that no reader maps a file it has. `false` if
  // it could not be published — another process is publishing there, or the file could not be
  // written — and what was published before stays.
  def publish[value: Tel.Encodable](path: Path on Linux, value: value)
    ( using value is stratiform.TelSchematic over Tels.Type )
  :   Boolean =

    safely(value.ratel).lay(false): image =>
      val made: Optional[Text] = identity(image)

      if made.present && Filing.exists(path) && identity(path) == made then true else
        safely:
          path.parent.let: directory =>
            if !Filing.directory(directory) then directory.create[Directory](CreateFlag.Parents)

          path.claim[Ratel of value](_.publish(image))

        . present

  // A reader of the share at `path`, for as long as the process wants it: `read` lends the
  // image as it is now. The file is mapped once, and again only when it has been replaced; it
  // need not exist when the reader is made, and is looked for again on each `read` until it
  // does. A reader may be shared between threads.
  final class Reader(path: Path on Linux, interval: Long = Shares.interval):
    // What holds the file mapped: galilei's observer lives for the scope of `observe`, so a
    // thread stays in that scope, lending the observer out, until the reader is closed.
    private final class Hold(val observer: Observer[Ratel], val release: juc.CountDownLatch)

    @scala.caps.unsafe.untrackedCaptures
    private var hold: Optional[Hold] = Unset

    @scala.caps.unsafe.untrackedCaptures
    private var asked: Optional[Long] = Unset

    @scala.caps.unsafe.untrackedCaptures
    private var closed: Boolean = false

    // Maps the file, if it is there, on a thread which stays within the observer's scope.
    private def attach(): Optional[Hold] =
      val ready: juc.CompletableFuture[Optional[Hold]] = juc.CompletableFuture()
      val release: juc.CountDownLatch = juc.CountDownLatch(1)

      val task: Runnable = new Runnable:
        def run(): Unit =
          try
            safely:
              path.observe[Ratel]: observer =>
                ready.complete(Hold(scala.caps.unsafe.unsafeAssumePure(observer), release))
                release.await()

            ()
          finally
            ready.complete(Unset)
            ()

      val thread: Thread = Thread(task)

      thread.setDaemon(true)
      thread.setName(s"share ${path.encode}")
      thread.start()

      // `Unset` is what a thread which mapped nothing answers with.
      ready.get() match
        case hold: Hold => hold
        case _          => Unset

    // The observer, of the file as it is now. The filesystem is asked once in each interval,
    // whatever the answer: whether the name leads to a newer file, if one is mapped, and
    // whether there is a file yet, if none is — so a share nobody has published costs a reader
    // one `stat` an interval, and no thread.
    private def current(): Optional[Hold] = synchronized:
      if closed then Unset else
        val now: Long = java.lang.System.nanoTime/1000000L

        if asked.lay(true)(now - _ >= interval) then
          asked = now

          if hold.absent then (if Filing.exists(path) then hold = attach())
          else hold.let: held => safely(held.observer.refresh())

        hold

    // Lends the image to `block`, which reads it in place: `ratel` names the reader within it.
    // `Unset` if nothing is published, or what is there cannot be read as an image; a block
    // is given the means to raise an error of the image's, and is `Unset` if it does.
    def read[result](block: (Ratel.Reader^, Tactic[Ratel.Error]) ?=> result): Optional[result] =
      current().let: held => safely(held.observer.open()(block))

    // Whether a file is mapped.
    def attached: Boolean = synchronized(hold.present)

    // Unmaps the file; the reader answers `Unset` from now on.
    def close(): Unit = synchronized:
      closed = true
      hold.let(_.release.countDown())
      hold = Unset
