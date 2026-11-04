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

import java.nio.file as jnf

import soundness.*

import charsets.utf8Charset
import filesystemBackends.javaBaseFilesystem
import filesystemOptions.{deleteRecursively, dereferenceSymlinks}
import filesystemTraversal.preOrderTraversal
import logging.silentLogging
import textSanitizers.skipSanitizer

// The handful of filesystem operations every tool writes for itself, each the one place it is
// written: a file replaced whole (authored beside the target and moved onto it, so a reader
// never sees half of one), a file's text, the regular files beneath a directory, a directory
// emptied, a scratch directory for the length of a block, and a path's canonical form.
// `temporary` and `canonical` are the two operations galilei does not yet offer; each is a
// Soundness issue, and will go when it lands.
object Filing:
  // Replaces `path` with `data`, creating its parents; `false` if it could not be written.
  def replace(path: Path on Linux, data: Data): Boolean =
    def write(handle: (Handle & Granting[Grant.Read & Grant.Write])^)(using Emit[Truncation.Error])
    :   Unit =

      handle.write(data)

    safely(path.create[File](CreateFlag.Replace, CreateFlag.Parents) { handle ?=> write(handle) })
    . present

  def replace(path: Path on Linux, text: Text): Boolean = replace(path, text.sysData)

  // The whole text of `path`, or `Unset` if it is absent or unreadable.
  def text(path: Path on Linux): Optional[Text] = safely(path.read[Text])

  def data(path: Path on Linux): Optional[Data] = safely(path.read[Data])

  // The regular files beneath `directory`, in pre-order; nothing for a path that is not one.
  def beneath(directory: Path on Linux): List[Path on Linux] =
    safely(directory.descendants.to[List].filter(_.entry() != Directory)).or(Nil)

  // Deletes `path` and everything beneath it, if it exists.
  def wipe(path: Path on Linux): Unit = safely(path.wipe())

  // Whether `path` exists.
  def exists(path: Path on Linux): Boolean = safely(path.existent()).or(false)

  // Whether `path` is a directory.
  def directory(path: Path on Linux): Boolean =
    safely(path.entry() == Directory).or(false)

  // A fresh, empty directory for the duration of `block`, deleted afterwards.
  def temporary[result](prefix: Text)(block: Path on Linux => result): result =
    val directory: jnf.Path = jnf.Files.createTempDirectory(prefix.s).nn
    val path: Path on Linux = unsafely(directory.toString.tt.as[Path on Linux])

    try block(path) finally wipe(path)

  // `path` with `root/` removed from its front, if it is beneath it: how a tool names a file
  // of a project to its user.
  def relative(root: Text, path: Text): Text =
    if path.starts(t"$root/") then path.skip(root.length + 1) else path

  // The canonical form of `path`: symlinks resolved, as `realpath`, or `path` itself if it
  // does not exist.
  def canonical(path: Text): Text =
    try jnf.Path.of(path.s).nn.toRealPath().nn.toString.tt catch case _: Exception => path
