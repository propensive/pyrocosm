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

import java.io as ji
import java.net as jn
import java.nio.file as jnf

import soundness.*

// The classpath of the running process, as a tool that compiles against itself (a REPL, a
// linter, a script runner) needs it. Under the Burdock/xek launcher the process was loaded
// from the externalized dependency jars in `~/.cache/burdock` plus the executable itself — a
// shebang-prefixed file with no `.jar` suffix, which the compiler refuses to read, though a
// `ZipFile` reads the archive past the shebang — so the executable is symlinked to a `.jar`
// path in the cache and the link stands in for it. Outside the launcher, the context
// loader's URLs and `java.class.path` are what there is.
object Runtimes:
  private def property(name: String): Optional[Text] =
    val value: String | Null = java.lang.System.getProperty(name)
    if value == null || value.isEmpty then Unset else value.nn.tt

  // The executable the launcher ran, canonical, when it set `ethereal.script`.
  private def executable: Optional[Text] = property("ethereal.script").let: script =>
    try ji.File(script.s).getCanonicalPath.nn.tt catch case _: Throwable => script

  // The executable symlinked to a `.jar` path; `Unset` without a launcher, or where the link
  // cannot be made. The link must outlive this process — a hub started from it goes on running,
  // and loading classes through it lazily, after the daemon which started it has exited — so it
  // is not scratch, but stands at a stable path in the cache,
  // `$XDG_CACHE_HOME/pyrocosm/launchers/<digest of the executable's path>.jar`, which every
  // process of that executable shares. It is made if it is absent, or replaced if it points
  // elsewhere, by a link made under a unique name and renamed onto it, so that two processes
  // starting at once each find a whole, correct link there.
  private lazy val link: Optional[Text] = executable.let: real =>
    import environments.javaBaseEnvironment
    import systems.javaBaseSystem

    safely(Xdg.cacheHome[Path on Linux]).let: cache =>
      try
        val target: jnf.Path = jnf.Path.of(real.s).nn
        val directory: jnf.Path = jnf.Path.of(cache.encode.s, "pyrocosm", "launchers").nn
        val link: jnf.Path = directory.resolve(s"${Digests.of(real).keep(16)}.jar").nn

        def correct: Boolean =
          jnf.Files.isSymbolicLink(link) && jnf.Files.readSymbolicLink(link) == target

        if !correct then
          jnf.Files.createDirectories(directory)
          val staged: jnf.Path = directory.resolve(s".${java.util.UUID.randomUUID}.jar").nn

          try
            jnf.Files.createSymbolicLink(staged, target)
            jnf.Files.move(staged, link, jnf.StandardCopyOption.ATOMIC_MOVE)
          finally jnf.Files.deleteIfExists(staged)

        link.toString.tt
      catch case _: Throwable => Unset

  // The entries as the loaders name them — `loader`'s URLs, then its parents', since a
  // launcher may load a tool's libraries through a child of the loader it was given — and
  // then `java.class.path`, in their order, each once. An entry of `java.class.path` is made
  // absolute, as a loader's URLs are, since a JVM started with `-cp out.jar` names it relative to
  // a directory which a process this one starts need not share.
  def classpath(loader: ClassLoader = Thread.currentThread.nn.getContextClassLoader.nn)
  :   List[Text] =

    def urls(loader: ClassLoader | Null): List[Text] =
      if loader == null then Nil else
        val own: List[Text] = loader match
          case loader: jn.URLClassLoader =>
            Iterator.array(loader.getURLs.nn).to[List].bind: url =>
              try List(jnf.Path.of(url.nn.toURI).nn.toString.tt) catch case _: Throwable => Nil

          case _ =>
            Nil

        List.from(own.stdlib ++ urls(loader.getParent).stdlib)

    val system: List[Text] =
      property("java.class.path").lay(Nil): path =>
        path.cut(ji.File.pathSeparator.nn.tt).filter(_ != t"").map: entry =>
          ji.File(entry.s).getAbsolutePath.nn.tt

    List.from((urls(loader).stdlib ++ system.stdlib).distinct)

  // The same, for a compiler to read: the executable, which a compiler refuses (a
  // shebang-prefixed file with no `.jar` suffix, though a `ZipFile` reads the archive past the
  // shebang), stands as its `.jar` link, in its place or first.
  def compilable(loader: ClassLoader = Thread.currentThread.nn.getContextClassLoader.nn)
  :   List[Text] =

    val entries: List[Text] = classpath(loader)
    val own: Optional[Text] = executable

    def canonical(path: Text): Text =
      try ji.File(path.s).getCanonicalPath.nn.tt catch case _: Throwable => path

    link.lay(entries): link =>
      def self(entry: Text): Boolean = own == entry || own == canonical(entry)
      def stand(entry: Text): Text = if self(entry) then link else entry
      val replaced: List[Text] = entries.map(stand(_))
      if replaced.has(link) then replaced else link :: replaced

  def isJar(entry: Text): Boolean = entry.ends(t".jar")
