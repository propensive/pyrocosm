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
// path once per process and the link stands in for it. Outside the launcher, the context
// loader's URLs and `java.class.path` are what there is.
object Runtimes:
  private lazy val link: Optional[Text] =
    val executable: String | Null = java.lang.System.getProperty("ethereal.script")

    if executable == null then Unset else
      try
        val real: String = ji.File(executable).getCanonicalPath.nn
        val directory: jnf.Path = jnf.Files.createTempDirectory("pyrocosm").nn
        val link: jnf.Path = directory.resolve("pyrocosm.jar").nn
        jnf.Files.createSymbolicLink(link, jnf.Path.of(real))
        link.toString.tt
      catch case _: Throwable => Unset

  private def executable: Optional[Text] =
    val property: String | Null = java.lang.System.getProperty("ethereal.script")

    if property == null then Unset else
      try ji.File(property).getCanonicalPath.nn.tt catch case _: Throwable => property.tt

  // The entries as the loaders name them, in their order, each once, the executable's link
  // standing in for the executable itself.
  def classpath(loader: ClassLoader = Thread.currentThread.nn.getContextClassLoader.nn)
  :   List[Text] =

    val own: Optional[Text] = executable

    def canonical(path: Text): Text =
      try ji.File(path.s).getCanonicalPath.nn.tt catch case _: Throwable => path

    // The loader's URLs, then its parents', since a launcher may load a tool's libraries
    // through a child of the loader it was given.
    def urls(loader: ClassLoader | Null): List[Text] =
      if loader == null then Nil else
        val own: List[Text] = loader match
          case loader: jn.URLClassLoader =>
            List.from(loader.getURLs.nn.iterator.map(_.nn)).bind: url =>
              try List(jnf.Path.of(url.toURI).nn.toString.tt) catch case _: Throwable => Nil

          case _ =>
            Nil

        List.from(own.stdlib ++ urls(loader.getParent).stdlib)

    val system: String | Null = java.lang.System.getProperty("java.class.path")

    val property: List[Text] =
      if system == null || system.isEmpty then Nil
      else system.tt.cut(ji.File.pathSeparator.nn.tt).filter(_ != t"")

    def other(entry: Text): Boolean = own != entry && own != canonical(entry)
    val rest: List[Text] = List.from(urls(loader).stdlib ++ property.stdlib).filter(other(_))
    val distinct: List[Text] = List.from(rest.stdlib.distinct)
    link.lay(distinct): link => link :: distinct

  def isJar(entry: Text): Boolean = entry.ends(t".jar")
