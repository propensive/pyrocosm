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

// The classpath as a tool that compiles — a script runner, a REPL, a documentation tool —
// names it: an entry from a path, by its suffix; a path from an entry; the running process's
// own entries, through hellenism; and the options every such compile passes the fork, which
// are the ones Pyrocosm and the tools are themselves built with, minus a build's own choices
// of warnings and checks, which code written at a prompt should not have to satisfy.
object Classpaths:
  def entry(path: Text): Classpath.Entry =
    if path.ends(t".jar") then Classpath.Entry.Jar(path) else Classpath.Entry.Directory(path)

  def path(entry: Classpath.Entry): Optional[Text] = entry match
    case Classpath.Entry.Jar(path)       => path
    case Classpath.Entry.Directory(path) => path
    case _                               => Unset

  // The process's classpath, from its own entries (the loader's, or `java.class.path`), with
  // `entries` after them.
  def local(entries: List[Text] = Nil)(using System): LocalClasspath =
    val own: LocalClasspath = LocalClasspath.of(Classloader.threadContext)

    val extra: List[Classpath.Entry.Directory | Classpath.Entry.Jar] = entries.map: entry =>
      if entry.ends(t".jar") then Classpath.Entry.Jar(entry) else Classpath.Entry.Directory(entry)

    LocalClasspath((own.entries + extra)*)

  // The fork's repairs, which reading Soundness's TASTy needs, and the language features
  // Soundness idioms use.
  val houseOptions: List[Text] = List(
    t"-Zalias-captures",
    t"-Zdiagnostic-givens",
    t"-Zgiven-prefixes",
    t"-Zinline-source-maps",
    t"-Zopaque-mutability",
    t"-Zpure-iarrays",
    t"-Zretains-bounds",
    t"-Zretains-skolems",
    t"-Zspreadable-varargs",
    t"-Zunboxed-pure-types",
    t"-Zunion-captures",
    t"-experimental",
    t"-new-syntax",
    t"-feature",
    t"-preview",
    t"-deprecation",
    t"-Xmax-inlines", t"32",
    t"-Yno-flexible-types",
    t"-Yexplicit-nulls",
    t"-language:experimental.modularity",
    t"-language:experimental.dependent",
    t"-language:experimental.relaxedLambdaSyntax",
    t"-language:experimental.genericNumberLiterals",
    t"-language:experimental.into",
    t"-language:experimental.erasedDefinitions",
    t"-language:implicitConversions",
    t"-language:experimental.saferExceptions",
    t"-language:experimental.strictEqualityPatternMatching",
    t"-language:experimental.subCases",
    t"-language:experimental.multiSpreads")

  // The predef options: proscenium's, with `packages` imported after it.
  def imports(packages: Text*): List[Text] =
    val all: List[Text] = List(t"java.lang", t"proscenium") + List(packages*)
    List(t"-Yno-predef", t"-Yimports:${all.join(t",")}")
