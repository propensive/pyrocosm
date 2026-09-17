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

// Excluded from the umbrella: `Filter` (panopticon), which would outrank this package's own
// definition, since a wildcard import beats a package member declared in another file.
import soundness.{Filter as _, *}

// Which paths of a commit's tree take no part in its fingerprint. Repository-relative globs,
// read from a tool's own `.pyrocosm/<tool>/config.tel` (never from `.gitignore` or
// `.dockerignore`), as a `notes` block of `exclude` and `include` lines. A path is excluded when
// an `exclude` glob matches it and no `include` glob does.
//
// The glob rules are deliberately few: `*` matches within one path segment, `**` matches across
// segments, `?` matches one character, a glob with no `/` in it matches a file or directory of
// that name at any depth, a glob ending in `/` matches a directory and everything beneath it, and
// any glob that matches a directory matches its contents. A leading `/` anchors at the root and
// is otherwise ignored.
case class Filter(exclude: List[Text] = Nil, include: List[Text] = Nil):
  private lazy val exclusions: List[java.util.regex.Pattern] = exclude.map(Filter.pattern(_))
  private lazy val inclusions: List[java.util.regex.Pattern] = include.map(Filter.pattern(_))

  def excludes(path: Text): Boolean =
    exclusions.exists(_.matcher(path.s).nn.matches) && !inclusions.exists(_.matcher(path.s).nn.matches)

  def empty: Boolean = exclude.nil

object Filter:
  val none: Filter = Filter()

  private[pyrocosm] def pattern(glob: Text): java.util.regex.Pattern =
    val anchored = glob.s.startsWith("/")
    val trimmed = glob.s.stripPrefix("/").stripSuffix("/")
    val body = StringBuilder()
    var index = 0

    while index < trimmed.length do
      trimmed.charAt(index) match
        case '*' if trimmed.startsWith("**/", index) => body.append("(?:.*/)?"); index += 2
        case '*' if trimmed.startsWith("**", index)  => body.append(".*"); index += 1
        case '*'                                     => body.append("[^/]*")
        case '?'                                     => body.append("[^/]")
        case char                                    => body.append(java.util.regex.Pattern.quote(char.toString).nn)
      index += 1

    val prefix = if anchored || trimmed.contains("/") then "^" else "^(?:.*/)?"
    java.util.regex.Pattern.compile(prefix + body.toString + "(?:/.*)?$").nn
