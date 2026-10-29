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

object Purview:
  case class Entry(path: Text, label: Text)

  private def toggle(entries: List[Entry], entry: Entry): List[Entry] =
    if entries.exists(_.path == entry.path) then entries.filter(_.path != entry.path)
    else entries + List(entry)

// A reader's purview — what they have brought into scope and set aside: the wildcard imports
// a page is read under, and the pages bookmarked. Each is a local destination's path — a
// package's page is the package, so importing it is importing its members — with the label it
// is listed by. A qualified name whose prefix names an imported page is shown from the segment
// after it, so an application writes every name in full and the renderer, given the purview,
// elides what is in scope. An interface which uses it declares a starting purview
// (`Interface.purview`); the web frontend keeps a reader's own in a cookie.
case class Purview(imports: List[Purview.Entry] = Nil, bookmarks: List[Purview.Entry] = Nil):
  def imported(path: Text): Boolean = imports.exists(_.path == path)
  def bookmarked(path: Text): Boolean = bookmarks.exists(_.path == path)

  // The entry is removed if its path is present, else added at the end, so a list keeps the
  // order the reader made it in.
  def toggleImport(entry: Purview.Entry): Purview =
    copy(imports = Purview.toggle(imports, entry))

  def toggleBookmark(entry: Purview.Entry): Purview =
    copy(bookmarks = Purview.toggle(bookmarks, entry))

  // The tokens with every imported prefix elided: in each run of chained tokens, the last
  // linked segment before the final one whose page is imported, and everything before it and
  // the separators after it, go; what remains begins a run of its own. A run whose segments are
  // not imported, or which has no links, is as it was.
  def elide(tokens: List[Token]): List[Token] =
    if imports.nil then tokens else
      var done: List[Token] = Nil
      var run: List[Token] = Nil

      def imported(token: Token): Boolean = token.link match
        case Inline.Destination.Local(path, _) => this.imported(path)
        case _                                 => false

      def flush(): Unit =
        val tokens: List[Token] = run.reverse

        // The last linked segment, and the last imported one before it.
        var last: Int = -1
        var cut: Int = -1

        tokens.indexed.each: (token: Token, index: Ordinal) =>
          if token.link.present then last = index.n0

        tokens.indexed.each: (token: Token, index: Ordinal) =>
          if token.link.present && index.n0 < last && imported(token) then cut = index.n0

        val kept: List[Token] =
          if cut < 0 then tokens else
            var skipping: Boolean = true
            var first: Boolean = true

            tokens.indexed.bind: (token: Token, index: Ordinal) =>
              if index.n0 <= cut || (skipping && token.link.absent) then Nil
              else
                skipping = false

                if first then
                  first = false
                  List(token.copy(chained = false))
                else
                  List(token)

        done = kept.reverse + done
        run = Nil

      tokens.each: (token: Token) =>
        if !token.chained then flush()
        run = token :: run

      flush()
      done.reverse
