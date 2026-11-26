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

// Excluded from the umbrella: `Channel` (anticipation), `Control` (coaxial), `Glyph` (phoenicia)
// and `Language` (cosmopolite), which would outrank this package's own definitions, since a
// wildcard import beats a package member declared in another file.
import soundness.{Channel as _, Control as _, Filter as _, Glyph as _, Language as _, Origin as _,
  Status as _, *}

import codepages.utf8Codepage
import discriminables.jsonByKindDiscriminable
import dysasymptotics.{linearAccess, linearSize}
import errorDiagnostics.emptyDiagnostics
import formatting.compactJsonFormatting
import formatting.indentedCssFormatting
import harlequin.Scala
import logging.silentLogging
import palettes.solarizedDarkGaugePalette
import strategies.throwUnsafely
import tableStyles.thickTableStyle
import termcapDefinitions.xtermTrueColorTermcap
import textMetrics.uniformMetric

// An item of a listing with a note: its name, and beneath it what else there is to say of it.
object ItemTests extends Suite(m"Pyrocosm item tests"):
  def run(): Unit =
    val open = Action(t"open")
    val more = Action(t"more")

    val note: List[Inline] =
      List
        ( Inline.Textual(t"5 minutes ago · "),
          Inline.Link(Inline.Destination.Internal(more), Inline.text(t"more")) )

    val noted: Block.Item = Block.Item(List(Block.paragraph(t"fume")), open, note = note)
    val plain: Block.Item = Block.Item(List(Block.paragraph(t"flare")))
    val listing: Block = Block.Listing(false, List(noted, plain))

    test(m"an item's note is a paragraph of its own after its content, on the web"):
      val html: Text = HtmlRenderer().blocks(List(listing)).show

      ( html.contains(t"""<p>fume</p><p class="pyro-item-note">5 minutes ago · """),
        html.contains(t"""<li><p>flare</p></li>""") )

    . assert(_ == (true, true))

    test(m"an item's note is a line beneath its content, within its indent, in the terminal"):
      TerminalRenderer().blocks(List(listing), 40).map(_.plain)

    . assert(_ == List(t"• fume", t"  5 minutes ago · more", t"• flare"))

    test(m"a link in an item's note is an action its blocks lead to"):
      (Actions.of(List(listing)), Actions.linked(List(listing)))

    . assert(_ == (List(open), List(more)))

    test(m"an item with a note is the same item read back from TEL"):
      listing.in[Tel].show.read[Tel].as[Block]

    . assert(_ == listing)
