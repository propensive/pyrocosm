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

import java.util.regex as jur

import scala.caps

import soundness.*

// A drawing an application keeps current: a run of SVG markup, identified like a handle, whose
// parts can be revised one at a time. A block holds a figure by reference, so the same figure
// can appear in a panel and go on changing: a frontend showing it binds the figure's revision
// cell and repaints only what changed — one identified part, or the whole drawing when its
// frame moved. Pyrocosm knows nothing of how the markup was drawn; a charting library which
// names its parts (tasseomancy's `series-0`, `legend`, …) is the intended producer.
//
// The markup is carried verbatim, and a web frontend embeds it unescaped: a figure is the
// application's own drawing, not user input.
object Figure:
  // What changed since the figure was last shown: a single part, named by the `id` attribute
  // the markup gives it, or everything.
  enum Revision:
    case Redraw(svg: Text)
    case Replace(part: Text, svg: Text)

  // The serial form: what a figure is at one moment, without its cell.
  case class Snapshot(id: Text, alt: List[Inline], svg: Text)

  def apply(alt: List[Inline], svg: Text): Figure = new Figure(Handles.fresh('f'), alt, svg)

  // A figure read back from its serial form keeps its id, so it compares equal to the original.
  def restore(snapshot: Snapshot): Figure = new Figure(snapshot.id, snapshot.alt, snapshot.svg)

  private val idPattern: jur.Pattern = jur.Pattern.compile("""\bid="([^"]*)"""").nn
  private val urlPattern: jur.Pattern = jur.Pattern.compile("""url\(#([^)]*)\)""").nn
  private val hrefPattern: jur.Pattern = jur.Pattern.compile("""\bhref="#([^"]*)"""").nn

  // The markup with every `id` qualified by the figure's own, and every `url(#…)` and
  // `href="#…"` reference following it, so that several figures whose producer uses the same
  // fixed part names can share one page, and a part can be found by `<figure>-<part>`.
  def namespace(figure: Text, markup: Text): Text =
    val prefix: String = jur.Matcher.quoteReplacement(s"${figure.s}-").nn
    val ids = idPattern.matcher(markup.s).nn.replaceAll(s"""id="$prefix$$1"""").nn
    val urls = urlPattern.matcher(ids).nn.replaceAll(s"""url(#$prefix$$1)""").nn
    hrefPattern.matcher(urls).nn.replaceAll(s"""href="#$prefix$$1"""").nn.tt

  // Every figure a run of blocks holds, in document order, as `Actions.of` finds actions.
  def of(blocks: List[Block]): List[Figure] = blocks.bind { (block: Block) => ofBlock(block) }

  private def ofBlock(block: Block): List[Figure] = block match
    case Block.Figure(figure)            => List(figure)
    case Block.Listing(_, items)         => items.bind { (item: Block.Item) => of(item.content) }
    case Block.Quotation(content)        => of(content)
    case Block.Notice(_, _, content)     => of(content)
    case Block.Disclosure(_, content, _) => of(content)
    case Block.Group(content)            => of(content)
    case Block.Record(entries, _)        => entries.bind { (entry: Block.Entry) => of(entry.value) }
    case _                               => Nil

final class Figure private[pyrocosm] (val id: Text, val alt: List[Inline], svg0: Text):
  @caps.unsafe.untrackedCaptures
  @volatile
  private var current: Text = svg0

  // The latest change, for a frontend to bind; `svg` is always the whole current drawing, for
  // a frontend that has yet to show the figure at all.
  val revision: Live[Optional[Figure.Revision]] = Live(Unset)

  def svg: Text = current
  def snapshot: Figure.Snapshot = Figure.Snapshot(id, alt, current)

  def redraw(svg: Text): Unit =
    current = svg
    revision() = Figure.Revision.Redraw(svg)

  // One part changed: `markup` replaces the element carrying `id="<part>"`; `svg` is the whole
  // drawing after the change, kept for the next fresh render.
  def replace(part: Text, markup: Text, svg: Text): Unit =
    current = svg
    revision() = Figure.Revision.Replace(part, markup)

  override def equals(other: Any): Boolean = other match
    case figure: Figure => figure.id == id && figure.alt == alt && figure.svg == current
    case _              => false

  override def hashCode: Int = id.hashCode
  override def toString: String = s"Figure($id)"
