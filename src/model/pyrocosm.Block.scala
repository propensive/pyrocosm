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

import acyclicity.*
import anticipation.*
import vacuous.*

// Flow content: the things which occur one after another down a page or a pane. Comparable to
// Markdown's block nodes, and richer: tables with layout semantics, records, notices, trees,
// graphs, charts and gauges. Every block is data; nothing here says how it looks.
object Block:
  // How a table column may give way when the table is squeezed, mirroring escritoire`s
  // `Columnar` strategies on the terminal; the web maps them to `1fr`, `max-content` and
  // container queries.
  enum Sizing:
    case Stretch                                        // absorbs the spare width; at most one per table
    case Rigid                                          // never narrower than its content: figures
    case Paragraph                                      // wraps
    case Collapsible(priority: Double)                  // vanishes below a threshold; higher goes first

  enum Alignment:
    case Start, End, Center, Justify

  case class Column
    ( title:     List[Inline],
      alignment: Alignment = Alignment.Start,
      sizing:    Sizing    = Sizing.Paragraph,
      numeric:   Boolean   = false )

  // A row with an `Action` is selectable: a click on the web, focus and Enter in the terminal.
  // This is how master/detail interfaces are built, with no layout-level machinery.
  case class Row
    ( cells:  List[List[Inline]],
      tone:   Optional[Tone]   = Unset,
      action: Optional[Action] = Unset )

  case class Item(content: List[Block], action: Optional[Action] = Unset)

  case class Entry(key: List[Inline], value: List[Block])

  // A range of one line of a code block to be marked: an error, a caution, a highlight or a
  // parameter, with an optional caption — fluence's annotated samples.
  object Note:
    enum Style:
      case Erroneous, Caution, Highlight, Param

  case class Note(line: Int, start: Int, end: Int, style: Note.Style, caption: Optional[Text] = Unset)

  case class TreeNode
    ( label:    List[Inline],
      children: List[TreeNode]   = Nil,
      tone:     Optional[Tone]   = Unset,
      action:   Optional[Action] = Unset )

  // A node of a graph, identified by `id` so that a `Dag` of vertices has structural identity.
  case class Vertex
    ( id:     Text,
      label:  List[Inline],
      tone:   Optional[Tone]   = Unset,
      action: Optional[Action] = Unset )

  object Chart:
    enum Kind:
      case Sparkline, Bars, Histogram

  case class Series(label: List[Inline], values: List[Double], tone: Optional[Tone] = Unset)

  def paragraph(text: Text): Block = Paragraph(Inline.text(text))

enum Block:
  case Paragraph(content: List[Inline])
  case Heading(level: Int, content: List[Inline])
  case Listing(ordered: Boolean, items: List[Block.Item])
  case Quotation(content: List[Block])
  case Rule
  case Code(language: Language, lines: List[List[Token]], notes: List[Block.Note] = Nil)
  case Table(columns: List[Block.Column], rows: List[Block.Row], caption: Optional[List[Inline]] = Unset)
  case Record(entries: List[Block.Entry], title: Optional[List[Inline]] = Unset)
  case Notice(tone: Tone, title: Optional[List[Inline]], content: List[Block])
  case Disclosure(summary: List[Inline], content: List[Block], open: Boolean = false)
  case Image(source: Text, alt: Text)
  case Tree(roots: List[Block.TreeNode])
  case Graph(dag: Dag[Block.Vertex])
  case Chart(kind: Block.Chart.Kind, series: List[Block.Series])
  case Gauge(status: Status, caption: Optional[List[Inline]] = Unset)
  case Group(content: List[Block])                      // a run of blocks which belong together
