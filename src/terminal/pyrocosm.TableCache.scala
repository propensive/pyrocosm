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

import scala.caps
import scala.collection.mutable as scm

import java.util as ju

import soundness.{Standing as _, Step as _, Token as _, *}

import dysasymptotics.{linearAccess, linearSize}
import columnAttenuation.ignoreAttenuation

// One table's rendering, kept between repaints: its escritoire layout, and per row its phrased
// cells, its height and — once it has been in view — its lines. Rows are matched by identity
// (`eq`) against the last content, so a host that keeps unchanged row objects stable pays only
// for the rows that changed; a new row is admitted to the layout, and only a row the layout
// cannot accommodate reflows the table. The window a panel shows is rendered from the heights:
// rows above and below it contribute their height and nothing else.
//
// Paint-thread state, like the fixture that holds it; the mutable buffers are what the
// annotation launders.
class TableCache(renderer: TerminalRenderer, columns: List[Block.Column], caption: Optional[List[Inline]]):
  private given Text is Measurable = renderer.measurable
  private given Hyphenation = renderer.hyphenating
  private given TableStyle = renderer.tabling

  private val scaffold: Scaffold[Block.Row, Teletype] = renderer.scaffold(columns)
  private val count: Int = columns.size

  @caps.unsafe.untrackedCaptures
  private var width: Int = -1

  @caps.unsafe.untrackedCaptures
  private var layout: Optional[escritoire.Layout[Block.Row, Teletype]] = Unset

  @caps.unsafe.untrackedCaptures
  private var header: List[Teletype] = Nil

  @caps.unsafe.untrackedCaptures
  private var footer: List[Teletype] = Nil

  @caps.unsafe.untrackedCaptures
  private var rows: scm.ArrayBuffer[Block.Row] = scm.ArrayBuffer()

  @caps.unsafe.untrackedCaptures
  private var cells: scm.ArrayBuffer[escritoire.Cells[Teletype]] = scm.ArrayBuffer()

  // A height of -1 is not yet known; `offsets` are the prefix sums, one more than the rows.
  @caps.unsafe.untrackedCaptures
  private var heights: scm.ArrayBuffer[Int] = scm.ArrayBuffer()

  @caps.unsafe.untrackedCaptures
  private var offsets: scm.ArrayBuffer[Int] = scm.ArrayBuffer(0)

  @caps.unsafe.untrackedCaptures
  private var lines: scm.ArrayBuffer[List[Teletype] | Null] = scm.ArrayBuffer()

  // The row carrying the selection's decoration when its lines were made, or -1.
  @caps.unsafe.untrackedCaptures
  private var highlighted: Int = -1

  // Counts, for tests and diagnostics: rows rendered by `window`, and layouts reflowed.
  @caps.unsafe.untrackedCaptures
  var rendered: Int = 0

  @caps.unsafe.untrackedCaptures
  var reflows: Int = 0

  private def chrome(layout: escritoire.Layout[Block.Row, Teletype]): Unit =
    header = layout.topRule.let(List(_)).or(Nil) + layout.titleLines + List(layout.titleRule)
    footer = layout.bottomRule.let(List(_)).or(Nil) + renderer.caption(caption)

  // Brings the cache up to date with `rows0` at `width0`: cheap when neither changed.
  def update(rows0: List[Block.Row], width0: Int): Unit =
    val width1 = width0.max(4)
    var reflow = false

    if width1 != width then
      width = width1
      // A new width reflows an existing layout; the first layout has nothing to reflow.
      reflow = layout.present
      layout = layout.let(_.resize(width1)).or(scaffold.layout(width1))

    var current: escritoire.Layout[Block.Row, Teletype] = layout.or(scaffold.layout(width1))

    // The previous rows by identity, for a row that moved rather than stayed in place.
    lazy val previous: ju.IdentityHashMap[Block.Row, Int] =
      val map = ju.IdentityHashMap[Block.Row, Int]()
      var index = 0
      while index < rows.length do
        map.put(rows(index), index)
        index += 1
      map

    val rows1: scm.ArrayBuffer[Block.Row] = scm.ArrayBuffer()
    val cells1: scm.ArrayBuffer[escritoire.Cells[Teletype]] = scm.ArrayBuffer()
    val heights1: scm.ArrayBuffer[Int] = scm.ArrayBuffer()
    val lines1: scm.ArrayBuffer[List[Teletype] | Null] = scm.ArrayBuffer()
    var index = 0

    rows0.each: row =>
      val found: Int =
        if index < rows.length && (rows(index) eq row) then index
        else Optional(previous.get(row)).let(_.intValue).or(-1)

      if found >= 0 then
        rows1 += row
        cells1 += cells(found)
        heights1 += heights(found)
        lines1 += lines(found)
      else
        val phrased = current.cells(row)
        val next = current.extend(phrased)

        if !next.eq(current) then
          if !next.stable(current) then reflow = true
          current = next

        rows1 += row
        cells1 += phrased
        heights1 += -1
        lines1 += null

      index += 1

    if reflow then
      reflows += 1
      var i = 0
      while i < heights1.length do
        heights1(i) = -1
        lines1(i) = null
        i += 1

    if reflow || layout.absent then chrome(current)
    layout = current

    var i = 0
    val offsets1: scm.ArrayBuffer[Int] = scm.ArrayBuffer(0)
    while i < heights1.length do
      if heights1(i) < 0 then heights1(i) = current.height(cells1(i))
      offsets1 += offsets1(i) + heights1(i)
      i += 1

    rows = rows1
    cells = cells1
    heights = heights1
    lines = lines1
    offsets = offsets1
    if highlighted >= rows.length then highlighted = -1

  // The table's height in lines, from the heights alone.
  def height: Int = header.size + offsets(offsets.length - 1) + footer.size

  // The table's lines [from, until), rendering only the rows in that window which have none.
  def window(from: Int, until: Int, selected: Optional[Action]): List[Teletype] =
    layout.lay(Nil: List[Teletype]): layout =>
      val chosen: Int = selected.lay(-1): action =>
        var found = -1
        var i = 0
        while found < 0 && i < rows.length do
          if rows(i).action == action then found = i
          i += 1
        found

      if chosen != highlighted then
        if highlighted >= 0 && highlighted < lines.length then lines(highlighted) = null
        if chosen >= 0 then lines(chosen) = null
        highlighted = chosen

      val out: scm.ListBuffer[Teletype] = scm.ListBuffer()
      val headerSize = header.size

      def slice(all: List[Teletype], start: Int): Unit =
        val keepFrom = (from - start).max(0)
        val keepUntil = (until - start)
        if keepUntil > 0 then out ++= all.skip(keepFrom).keep(keepUntil - keepFrom).stdlib

      if from < headerSize then slice(header, 0)

      // The first row at or after `from`, by the offsets.
      val target = from - headerSize
      var low = 0
      var high = rows.length
      while low < high do
        val mid = (low + high)/2
        if offsets(mid + 1) <= target then low = mid + 1 else high = mid

      var i = low
      while i < rows.length && headerSize + offsets(i) < until do
        if lines(i) == null then
          lines(i) = layout.lines(cells(i), renderer.rowDecorations(rows(i), count, i == chosen))
          rendered += 1
        slice(lines(i).nn, headerSize + offsets(i))
        i += 1

      val footerStart = headerSize + offsets(rows.length)
      if footerStart < until then slice(footer, footerStart)

      List.from(out)
