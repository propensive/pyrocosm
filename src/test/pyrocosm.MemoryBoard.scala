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

// A board in memory: the plain characters put on it, by cell, and where the caret was left.
class MemoryBoard(val width: Int, val height: Int) extends profanity.Board:
  val cells: java.util.HashMap[Int, Char] = java.util.HashMap()
  var column: Int = 0
  var row: Int = 0
  var caret: (Int, Int) = (0, 0)

  def move(column: Ordinal, row: Ordinal): Unit =
    this.column = column.n0
    this.row = row.n0

  def put(text: Text): Unit = text.s.foreach: char =>
    if char == '\n' then
      column = 0
      row += 1
    else
      if row < height && column < width then cells.put(row*width + column, char)
      column += 1

  def put(text: Teletype): Unit = put(text.plain)

  def clear(): Unit =
    cells.clear()
    column = 0
    row = 0

  def clearLine(): Unit = ()
  def cursor(visible: Boolean): Unit = ()
  def showCaret(column: Ordinal, row: Ordinal): Unit = caret = (column.n0, row.n0)
  def flush(): Unit = ()

  def line(row: Int): Text =
    val builder = java.lang.StringBuilder()

    (0 until width).foreach: column =>
      builder.append(cells.getOrDefault(row*width + column, ' '))

    builder.toString.tt
