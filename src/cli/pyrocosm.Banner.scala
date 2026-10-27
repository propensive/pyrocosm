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

import textMetrics.uniformMetric

// The Pyrocosm wordmark that heads `<tool> about`, with the tool's own name beside it, sitting
// on the same baseline, and every character tinted along the diagonal from yellow at the top
// left to red at the bottom right. Spaces are left untinted, so that no escape is spent on them,
// and a terminal without colour shows the wordmark plain.
object Banner:
  private val pyrocosm: scala.List[Text] =
    scala.List
      ( t"▄▄──▄▄",
        t"██  ██ ▄▄ ▄▄ ▄▄─▄▄ ▄▄─▄▄ ▄▄─▄▄ ▄▄─▄▄ ▄▄─▄▄ ▄▄─▄▄─▄▄",
        t"██──▀▀ ██ ██ ██    ██ ██ ██    ██ ██ ▀▀─▄▄ ██ ██ ██",
        t"▀▀     ▀▀─██ ▀▀    ▀▀─▀▀ ▀▀─▀▀ ▀▀─▀▀ ▀▀─▀▀ ▀▀ ▀▀ ▀▀",
        t"       ▀▀─▀▀" )

  // The letters of the tools' names, three rows each, in the wordmark's style.
  private val letters: scala.collection.immutable.Map[Char, scala.List[Text]] =
    scala.collection.immutable.Map
      ( 'a' -> scala.List(t"▄─▄", t"█─█", t"▀ ▀"),
        'e' -> scala.List(t"▄─▄", t"█─ ", t"▀─▀"),
        'f' -> scala.List(t"▄─▄", t"█─ ", t"▀  "),
        'i' -> scala.List(t"▄", t"█", t"▀"),
        'l' -> scala.List(t"▄  ", t"█  ", t"▀─▀"),
        'm' -> scala.List(t"▄─▄─▄", t"█ █ █", t"▀ ▀ ▀"),
        'r' -> scala.List(t"▄─▄", t"█▄▀", t"▀ ▀"),
        'u' -> scala.List(t"▄ ▄", t"█ █", t"▀─▀"),
        'v' -> scala.List(t"▄ ▄", t"█ █", t" ▀ "),
        'y' -> scala.List(t"▄ ▄", t"▀─█", t"▀─▀") )

  // `name` in the letters above, a space between each; `Unset` if it has a letter there is no
  // art for.
  private def word(name: Text): Optional[scala.List[Text]] =
    val chars: scala.List[Char] = scala.List.tabulate(name.s.length)(name.s.charAt(_).toLower)

    val glyphs: scala.List[Optional[scala.List[Text]]] =
      chars.map(letters.get(_).fold(Unset)(identity))

    if glyphs.isEmpty || glyphs.exists(_.absent) then Unset else
      val known: scala.List[scala.List[Text]] = glyphs.map(_.or(scala.Nil))

      scala.List.tabulate(3): row => known.map(_(row)).reduce(spaced)

  private def spaced(left: Text, right: Text): Text = t"$left $right"

  // The wordmark, and below it the tool's name, its top row on the wordmark's last, eight
  // columns to the right of the `y`'s tail; just the wordmark if the name cannot be spelled.
  private def art(name: Text): scala.List[Text] =
    word(name).lay(pyrocosm): word =>
      val tail: Text = pyrocosm.last
      val indent: Text = t"".pad(tail.s.length + 8)

      val first: Text = t"${tail.pad(tail.s.length + 8)}${word.head}"

      val rest: scala.List[Text] = word.tail.map: row => t"$indent$row"

      pyrocosm.init ::: first :: rest

  private val yellow: Int = 0xffd700
  private val red: Int = 0xe01b24

  // The colour `fraction` of the way from yellow to red, each channel blended linearly: both
  // ends are rich in red, so the blend passes through orange, never through a muddy brown.
  private def blend(fraction: Double): Chroma =
    def channel(shift: Int): Int =
      val from: Int = (yellow >> shift) & 0xff
      val to: Int = (red >> shift) & 0xff
      (from + (to - from)*fraction).round.toInt << shift

    Chroma(channel(16) | channel(8) | channel(0))

  // The art's characters tinted along the diagonal: how far each is from the top left, as rows
  // and columns travelled, each scaled to the art's height and width, over how far the furthest
  // character (not space) is, since the art's bottom-right corner may be blank.
  def lines(name: Text): List[Teletype] =
    val art: scala.List[Text] = Banner.art(name)
    val width: Int = art.map(_.s.length).max

    def distance(row: Int, column: Int): Double =
      column.toDouble/(width - 1) + row.toDouble/(art.length - 1)

    val furthest: Double =
      art.zipWithIndex.flatMap: (line, row) =>
        (0 until line.s.length).filter(line.s.charAt(_) != ' ').map(distance(row, _))

      . max

    val lines: scala.List[Teletype] = art.zipWithIndex.map: (line, row) =>
      scala.List.tabulate(line.s.length)(line.s.charAt(_)).zipWithIndex.foldLeft(e""):
        case (teletype, (char, column)) =>
          val glyph: Teletype = e"${char.show}"

          if char == ' ' then teletype.append(glyph)
          else teletype.append(e"${Fg(blend(distance(row, column)/furthest))}($glyph)")

    List(lines*)
