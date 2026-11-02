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

// Lengths of time as every tool reads and shows them: `--expires 30m`, `--budget 1.5h`, a
// script's run time, a daemon's uptime, the age of a record. One grammar — a number, whole or
// decimal, and a unit, `ms`, `s`, `m`, `h` or `d`, with a bare number meaning seconds — and
// one rendering: the coarsest non-zero unit first, down to seconds (`5s`, `2m 5s`, `1h 0m 5s`,
// `2d 1h 0m 5s`), or milliseconds below a second (`420ms`). Relative times within the hour are
// aviation's English ("just now", "8 minutes ago"); beyond it, the clock time today, the day
// and month this year, and the full date otherwise.
object Timing:
  private val units: List[(Text, Double)] =
    List(t"ms" -> 0.001, t"s" -> 1.0, t"m" -> 60.0, t"h" -> 3600.0, t"d" -> 86400.0)

  // The duration `text` names, or `Unset` if it is zero, negative or not of the grammar.
  def parse(text: Text): Optional[Duration] =
    val trimmed: Text = text.trim

    def suffixed(entry: (Text, Double)): Boolean = trimmed.ends(entry(0))

    val unit: Optional[(Text, Double)] = units.filter(suffixed(_)).head
    val number: Text = unit.lay(trimmed) { (suffix, _) => trimmed.keep(trimmed.length - suffix.length) }
    val factor: Double = unit.lay(1.0)(_(1))

    safely(number.trim.as[Double]).let: count =>
      if count > 0.0 then (count*factor)*Second else Unset

  // The duration as the number of milliseconds it covers.
  def millis(duration: Duration): Long = (duration.value*1000.0).toLong

  def show(duration: Duration): Text = show(millis(duration))

  // `millis`, rendered: the coarsest unit present first, and every unit down to seconds after
  // it; `420ms` below a second.
  def show(millis: Long): Text =
    if millis < 1000L then t"${millis.max(0L).show}ms" else
      val seconds: Long = millis/1000L
      val days: Long = seconds/86400L
      val hours: Long = seconds%86400L/3600L
      val minutes: Long = seconds%3600L/60L
      val rest: Long = seconds%60L

      if days > 0 then t"${days.show}d ${hours.show}h ${minutes.show}m ${rest.show}s"
      else if hours > 0 then t"${hours.show}h ${minutes.show}m ${rest.show}s"
      else if minutes > 0 then t"${minutes.show}m ${rest.show}s"
      else t"${rest.show}s"

  // How long ago `instant` was, relative to `now`: within the hour in words; today by its
  // clock time; this year by day and month; and otherwise the date.
  def ago(instant: Instant over Unix, now: Instant over Unix = aviation.now()): Text =
    val elapsed: Long = (now.long - instant.long).max(0L)

    if elapsed < 3600000L then
      val seconds: Long = elapsed/1000L
      val minutes: Int = (seconds/60L).toInt

      val span: Timespan =
        if minutes > 0 then Timespan(minutes = -minutes)
        else Timespan(seconds = Quantity(-(seconds.toDouble)))

      aviation.Vernacular.english.relativeTimespan(span)
    else
      val then0: java.time.ZonedDateTime =
        java.time.Instant.ofEpochMilli(instant.long).nn.atZone(java.time.ZoneId.systemDefault).nn

      val today: java.time.ZonedDateTime =
        java.time.Instant.ofEpochMilli(now.long).nn.atZone(java.time.ZoneId.systemDefault).nn

      val pattern: String =
        if then0.toLocalDate == today.toLocalDate then "HH:mm"
        else if then0.getYear == today.getYear then "d MMM"
        else "d MMM yyyy"

      then0.format(java.time.format.DateTimeFormatter.ofPattern(pattern)).nn.tt
