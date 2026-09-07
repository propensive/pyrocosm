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

import anticipation.*
import gossamer.*
import vacuous.*

// Numbers as a person reads them, shared by every renderer so that a figure or an amount is the
// same text in the terminal and on the web.
object Amounts:
  // Three significant figures or so, unless a precision is asked for.
  def figure(value: Double, precision: Optional[Int] = Unset): Text =
    val decimals: Int = precision.or:
      if value == value.toLong.toDouble then 0
      else if value.abs >= 100 then 0
      else if value.abs >= 10 then 1
      else 2

    java.lang.String.format(java.util.Locale.ROOT, s"%.${decimals}f", java.lang.Double.valueOf(value)).nn.tt

  // An amount in base units, scaled to the unit a reader would choose: seconds down to
  // nanoseconds, bytes up to gigabytes; anything else is shown as it came.
  def scaled(value: Double, units: Text): (Text, Text) =
    val absolute = value.abs

    def step(divisor: Double, unit: Text): (Text, Text) = (figure(value/divisor), unit)

    units.s match
      case "s" =>
        if absolute == 0.0 then (t"0", t"s")
        else if absolute < 1e-6 then step(1e-9, t"ns")
        else if absolute < 1e-3 then step(1e-6, t"µs")
        else if absolute < 1.0 then step(1e-3, t"ms")
        else if absolute < 60.0 then step(1.0, t"s")
        else if absolute < 3600.0 then step(60.0, t"min")
        else step(3600.0, t"h")

      case "B" =>
        if absolute < 1e3 then step(1.0, t"B")
        else if absolute < 1e6 then step(1e3, t"kB")
        else if absolute < 1e9 then step(1e6, t"MB")
        else step(1e9, t"GB")

      case _ =>
        (figure(value), units)
