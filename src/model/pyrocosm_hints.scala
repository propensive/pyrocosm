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
package pyrocosm.hints

import pyrocosm.*

// Medium-neutral hints, meaningful to any renderer.
case class Proportion(fraction: Double) extends Hint             // the share of space a panel would like
case class Minimum(columns: Int, rows: Int) extends Hint         // below which the panel is not worth showing
case class Beside(panel: Panel.Id) extends Hint                  // prefer adjacency to another panel
case class Below(panel: Panel.Id) extends Hint
case object Compact extends Hint                                 // tighter spacing
case object Follow extends Hint                                  // keep the newest content in view

// Hints only the terminal renderer understands.
object terminal:
  enum Occupancy extends Hint:
    case Inline, Fullscreen

  // `Rules`: no sides, a low line (`⎽`) above and a high line (`‾`) below, as a transcript's
  // entries keep of the prompt they were entered at (`Block.Rule` with an edge).
  enum Border extends Hint:
    case None, Light, Heavy, Rounded, Rules

  case class MaxRows(rows: Int) extends Hint

// Hints only the web renderer understands.
object web:
  case class Span(columns: Int) extends Hint                     // grid columns a card may occupy
  case object Card extends Hint
  case object Sticky extends Hint
  enum Side extends Hint:
    case Verso, Recto
  case class Icon(name: anticipation.Text) extends Hint
  case object Collapsed extends Hint
