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

// The colours code is set in, wherever it is drawn: the ground of a block of code, the text
// upon it, and a colour for each accent. A web theme and a terminal theme which take the same
// one show the same code in the same colours, which is the point of its being defined here,
// beneath both, and in the model's own colour, which each medium takes as its own.
trait CodeTheme:
  def background: Block.Rgb
  def foreground: Block.Rgb
  def accent(accent: Token.Accent): Block.Rgb

object CodeTheme:
  // The Zed colours Flame's terminal highlights with, on a deep plum.
  object Ember extends CodeTheme:
    private def rgb(packed: Int): Block.Rgb =
      Block.Rgb((packed >> 16) & 0xff, (packed >> 8) & 0xff, packed & 0xff)

    private val plum = rgb(0x1b0121)
    private val text = rgb(0xd4be98)
    private val keyword = rgb(0xff6633)
    private val string = rgb(0x99ffff)
    private val number = rgb(0xcc3366)
    private val term = rgb(0xffcc99)
    private val typal = rgb(0x00cc99)
    private val comment = rgb(0x9fb83a)
    private val symbol = rgb(0xcc6699)
    private val parens = rgb(0xf28534)
    private val error = rgb(0xea6962)

    def background: Block.Rgb = plum
    def foreground: Block.Rgb = text

    def accent(accent: Token.Accent): Block.Rgb = accent match
      case Token.Accent.Keyword    => keyword
      case Token.Accent.Modifier   => keyword
      case Token.Accent.Command    => keyword
      case Token.Accent.Comment    => comment
      case Token.Accent.Substitute => text
      case Token.Accent.String     => string
      case Token.Accent.Number     => number
      case Token.Accent.Term       => term
      case Token.Accent.Typal      => typal
      case Token.Accent.Symbol     => symbol
      case Token.Accent.Parens     => parens
      case Token.Accent.Error      => error
      case Token.Accent.Unparsed   => text
