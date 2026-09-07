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
import cataclysm.*
import gossamer.*
import rudiments.*
import spectacular.*
import symbolism.*
import turbulence.*

import contingency.strategies.throwUnsafely
import denominative.dysasymptotics.linearSize
import fulminate.errorDiagnostics.emptyDiagnostics

// The web's colours by role: the same solarized values the terminal theme uses, as the seed of
// one palette for both media. Emitted once as CSS custom properties (`variables`), so every rule
// in `WebStyles` refers to a role by `var(--pyro-…)` and a second theme is a second `:root` block.
trait WebTheme:
  def background: Chroma
  def surface: Chroma       // cards and panels
  def foreground: Chroma
  def muted: Chroma
  def border: Chroma
  def tone(tone: Tone): Chroma
  def accent(accent: Token.Accent): Chroma
  def key: Chroma
  def reference: Chroma
  def figure: Chroma
  def units: Chroma
  def link: Chroma
  def selection: Chroma

  // The `:root` block. Cataclysm's `css` interpolator checks a substituted value against the
  // property's grammar and has no notion of a custom property, so the block is read from text
  // at runtime, where a `--name` declaration is accepted with any value.
  def variables: Css =
    val roles: List[(Text, Chroma)] =
      List
        ( t"bg" -> background, t"surface" -> surface, t"fg" -> foreground, t"muted" -> muted,
          t"border" -> border, t"key" -> key, t"reference" -> reference, t"figure" -> figure,
          t"units" -> units, t"link" -> link, t"selection" -> selection )

    val tones: List[(Text, Chroma)] =
      Tone.values.foldLeft(Nil: List[(Text, Chroma)]) { (acc, tone0) => acc :+ (t"tone-${tone0.toString.tt.lower}" -> tone(tone0)) }

    val accents: List[(Text, Chroma)] =
      Token.Accent.values.foldLeft(Nil: List[(Text, Chroma)]) { (acc, accent0) => acc :+ (t"accent-${accent0.toString.tt.lower}" -> accent(accent0)) }

    val declarations: Text =
      (roles + tones + accents).map { (name, chroma) => t"--pyro-$name: ${WebTheme.hex(chroma)}" }.join(t"; ")

    t":root { $declarations }".read[Css]

object WebTheme:
  given default: WebTheme = SolarizedDark

  def hex(chroma: Chroma): Text =
    def channel(value: Int): Text =
      val digits = java.lang.Integer.toHexString(value&255).nn
      (if digits.length == 1 then "0"+digits else digits).tt

    t"#${channel(chroma.red)}${channel(chroma.green)}${channel(chroma.blue)}"

  object SolarizedDark extends WebTheme:
    private val base03 = Chroma(0x002b36)
    private val base02 = Chroma(0x073642)
    private val base01 = Chroma(0x586e75)
    private val base0 = Chroma(0x93a1a1)
    private val yellow = Chroma(0xb58900)
    private val orange = Chroma(0xcb4b16)
    private val red = Chroma(0xdc322f)
    private val magenta = Chroma(0xd33682)
    private val violet = Chroma(0x6c71c4)
    private val blue = Chroma(0x268bd2)
    private val cyan = Chroma(0x2aa198)
    private val green = Chroma(0x859900)

    def background: Chroma = base03
    def surface: Chroma = base02
    def foreground: Chroma = base0
    def muted: Chroma = base01
    def border: Chroma = base01
    def key: Chroma = yellow
    def reference: Chroma = blue
    def figure: Chroma = base0
    def units: Chroma = cyan
    def link: Chroma = blue
    def selection: Chroma = Chroma(0x0b4a5c)

    def tone(tone: Tone): Chroma = tone match
      case Tone.Success => green
      case Tone.Failure => red
      case Tone.Warning => yellow
      case Tone.Muted   => base01
      case Tone.Accent  => cyan
      case Tone.Info    => blue

    def accent(accent: Token.Accent): Chroma = accent match
      case Token.Accent.Keyword  => orange
      case Token.Accent.Modifier => orange
      case Token.Accent.String   => cyan
      case Token.Accent.Number   => magenta
      case Token.Accent.Term     => base0
      case Token.Accent.Typal    => yellow
      case Token.Accent.Symbol   => violet
      case Token.Accent.Parens   => base01
      case Token.Accent.Error    => red
      case Token.Accent.Unparsed => base0
