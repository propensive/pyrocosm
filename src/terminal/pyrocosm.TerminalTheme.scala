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

// Excluded from the umbrella: `Token` (harlequin), which would outrank this package's own
// definitions, since a wildcard import beats a package member declared in another file.
import soundness.{Token as _, *}

object TerminalTheme:
  given default: TerminalTheme = Solarized

  // An iridescence colour as the packed form a theme is written in.
  def chroma(color: Color in Srgb): Chroma = color.chroma

  // Solarized dark, the scheme fume's reports have always used, in iridescence's own values;
  // the syntax accents follow flame's editor palette where solarized has no equivalent.
  object Solarized extends TerminalTheme:
    private object colours extends iridescence.Solarized:
      val luminosity: Brightness = Brightness.Dark
      val background: Color in Srgb = this.base03
      val foreground: Color in Srgb = this.base3

    private val base03 = chroma(colours.base03)
    private val base02 = chroma(colours.base02)
    private val base01 = chroma(colours.base01)
    private val base0 = chroma(colours.base0)
    private val yellow = chroma(colours.yellow)
    private val orange = chroma(colours.orange)
    private val red = chroma(colours.red)
    private val magenta = chroma(colours.magenta)
    private val violet = chroma(colours.violet)
    private val blue = chroma(colours.blue)
    private val cyan = chroma(colours.cyan)
    private val green = chroma(colours.green)

    def background: Chroma = base03
    def foreground: Chroma = base0
    def muted: Chroma = base01
    def key: Chroma = yellow
    def reference: Chroma = blue
    def figure: Chroma = base0
    def units: Chroma = cyan
    def link: Chroma = blue
    def selection: Chroma = base02

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
      case Token.Accent.Command  => orange
      case Token.Accent.Comment  => green
      case Token.Accent.Substitute => base0

// The terminal's colours, by role. A theme answers for every tone, every syntax accent and the
// handful of other roles the renderer distinguishes, so that no colour literal appears in the
// renderer and a theme can be swapped by import.
trait TerminalTheme:
  def foreground: Chroma
  def muted: Chroma
  def tone(tone: Tone): Chroma
  def accent(accent: Token.Accent): Chroma
  def key: Chroma          // keystrokes
  def reference: Chroma    // identifiers and hashes
  def figure: Chroma       // numbers
  def units: Chroma        // the units after an amount
  def link: Chroma
  def selection: Chroma    // the background of a focused, selected row

  // The ground a subdued colour fades toward: what a repeated class or file name and a plumbing
  // frame of a stack trace recede into. Every theme says what its ground is, since a theme on
  // a black terminal which faded toward another's dark blue would not fade at all.
  def background: Chroma

  // A stack trace's colours, digression's own by default so a trace reads the same here as in
  // any other Soundness terminal: the file, the method and the line, and the five accents the
  // packages take in turn.
  def traceFile: Chroma = Chroma(0x5f9e9f)
  def traceMethod: Chroma = Chroma(0xabcfdf)
  def traceLine: Chroma = Chroma(0x47d1cc)

  def traceAccents: List[Chroma] =
    List(Chroma(0xf84020), Chroma(0xd88600), Chroma(0xfefe00), Chroma(0xfeae00), Chroma(0xaefe00))

  // `chroma`, mixed toward the background by `factor`: `1.0` is the colour itself, `0.0` the
  // background, as iridescence mixes.
  def subdue(chroma: Chroma, factor: Double): Chroma =
    chroma.packed.srgb.mix(background.packed.srgb, 1.0 - factor.max(0.0).min(1.0)).chroma

  // The theme's colours for a gauge, by the roles a gauge draws with: the track is the ground
  // a little toward the foreground, the fill the accent, its leading edge the info tone.
  def gaugePalette: GaugePalette =
    val ground: Color in Srgb = background.packed.srgb
    val ink: Color in Srgb = foreground.packed.srgb
    val rail: Color in Srgb = subdue(foreground, 0.15).packed.srgb
    val bar: Color in Srgb = tone(Tone.Accent).packed.srgb
    val edge: Color in Srgb = tone(Tone.Info).packed.srgb
    val faint: Color in Srgb = muted.packed.srgb
    val good: Color in Srgb = tone(Tone.Success).packed.srgb
    val warn: Color in Srgb = tone(Tone.Warning).packed.srgb
    val bad: Color in Srgb = tone(Tone.Failure).packed.srgb

    new GaugePalette:
      val background: Color in Srgb = ground
      val foreground: Color in Srgb = ink
      val track: Color in Srgb = rail
      val fill: Color in Srgb = bar
      val leadingEdge: Color in Srgb = edge
      val caption: Color in Srgb = ink
      val muted: Color in Srgb = faint
      val success: Color in Srgb = good
      val warning: Color in Srgb = warn
      val danger: Color in Srgb = bad
