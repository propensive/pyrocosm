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
  // frame of a stack trace recede into. Dark by default, as the terminal themes here are.
  def background: Chroma = Chroma(0x002b36)

  // A stack trace's colours, digression's own by default so a trace reads the same here as in
  // any other Soundness terminal: the file, the method and the line, and the five accents the
  // packages take in turn.
  def traceFile: Chroma = Chroma(0x5f9e9f)
  def traceMethod: Chroma = Chroma(0xabcfdf)
  def traceLine: Chroma = Chroma(0x47d1cc)

  def traceAccents: List[Chroma] =
    List(Chroma(0xf84020), Chroma(0xd88600), Chroma(0xfefe00), Chroma(0xfeae00), Chroma(0xaefe00))

  // `chroma`, mixed toward the background by `factor`: `1.0` is the colour itself, `0.0` the
  // background. As iridescence's `Palette#subdue`, but on a packed colour.
  def subdue(chroma: Chroma, factor: Double): Chroma =
    def channel(from: Int, to: Int): Int = (to + (from - to)*factor).toInt.max(0).min(255)

    Chroma
      ( channel(chroma.red, background.red),
        channel(chroma.green, background.green),
        channel(chroma.blue, background.blue) )

object TerminalTheme:
  given default: TerminalTheme = Solarized

  // Solarized dark, the scheme fume's reports have always used; the syntax accents follow
  // flame's editor palette where solarized has no equivalent.
  object Solarized extends TerminalTheme:
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

    def foreground: Chroma = base0
    def muted: Chroma = base01
    def key: Chroma = yellow
    def reference: Chroma = blue
    def figure: Chroma = base0
    def units: Chroma = cyan
    def link: Chroma = blue
    def selection: Chroma = Chroma(0x073642)

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
