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

import strategies.throwUnsafely
import dysasymptotics.linearSize
import errorDiagnostics.emptyDiagnostics

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

  // The text on an accent-filled surface, a button's face: the background by default, which
  // is dark on a dark theme's bright accent and light on a light theme's deep one.
  def onAccent: Chroma = background

  // Titles, which may be darker than prose; a button's face and the text on it, which need
  // not be the accent: the default theme's buttons are yellow with black text.
  def title: Chroma = foreground
  def button: Chroma = tone(Tone.Accent)
  def onButton: Chroma = onAccent

  // The menu bar along the top of every page, and its text: dark with white text by default.
  def menubar: Chroma = foreground
  def onMenubar: Chroma = background

  // A code block's own ground and text, and the token accents on that ground: a light theme
  // may set code on a dark ground, with brighter accents than its prose carries. By default a
  // code block is a surface in the theme's own colours.
  def codeBackground: Chroma = surface
  def codeForeground: Chroma = foreground
  def codeAccent(accent: Token.Accent): Chroma = this.accent(accent)

  // The `:root` block. Cataclysm's `css` interpolator checks a substituted value against the
  // property's grammar and has no notion of a custom property, so the block is read from text
  // at runtime, where a `--name` declaration is accepted with any value.
  def variables: Css =
    val roles: List[(Text, Chroma)] =
      List
        ( t"bg" -> background, t"surface" -> surface, t"fg" -> foreground, t"muted" -> muted,
          t"border" -> border, t"key" -> key, t"reference" -> reference, t"figure" -> figure,
          t"units" -> units, t"link" -> link, t"selection" -> selection,
          t"on-accent" -> onAccent, t"code-bg" -> codeBackground, t"code-fg" -> codeForeground,
          t"title" -> title, t"button" -> button, t"on-button" -> onButton,
          t"menubar" -> menubar, t"on-menubar" -> onMenubar )

    val tones: List[(Text, Chroma)] =
      Tone.values.foldLeft(Nil: List[(Text, Chroma)]) { (acc, tone0) => acc :+ (t"tone-${tone0.toString.tt.lower}" -> tone(tone0)) }

    val accents: List[(Text, Chroma)] =
      Token.Accent.values.foldLeft(Nil: List[(Text, Chroma)]) { (acc, accent0) => acc :+ (t"accent-${accent0.toString.tt.lower}" -> accent(accent0)) }

    val codeAccents: List[(Text, Chroma)] =
      Token.Accent.values.foldLeft(Nil: List[(Text, Chroma)]) { (acc, accent0) => acc :+ (t"code-accent-${accent0.toString.tt.lower}" -> codeAccent(accent0)) }

    val declarations: Text =
      (roles + tones + accents + codeAccents).map { (name, chroma) => t"--pyro-$name: ${WebTheme.hex(chroma)}" }.join(t"; ")

    t":root { $declarations }".read[Css]

object WebTheme:
  given default: WebTheme = Ember

  def hex(chroma: Chroma): Text =
    def channel(value: Int): Text =
      val digits = java.lang.Integer.toHexString(value&255).nn
      (if digits.length == 1 then "0"+digits else digits).tt

    t"#${channel(chroma.red)}${channel(chroma.green)}${channel(chroma.blue)}"

  // The default: a light page with flame accents. Text is smoke-charcoal on the faintest grey,
  // cards are white, an action is a flame, `Info` is a cosmic violet so it reads apart from
  // the warning amber and the failure crimson, and code sits on a near-black ground with a
  // hint of violet, its tokens in brighter accents.
  object Ember extends WebTheme:
    private val mist = Chroma(0xf5f5f7)
    private val white = Chroma(0xffffff)
    private val smoke = Chroma(0x2a1a12)
    private val soot = Chroma(0x120b08)
    private val saffron = Chroma(0xf5c518)
    private val ash = Chroma(0x7a6257)
    private val cinder = Chroma(0xb0a099)
    private val flame = Chroma(0xd9480f)
    private val rust = Chroma(0xc2410c)
    private val ember = Chroma(0x9a3412)
    private val gold = Chroma(0xb45309)
    private val amber = Chroma(0xd97706)
    private val crimson = Chroma(0xc81e1e)
    private val magenta = Chroma(0xa21caf)
    private val violet = Chroma(0x6d4c9f)
    private val leaf = Chroma(0x3f7d3a)

    // Code on its dark ground takes the Zed colours Flame's terminal highlights with, so code
    // reads the same in a browser as in a Flame session.
    private val night = Chroma(0x16131f)
    private val codeText = Chroma(0xd4be98)
    private val codeKeyword = Chroma(0xff6633)
    private val codeString = Chroma(0x99ffff)
    private val codeNumber = Chroma(0xcc3366)
    private val codeTerm = Chroma(0xffcc99)
    private val codeType = Chroma(0x00cc99)
    private val codeSymbol = Chroma(0xcc6699)
    private val codeParens = Chroma(0xf28534)
    private val codeError = Chroma(0xea6962)

    def background: Chroma = mist
    def surface: Chroma = white
    def foreground: Chroma = smoke
    def muted: Chroma = ash
    def border: Chroma = Chroma(0xe4e2e6)
    def key: Chroma = ember
    def reference: Chroma = gold
    def figure: Chroma = smoke
    def units: Chroma = ash
    def link: Chroma = rust
    def selection: Chroma = Chroma(0xffe4d1)
    override def onAccent: Chroma = white
    override def title: Chroma = soot
    override def button: Chroma = saffron
    override def onButton: Chroma = soot
    override def menubar: Chroma = night
    override def onMenubar: Chroma = white

    def tone(tone: Tone): Chroma = tone match
      case Tone.Success => leaf
      case Tone.Failure => crimson
      case Tone.Warning => amber
      case Tone.Muted   => ash
      case Tone.Accent  => flame
      case Tone.Info    => violet

    def accent(accent: Token.Accent): Chroma = accent match
      case Token.Accent.Keyword  => rust
      case Token.Accent.Modifier => rust
      case Token.Accent.String   => gold
      case Token.Accent.Number   => magenta
      case Token.Accent.Term     => smoke
      case Token.Accent.Typal    => violet
      case Token.Accent.Symbol   => ash
      case Token.Accent.Parens   => cinder
      case Token.Accent.Error    => crimson
      case Token.Accent.Unparsed => smoke
      case Token.Accent.Command  => rust

    override def codeBackground: Chroma = night
    override def codeForeground: Chroma = codeText

    override def codeAccent(accent: Token.Accent): Chroma = accent match
      case Token.Accent.Keyword  => codeKeyword
      case Token.Accent.Modifier => codeKeyword
      case Token.Accent.Command  => codeKeyword
      case Token.Accent.String   => codeString
      case Token.Accent.Number   => codeNumber
      case Token.Accent.Term     => codeTerm
      case Token.Accent.Typal    => codeType
      case Token.Accent.Symbol   => codeSymbol
      case Token.Accent.Parens   => codeParens
      case Token.Accent.Error    => codeError
      case Token.Accent.Unparsed => codeText

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
      case Token.Accent.Command  => orange
