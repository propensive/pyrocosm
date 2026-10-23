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

  // A stack trace's colours: the file, the method and the line, and the five accents its
  // packages take in order of first appearance. Digression's own by default, which suit a dark
  // ground; a light theme overrides them with deeper values.
  def traceFile: Chroma = Chroma(0x5f9e9f)
  def traceMethod: Chroma = Chroma(0xabcfdf)
  def traceLine: Chroma = Chroma(0x47d1cc)

  def traceAccents: List[Chroma] =
    List(Chroma(0xf84020), Chroma(0xd88600), Chroma(0xfefe00), Chroma(0xfeae00), Chroma(0xaefe00))

  // The page has three modes, which a button in the masthead cycles through: *mixed* (the
  // theme's own colours: a light page with code on a dark ground), *light* (the same page,
  // code on a light ground, its accents the same hues deepened until they read against it)
  // and *dark* (the theme's `dark` variant: a dark page, code on the dark ground). A theme
  // with no dark variant is its own, and shows the same in both.
  def dark: WebTheme = scala.caps.unsafe.unsafeAssumePure(this)
  def lightCodeBackground: Chroma = surface
  def lightCodeForeground: Chroma = foreground
  def lightCodeAccent(accent: Token.Accent): Chroma = WebTheme.contrasting(codeAccent(accent), lightCodeBackground)

  // Inline code sits among prose, so in the mixed mode it takes the light ground rather than
  // the block's dark one; a dark theme sets it to its block's ground.
  def inlineCodeBackground: Chroma = lightCodeBackground

  // Every colour of this theme, by the custom property it sets.
  def colours: List[(Text, Chroma)] =
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

    val trace: List[(Text, Chroma)] =
      List(t"trace-file" -> traceFile, t"trace-method" -> traceMethod, t"trace-line" -> traceLine)
      + traceAccents.indexed.map { (chroma, index) => t"trace-accent-${index.n1}" -> chroma }

    roles + tones + accents + codeColours + List(t"inline-bg" -> inlineCodeBackground)

  // A code block's colours alone: its ground, its text and its token accents.
  def codeColours: List[(Text, Chroma)] =
    List(t"code-bg" -> codeBackground, t"code-fg" -> codeForeground)
    + Token.Accent.values.foldLeft(Nil: List[(Text, Chroma)]) { (acc, accent0) => acc :+ (t"code-accent-${accent0.toString.tt.lower}" -> codeAccent(accent0)) }

  // The light mode's code colours, on the light ground.
  def lightCodeColours: List[(Text, Chroma)] =
    List(t"code-bg" -> lightCodeBackground, t"code-fg" -> lightCodeForeground, t"inline-bg" -> lightCodeBackground)
    + Token.Accent.values.foldLeft(Nil: List[(Text, Chroma)]) { (acc, accent0) => acc :+ (t"code-accent-${accent0.toString.tt.lower}" -> lightCodeAccent(accent0)) }

  // The `:root` block, which is the mixed mode, and the light and dark modes' blocks, keyed
  // by the `data-mode` the script sets on the document. Cataclysm's `css` interpolator checks
  // a substituted value against the property's grammar and has no notion of a custom
  // property, so the blocks are read from text at runtime, where a `--name` declaration is
  // accepted with any value.
  def variables: Css =
    def block(selector: Text, colours: List[(Text, Chroma)]): Text =
      val declarations: Text = colours.map { (name, chroma) => t"--pyro-$name: ${WebTheme.hex(chroma)}" }.join(t"; ")
      t"$selector { $declarations }"

    List
      ( block(t":root", colours),
        block(t"[data-mode=\"light\"]", lightCodeColours),
        block(t"[data-mode=\"dark\"]", dark.colours) )
    . join(t" ")
    . read[Css]

object WebTheme:
  given default: WebTheme = Ember

  def hex(chroma: Chroma): Text =
    def channel(value: Int): Text =
      val digits = java.lang.Integer.toHexString(value&255).nn
      (if digits.length == 1 then "0"+digits else digits).tt

    t"#${channel(chroma.red)}${channel(chroma.green)}${channel(chroma.blue)}"

  // The relative luminance of a colour, as the contrast ratio is defined on.
  def luminance(chroma: Chroma): Double =
    def linear(value: Int): Double =
      val c: Double = (value&255)/255.0
      if c <= 0.03928 then c/12.92 else java.lang.Math.pow((c + 0.055)/1.055, 2.4)

    0.2126*linear(chroma.red) + 0.7152*linear(chroma.green) + 0.0722*linear(chroma.blue)

  def contrast(a: Chroma, b: Chroma): Double =
    val (light, dark) = (luminance(a).max(luminance(b)), luminance(a).min(luminance(b)))
    (light + 0.05)/(dark + 0.05)

  // The colour's hue and saturation at a lightness, and back.
  private def hsl(chroma: Chroma): (Double, Double, Double) =
    val r: Double = (chroma.red&255)/255.0
    val g: Double = (chroma.green&255)/255.0
    val b: Double = (chroma.blue&255)/255.0
    val max: Double = r.max(g).max(b)
    val min: Double = r.min(g).min(b)
    val l: Double = (max + min)/2
    val d: Double = max - min

    if d == 0.0 then (0.0, 0.0, l) else
      val s: Double = if l > 0.5 then d/(2 - max - min) else d/(max + min)
      val h: Double =
        if max == r then ((g - b)/d + (if g < b then 6 else 0))/6
        else if max == g then ((b - r)/d + 2)/6
        else ((r - g)/d + 4)/6
      (h, s, l)

  private def rgb(h: Double, s: Double, l: Double): Chroma =
    def hue(p: Double, q: Double, t0: Double): Double =
      val t: Double = if t0 < 0 then t0 + 1 else if t0 > 1 then t0 - 1 else t0
      if t < 1.0/6 then p + (q - p)*6*t
      else if t < 0.5 then q
      else if t < 2.0/3 then p + (q - p)*(2.0/3 - t)*6
      else p

    val (r, g, b): (Double, Double, Double) =
      if s == 0.0 then (l, l, l) else
        val q: Double = if l < 0.5 then l*(1 + s) else l + s - l*s
        val p: Double = 2*l - q
        (hue(p, q, h + 1.0/3), hue(p, q, h), hue(p, q, h - 1.0/3))

    def channel(value: Double): Int = (value*255 + 0.5).toInt.max(0).min(255)
    Chroma((channel(r) << 16) | (channel(g) << 8) | channel(b))

  // The colour, in its own hue, deepened (or lightened, on a dark ground) a step at a time
  // until it reads against the ground at the ratio WCAG asks of text: how a light mode's code
  // accents come from a dark mode's.
  def contrasting(chroma: Chroma, ground: Chroma, ratio: Double = 4.5): Chroma =
    val (h, s, l) = hsl(chroma)
    val darker: Boolean = luminance(ground) > 0.5

    def recur(l: Double): Chroma =
      val candidate: Chroma = rgb(h, s, l)
      if contrast(candidate, ground) >= ratio || l <= 0.0 || l >= 1.0 then candidate
      else recur(if darker then l - 0.02 else l + 0.02)

    recur(l)

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
    // reads the same in a browser as in a Flame session; in the light mode it sits on a pale
    // teal, with the same hues deepened to read there.
    private val night = Chroma(0x16131f)
    private val plum = Chroma(0x1b0121)
    private val lagoon = Chroma(0xbce0dc)
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

    override def codeBackground: Chroma = plum
    override def codeForeground: Chroma = codeText
    override def lightCodeBackground: Chroma = lagoon
    override def lightCodeForeground: Chroma = soot
    override def dark: WebTheme = Night

    // A trace sits on the light page, so its colours are deeper than digression's, in the
    // page's own hues: the accents are the tones and links the rest of the page uses.
    override def traceFile: Chroma = Chroma(0x2f6f70)
    override def traceMethod: Chroma = Chroma(0x3b6f88)
    override def traceLine: Chroma = Chroma(0x1e8f8a)
    override def traceAccents: List[Chroma] = List(crimson, rust, gold, amber, leaf)

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

  // Ember's dark mode: the same accents, brightened, on a deep blue page, with code on black.
  object Night extends WebTheme:
    private val abyss = Chroma(0x0e1729)
    private val deep = Chroma(0x172238)
    private val pale = Chroma(0xdfe4ee)
    private val white = Chroma(0xffffff)
    private val haze = Chroma(0x9ba7bd)
    private val slate = Chroma(0x2b3a57)
    private val saffron = Chroma(0xf5c518)
    private val soot = Chroma(0x120b08)
    private val flame = Chroma(0xff7a3d)
    private val rust = Chroma(0xf29b63)
    private val gold = Chroma(0xe0b45c)
    private val amber = Chroma(0xf0b04a)
    private val crimson = Chroma(0xf05e5e)
    private val magenta = Chroma(0xe07be8)
    private val violet = Chroma(0xb39ddb)
    private val leaf = Chroma(0x6fbf6a)

    def background: Chroma = abyss
    def surface: Chroma = deep
    def foreground: Chroma = pale
    def muted: Chroma = haze
    def border: Chroma = slate
    def key: Chroma = rust
    def reference: Chroma = gold
    def figure: Chroma = pale
    def units: Chroma = haze
    def link: Chroma = rust
    def selection: Chroma = Chroma(0x3a2a1f)
    override def onAccent: Chroma = soot
    override def title: Chroma = white
    override def button: Chroma = saffron
    override def onButton: Chroma = soot
    override def menubar: Chroma = Chroma(0x16131f)
    override def onMenubar: Chroma = white

    def tone(tone: Tone): Chroma = tone match
      case Tone.Success => leaf
      case Tone.Failure => crimson
      case Tone.Warning => amber
      case Tone.Muted   => haze
      case Tone.Accent  => flame
      case Tone.Info    => violet

    def accent(accent: Token.Accent): Chroma = accent match
      case Token.Accent.Keyword  => rust
      case Token.Accent.Modifier => rust
      case Token.Accent.String   => gold
      case Token.Accent.Number   => magenta
      case Token.Accent.Term     => pale
      case Token.Accent.Typal    => violet
      case Token.Accent.Symbol   => haze
      case Token.Accent.Parens   => Chroma(0x7f8ca3)
      case Token.Accent.Error    => crimson
      case Token.Accent.Unparsed => pale
      case Token.Accent.Command  => rust

    override def codeBackground: Chroma = Chroma(0x000000)   // full black on the dark page
    override def inlineCodeBackground: Chroma = codeBackground
    override def codeForeground: Chroma = Ember.codeForeground
    override def codeAccent(accent: Token.Accent): Chroma = Ember.codeAccent(accent)
    override def traceAccents: List[Chroma] = List(crimson, rust, gold, amber, leaf)

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
