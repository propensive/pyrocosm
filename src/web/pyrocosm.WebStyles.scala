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
import errorDiagnostics.emptyDiagnostics

// The one stylesheet. Every rule is a `css"…"` value, validated as the code compiles; every
// colour is a `var(--pyro-…)` reference to the theme's `:root` block, so a second theme is a
// second block and no rule changes. The measures, the spacing scale and the type are custom
// properties too, declared in a `:root` block of their own, so a rule names a step of the
// scale rather than a length.
//
// Type: Marcellus for titles, Yantramanav for prose, and Sono — a variable face with a `MONO` axis —
// for code, at fixed width, and for the small tracked capitals of labels and buttons, at
// variable width; fetched from Google Fonts by the `@import` that heads the sheet, with system
// fallbacks. Text is one size, but for labels and code (a little smaller, as monospace reads
// larger). Layout: the masthead and toolbar span the page; the matter beneath is
// centred within a measure, wider when a side column is present, and every column stacks on a
// narrow viewport.
object WebStyles:
  // The Google Fonts stylesheet declaring the three typefaces' `@font-face` rules; one node
  // built by hand, as cataclysm's typeface provisions would import it once per typeface.
  val fontsUrl: Text =
    t"https://fonts.googleapis.com/css2?family=Yantramanav:wght@300;400;500;700&family=Marcellus&family=Sono:wght,MONO@200..800,0..1&display=swap"

  // The `@import` of the fonts' stylesheet, which must precede every other rule, so a page
  // that prepends other rules (graffiti's own) puts this before them: `fonts + … + rules`.
  val fonts: Css = Css(List(Css.Node.At(t"import", t"url(\"$fontsUrl\")", Unset)))

  // The whole sheet: the fonts' import, then the rules.
  def css(theme: WebTheme): Css = fonts + rules(theme)

  def rules(theme: WebTheme): Css =
    // A tone's colour, its notice's edge and tint, and a tinted row; an accent's colour. These
    // rules are generated per case, so they are read from text like the theme's `:root`
    // block; the tints are `color-mix`es of the variable, which browsers resolve.
    def tone(tone: Tone): Css =
      val name = HtmlRenderer.toneClass(tone)
      val variable = t"var(--pyro-tone-${tone.toString.tt.lower})"
      List
        ( t".$name { color: $variable }",
          t".pyro-notice.$name { color: var(--pyro-fg); border-left-color: $variable; background-color: color-mix(in srgb, $variable 8%, var(--pyro-surface)) }",
          t"tr.$name > td { background-color: color-mix(in srgb, $variable 10%, transparent) }" )
      . join(t" ")
      . read[Css]

    def accent(accent: Token.Accent): Css =
      val name = HtmlRenderer.accentClass(accent)
      val lower = accent.toString.tt.lower
      t".$name { color: var(--pyro-accent-$lower) } .pyro-codeblock .$name { color: var(--pyro-code-accent-$lower) }".read[Css]

    // The non-colour custom properties: the fonts, the spacing scale, the radii, the small and
    // the monospace text sizes, the two measures and the shadows. Read from text as custom
    // property declarations are (the theme's block is the same).
    val scale: Css =
      List
        ( t":root { --pyro-font-body: \"Yantramanav\", system-ui, -apple-system, \"Segoe UI\", sans-serif;",
          t"--pyro-font-title: \"Marcellus\", Georgia, \"Times New Roman\", serif;",
          t"--pyro-font-mono: \"Sono\", ui-monospace, SFMono-Regular, Menlo, monospace;",
          t"--pyro-font-label: \"Sono\", \"Yantramanav\", system-ui, sans-serif;",
          t"--pyro-rule: color-mix(in srgb, var(--pyro-fg) 10%, transparent);",
          t"--pyro-space-1: 0.25rem; --pyro-space-2: 0.5rem; --pyro-space-3: 0.75rem; --pyro-space-4: 1rem;",
          t"--pyro-space-5: 1.5rem; --pyro-space-6: 2rem; --pyro-space-7: 3rem;",
          t"--pyro-radius: 3px; --pyro-radius-card: 3px;",
          t"--pyro-text-body: 0.88rem; --pyro-text-small: 0.715rem; --pyro-text-mono: 0.75rem; --pyro-text-inline: 0.825rem; --pyro-text-units: 0.79rem; --pyro-text-math: 1.05rem; --pyro-text-button: 0.6875rem;",
          t"--pyro-measure: 68rem; --pyro-measure-wide: 88rem; --pyro-gutter: clamp(1rem, 4vw, 2.5rem);",
          t"--pyro-shadow-card: 0 1px 2px color-mix(in srgb, var(--pyro-fg) 6%, transparent), 0 1px 6px color-mix(in srgb, var(--pyro-fg) 4%, transparent);",
          t"--pyro-shadow-button: 0 1px 2px color-mix(in srgb, var(--pyro-fg) 18%, transparent);",
          t"--pyro-focus-ring: 0 0 0 3px color-mix(in srgb, var(--pyro-tone-accent) 25%, transparent);",
          t"--pyro-track: color-mix(in srgb, var(--pyro-fg) 10%, transparent);",
          t"--pyro-button-face: linear-gradient(180deg, color-mix(in srgb, var(--pyro-surface) 12%, transparent) 0%, transparent 55%, color-mix(in srgb, var(--pyro-fg) 7%, transparent) 100%);",
          t"--pyro-button-hover: color-mix(in srgb, var(--pyro-button) 85%, var(--pyro-surface));",
          t"--pyro-page-face: linear-gradient(180deg, color-mix(in srgb, var(--pyro-fg) 5%, var(--pyro-bg)) 0%, var(--pyro-bg) 40rem) }" )
      . join(t" ")
      . read[Css]

    val base: Css =
      css"*, *::before, *::after { box-sizing: border-box }"
        + css"html { text-size-adjust: 100% }"
        + css"body { margin: 0; min-height: 100vh; background-color: var(--pyro-bg); background-image: var(--pyro-page-face); background-repeat: no-repeat; color: var(--pyro-fg); font-family: var(--pyro-font-body); font-size: var(--pyro-text-body); font-weight: 400; line-height: 1.55 }"
        + css"code, kbd, pre, .pyro-figure, .pyro-reference, .pyro-sparkline, .pyro-editor, .pyro-placeholder, .pyro-output, .pyro-completions { font-family: var(--pyro-font-mono); font-variation-settings: 'MONO' 1; font-size: var(--pyro-text-mono) }"
        + css"pre code, code code, kbd kbd { font-size: 1em }"
        // Inline code, references, figures and mathematics sit within prose, whose face is
        // larger on the body than theirs, so they take a size of their own to match it.
        + css"code.pyro-code, code.pyro-reference, .pyro-figure { font-size: var(--pyro-text-inline) }"
        + css"a { color: var(--pyro-link); text-decoration-thickness: 1px; text-underline-offset: 0.15em }"
        + css"a:hover { color: var(--pyro-tone-accent) }"
        + css"h1, h2, h3, h4 { font-family: var(--pyro-font-title); font-weight: 400; line-height: 1.25; letter-spacing: 0.01em; margin: 0 0 var(--pyro-space-3) 0; color: var(--pyro-title) }"
        + css"h1 { font-size: 2.45rem } h2 { font-size: 1.75rem } h3 { font-size: 1.575rem } h4 { font-size: 1.4rem }"
        + css"h2:not(:first-child) { margin-top: var(--pyro-space-6) } h3:not(:first-child), h4:not(:first-child) { margin-top: var(--pyro-space-5) }"
        + css"p { margin: 0 0 var(--pyro-space-3) 0 } p:last-child { margin-bottom: 0 }"
        + css"ul, ol { margin: 0 0 var(--pyro-space-3) 0; padding-left: var(--pyro-space-5); line-height: 1.25 } ul:last-child, ol:last-child { margin-bottom: 0 }"
        + css"ul { list-style: none; padding-left: 0 }"
        + css"li { margin: 0.2rem 0 }"
        + css"hr { border: 0; border-top: 1px solid var(--pyro-border); margin: var(--pyro-space-4) 0 }"
        + css"hr.pyro-rule-above { border-top: 0; border-bottom: 1px solid var(--pyro-border); margin-bottom: 0 }"
        + css"hr.pyro-rule-below { margin-top: 0 }"

    // The matter's measure and the columns: the graffiti rules for the layouts come earlier
    // in the sheet, so these override them at the same specificity. A side column stays in
    // view as the matter scrolls.
    val layout: Css =
      css"main.graffiti-mainstay { max-width: var(--pyro-measure); margin: 0 auto; padding: var(--pyro-space-6) var(--pyro-gutter) var(--pyro-space-7) var(--pyro-gutter) }"
        + css"body.pyro-has-verso main.graffiti-mainstay, body.pyro-has-recto main.graffiti-mainstay { max-width: var(--pyro-measure-wide) }"
        + css".graffiti-verso-layout, .graffiti-recto-layout { gap: var(--pyro-space-5); align-items: start }"
        + css".graffiti-verso, .graffiti-recto { position: sticky; top: var(--pyro-space-5) }"
        + css".graffiti-verso-content, .graffiti-recto-content { min-width: 0 }"

    // The menu bar spans the page as a dark band above the masthead; its contents are centred
    // to the wide measure: the wordmark at the start, the configuration link at the end.
    val menubar: Css =
      css"nav.graffiti-top-menu { display: block; gap: 0; background-color: var(--pyro-menubar); color: var(--pyro-on-menubar) }"
        + css".pyro-menubar-inner { max-width: var(--pyro-measure-wide); margin: 0 auto; padding: 0 var(--pyro-gutter); display: flex; align-items: stretch; gap: var(--pyro-space-5); min-height: 2.5rem }"
        + css".pyro-menubar a { display: inline-flex; align-items: center; color: var(--pyro-on-menubar); text-decoration: none; font-family: var(--pyro-font-label); font-variation-settings: 'MONO' 0; font-size: var(--pyro-text-small); font-weight: 600; letter-spacing: 0.1em; text-transform: uppercase; border-bottom: 2px solid transparent }"
        + css".pyro-menubar a:hover { color: var(--pyro-on-menubar); border-bottom-color: var(--pyro-button) }"
        + css".pyro-wordmark { font-weight: 700 } .pyro-menubar-link { margin-left: auto }"

    // The masthead spans the page as a band; its rows are centred to the wide measure.
    val masthead: Css =
      css"header.graffiti-masthead { display: block; gap: 0; background-color: var(--pyro-surface); border-bottom: 1px solid var(--pyro-border) }"
        + css".pyro-masthead, .pyro-toolbar { max-width: var(--pyro-measure-wide); margin: 0 auto; padding: var(--pyro-space-4) var(--pyro-gutter); display: flex; flex-wrap: wrap; align-items: center }"
        + css".pyro-masthead { gap: var(--pyro-space-3) var(--pyro-space-6); min-height: 4.5rem }"
        + css".pyro-brand { display: flex; flex-wrap: wrap; align-items: baseline; gap: var(--pyro-space-2) var(--pyro-space-5); min-width: 0 }"
        + css".pyro-title { font-family: var(--pyro-font-title); font-weight: 400; font-size: 2.94rem; line-height: 1.2; letter-spacing: 0.02em; margin: 0; color: var(--pyro-title) }"
        + css".pyro-status { color: var(--pyro-muted); font-size: var(--pyro-text-small) }"
        + css".pyro-status .pyro-panel-content { display: flex; flex-wrap: wrap; align-items: center; gap: var(--pyro-space-2) var(--pyro-space-5) }"
        + css".pyro-status .pyro-panel-content > * { margin: 0 } .pyro-status .pyro-progress { width: 10rem }"
        + css".pyro-status .pyro-steps { display: flex; flex-wrap: wrap; gap: var(--pyro-space-1) var(--pyro-space-4) } .pyro-status .pyro-steps li { padding: 0 }"
        + css".pyro-connection { display: inline-flex; align-items: center; gap: var(--pyro-space-2); margin-left: auto; padding: 0.3rem 0.75rem; border: 1px solid var(--pyro-border); border-radius: var(--pyro-radius); font-size: var(--pyro-text-small); font-weight: 500; letter-spacing: 0.08em; text-transform: uppercase; color: var(--pyro-muted); background-color: var(--pyro-bg) }"
        + css".pyro-connection::before { content: ''; width: 0.5em; height: 0.5em; border-radius: 50%; background-color: currentcolor }"
        + css".pyro-connection.pyro-online { color: var(--pyro-tone-success) } .pyro-connection.pyro-offline { color: var(--pyro-tone-warning) }"
        + css".pyro-toolbar { list-style: none; gap: var(--pyro-space-2) var(--pyro-space-3); padding-top: var(--pyro-space-3); padding-bottom: var(--pyro-space-3); border-top: 1px solid var(--pyro-border) }"
        + css".pyro-toolbar > li { margin: 0; display: flex; align-items: center }"

    val phrasing: Css =
      css"em { font-weight: 600; font-style: normal }"
        + css".pyro-key { display: inline-block; padding: 0.05em 0.45em; border: 1px solid var(--pyro-border); border-bottom-width: 2px; border-radius: var(--pyro-radius); background-color: var(--pyro-surface); font-size: 0.85em; line-height: 1.4; color: var(--pyro-fg) }"
        + css".pyro-reference { color: var(--pyro-reference) }"
        + css".pyro-figure { color: var(--pyro-figure); font-variant-numeric: tabular-nums }"
        + css".pyro-units { color: var(--pyro-units); font-size: var(--pyro-text-units) }"
        + css".pyro-binding { font-style: italic }"
        + css".pyro-glyph-check { color: var(--pyro-tone-success) } .pyro-glyph-cross { color: var(--pyro-tone-failure) }"
        + css".pyro-glyph-warning, .pyro-glyph-winner { color: var(--pyro-tone-warning) } .pyro-glyph-pending { color: var(--pyro-muted) }"
        + css".pyro-math { font-family: math, serif; font-size: var(--pyro-text-math) }"

    // The small, tracked capitals that label things: table headers, record keys, the labels
    // of toggles, the connection pill and the buttons.
    val label: Css =
      css".pyro-table th, .pyro-record dt, .pyro-series dt, .pyro-disclosure summary { font-family: var(--pyro-font-label); font-variation-settings: 'MONO' 0; font-size: var(--pyro-text-small); font-weight: 600; letter-spacing: 0.06em; text-transform: uppercase; color: var(--pyro-muted) }"
        + css".pyro-connection, .pyro-control label, .pyro-button { font-family: var(--pyro-font-label); font-variation-settings: 'MONO' 0 }"

    val flow: Css =
      // The block's text aligns with the prose around it: its ground extends a gutter's
      // width beyond the text on either side, by a negative margin the padding cancels.
      css".pyro-codeblock { background-color: var(--pyro-code-bg); color: var(--pyro-code-fg); border: 0; border-radius: var(--pyro-radius); padding: var(--pyro-space-3) var(--pyro-space-4); margin: 0 calc(-1 * var(--pyro-space-4)) var(--pyro-space-4) calc(-1 * var(--pyro-space-4)); overflow-x: auto; line-height: 1.5 }"
        + css".pyro-note-erroneous { text-decoration: underline wavy var(--pyro-tone-failure) }"
        + css".pyro-note-caution { text-decoration: underline wavy var(--pyro-tone-warning) }"
        + css".pyro-note-highlight { background-color: var(--pyro-selection); border-radius: 2px }"
        + css".pyro-codeblock .pyro-note-highlight { background-color: color-mix(in srgb, var(--pyro-code-fg) 22%, transparent) }"
        + css".pyro-codeblock .pyro-note-erroneous { text-decoration-color: var(--pyro-code-accent-error) }"
        + css".pyro-note-param { font-style: italic }"
        // A table is followed by clear space, even when it ends its panel.
        + css".pyro-table { border-collapse: collapse; margin: 0 0 var(--pyro-space-6) 0; width: 100%; border-top: 2px solid var(--pyro-fg); border-bottom: 2px solid var(--pyro-fg) }"
        + css".pyro-table th, .pyro-table td { padding: var(--pyro-space-1) var(--pyro-space-3); border: 0; border-bottom: 1px solid var(--pyro-rule); text-align: left; vertical-align: top; line-height: 1.25 }"
        // A heading cell's top padding carries a pixel more than its bottom, to balance the
        // table's 2px top rule against the 1px rule beneath the headings.
        + css".pyro-table th { color: var(--pyro-fg); border-bottom: 1px solid var(--pyro-fg); padding-top: calc(var(--pyro-space-1) + 1px) }"
        // After the cells' own rule, which sets every cell's alignment, so that a numeric or
        // end-aligned column's cells are aligned to the right as its class asks.
        + css".pyro-table th.pyro-align-end, .pyro-table td.pyro-align-end, .pyro-table th.pyro-numeric, .pyro-table td.pyro-numeric { text-align: right }"
        + css".pyro-table tbody tr:last-child td { border-bottom: 0 }"
        + css".pyro-table caption { caption-side: bottom; color: var(--pyro-muted); font-size: var(--pyro-text-small); padding: var(--pyro-space-2) 0; text-align: left }"
        + css".pyro-align-end, .pyro-numeric { text-align: right }"
        + css".pyro-numeric { font-variant-numeric: tabular-nums }"
        + css".pyro-rigid { white-space: nowrap; width: 1% }"
        + css".pyro-stretch { width: 100% }"
        + css".pyro-record { display: grid; grid-template-columns: max-content 1fr; gap: var(--pyro-space-2) var(--pyro-space-5); margin: 0 0 var(--pyro-space-4) 0; align-items: baseline }"
        + css".pyro-record dd { margin: 0 } .pyro-record-section { margin: 0 0 var(--pyro-space-4) 0 } .pyro-record-section > h4 { margin-bottom: var(--pyro-space-2) }"
        + css".pyro-record-section > .pyro-record { margin-bottom: 0 }"
        + css".pyro-notice { border: 1px solid var(--pyro-border); border-left: 4px solid var(--pyro-border); padding: var(--pyro-space-3) var(--pyro-space-4); margin: 0 0 var(--pyro-space-4) 0; background-color: var(--pyro-surface); border-radius: var(--pyro-radius) }"
        + css".pyro-notice header { margin-bottom: var(--pyro-space-1); font-family: var(--pyro-font-title); font-size: 1.47rem; letter-spacing: 0.01em }"
        + css".pyro-notice header strong { font-weight: 400 }"
        + css".pyro-quotation { border-left: 3px solid var(--pyro-border); margin: 0 0 var(--pyro-space-4) 0; padding: var(--pyro-space-1) 0 var(--pyro-space-1) var(--pyro-space-4); color: var(--pyro-muted) }"
        + css".pyro-disclosure { margin: 0 0 var(--pyro-space-4) 0 } .pyro-disclosure summary { cursor: pointer; padding: var(--pyro-space-1) 0 }"
        + css".pyro-disclosure summary:hover { color: var(--pyro-tone-accent) } .pyro-disclosure[open] > summary { margin-bottom: var(--pyro-space-2) }"
        + css".pyro-tree, .pyro-tree ul { list-style: none; padding-left: var(--pyro-space-5); margin: 0; border-left: 1px solid var(--pyro-border) }"
        + css".pyro-tree { padding-left: 0; border-left: 0; margin: 0 0 var(--pyro-space-4) 0 }"
        + css".pyro-graph { list-style: none; padding: 0; margin: 0 0 var(--pyro-space-4) 0 } .pyro-arrow { color: var(--pyro-muted) }"
        + css".pyro-listing:last-child, .pyro-record:last-child, .pyro-record-section:last-child, .pyro-notice:last-child, .pyro-tree:last-child, .pyro-graph:last-child, .pyro-disclosure:last-child, .pyro-quotation:last-child { margin-bottom: 0 }"
        + css".pyro-codeblock:last-child { margin-bottom: 0 }"
        + css".pyro-image { margin: 0 0 var(--pyro-space-4) 0 } .pyro-image img { max-width: 100%; border-radius: var(--pyro-radius) } .pyro-image figcaption { color: var(--pyro-muted); font-size: var(--pyro-text-small); margin-top: var(--pyro-space-2) }"
        // A drawing is shown at its own size, never stretched: its text then keeps the size it
        // was set at. One wider than the matter scrolls sideways within its holder.
        + css".pyro-drawing { margin: 0 0 var(--pyro-space-4) 0; max-width: 100% } .pyro-drawing-holder { overflow-x: auto; max-width: 100% } .pyro-drawing svg { display: block; width: auto; height: auto; max-width: none } .pyro-drawing figcaption { color: var(--pyro-muted); font-size: var(--pyro-text-small); margin-top: var(--pyro-space-2) }"
        // A drawing's rectangles and circles move smoothly when a revision changes their
        // geometry in place: a bar grows from the baseline to a new value.
        + css".pyro-drawing rect, .pyro-drawing circle { transition-property: x, y, width, height, cx, cy, r; transition-duration: 0.4s; transition-timing-function: ease-out }"

    // A meter drawn as a flat bar in every engine: `appearance: none` leaves it to the
    // background rules, and each engine's own part for the filled length takes the accent.
    val meter: Css =
      List
        ( t".pyro-meter { appearance: none; -webkit-appearance: none; display: block; width: 100%; height: 0.6rem; border: 0; background-color: var(--pyro-track); border-radius: 0; overflow: hidden }",
          t".pyro-meter::-webkit-meter-bar { background-color: var(--pyro-track); border: 0; border-radius: 0 }",
          t".pyro-meter::-webkit-meter-optimum-value { background-color: var(--pyro-tone-accent); border-radius: 0 }",
          t".pyro-meter::-moz-meter-bar { background-color: var(--pyro-tone-accent); border-radius: 0 }",
          t".pyro-progress, .pyro-reckoning progress { appearance: none; -webkit-appearance: none; display: block; width: 100%; height: 0.6rem; border: 0; background-color: var(--pyro-track); border-radius: 0; overflow: hidden }",
          t".pyro-progress::-webkit-progress-bar, .pyro-reckoning progress::-webkit-progress-bar { background-color: var(--pyro-track); border-radius: 0 }",
          t".pyro-progress::-webkit-progress-value, .pyro-reckoning progress::-webkit-progress-value { background-color: var(--pyro-tone-accent); border-radius: 0 }",
          t".pyro-progress::-moz-progress-bar, .pyro-reckoning progress::-moz-progress-bar { background-color: var(--pyro-tone-accent); border-radius: 0 }",
          t".pyro-progress:indeterminate { background-image: repeating-linear-gradient(90deg, var(--pyro-tone-accent) 0 0.75rem, transparent 0.75rem 1.5rem); background-size: 200% 100%; animation: pyro-sweep 1.2s linear infinite }",
          t".pyro-progress:indeterminate::-webkit-progress-bar { background-color: transparent }",
          t".pyro-progress:indeterminate::-moz-progress-bar { background-color: transparent }",
          t"@keyframes pyro-sweep { from { background-position: 0 0 } to { background-position: 1.5rem 0 } }" )
      . join(t" ")
      . read[Css]

    val charts: Css =
      css".pyro-chart { margin: 0 0 var(--pyro-space-4) 0 }"
        + css".pyro-series { display: grid; grid-template-columns: max-content 1fr; gap: var(--pyro-space-2) var(--pyro-space-4); align-items: center; margin: 0 }"
        + css".pyro-series dt { grid-column: 1 }"
        + css".pyro-series dd { grid-column: 2; margin: 0; display: flex; align-items: center; gap: var(--pyro-space-3) }"
        + meter
        + css".pyro-sparkline { color: var(--pyro-tone-info); letter-spacing: 0.05em; line-height: 1 }"
        + css".pyro-gauge { margin: 0 0 var(--pyro-space-3) 0 } .pyro-gauge:last-child { margin-bottom: 0 } .pyro-gauge figcaption { color: var(--pyro-muted); font-size: var(--pyro-text-small); margin-bottom: var(--pyro-space-1) }"
        + css".pyro-gauge-standing, .pyro-gauge-elapsed, .pyro-gauge-remaining, .pyro-gauge-count { display: flex; align-items: baseline; gap: var(--pyro-space-3); margin-bottom: var(--pyro-space-1) }"
        + css".pyro-gauge-standing figcaption, .pyro-gauge-elapsed figcaption, .pyro-gauge-remaining figcaption, .pyro-gauge-count figcaption { margin: 0; min-width: 6rem }"
        + css".pyro-reckoning progress { flex: 1 1 auto }"
        + css".pyro-reckoning { display: flex; align-items: center; gap: var(--pyro-space-3) }"
        + css".pyro-steps { list-style: none; padding: 0; margin: 0 } .pyro-steps li { padding: var(--pyro-space-1) 0; margin: 0 }"
        + css".pyro-standing-succeeded .pyro-standing { color: var(--pyro-tone-success) }"
        + css".pyro-standing-failed .pyro-standing { color: var(--pyro-tone-failure) }"
        + css".pyro-standing-running .pyro-standing { color: var(--pyro-tone-accent) }"
        + css".pyro-standing-pending .pyro-standing { color: var(--pyro-muted) }"

    val panels: Css =
      css".pyro-panel { background-color: var(--pyro-surface); border: 1px solid var(--pyro-border); border-radius: var(--pyro-radius-card); box-shadow: var(--pyro-shadow-card); padding: var(--pyro-space-5); margin: 0 0 var(--pyro-space-5) 0; overflow: auto }"
        + css".pyro-panel:last-child { margin-bottom: 0 }"
        + css".pyro-role-primary.pyro-panel { padding-left: 2.4rem; padding-right: 2.4rem }"
        + css".pyro-panel > h2 { font-size: 1.75rem; padding-bottom: var(--pyro-space-2); border-bottom: 1px solid var(--pyro-border); margin: 0 0 var(--pyro-space-4) 0 }"
        + css".graffiti-verso .pyro-panel, .graffiti-recto .pyro-panel { padding: var(--pyro-space-4) }"
        + css".graffiti-verso .pyro-panel > h2, .graffiti-recto .pyro-panel > h2 { font-size: 1.575rem }"
        + css".pyro-role-log .pyro-panel-content { max-height: 20rem; overflow-y: auto }"
        + css".pyro-role-transcript.pyro-panel { background-color: transparent; border: 0; box-shadow: none; padding: 0 }"
        + css".pyro-role-prompt.pyro-panel { padding: var(--pyro-space-4) }"
        + css".pyro-action { cursor: pointer } .pyro-action:hover { background-color: color-mix(in srgb, var(--pyro-tone-accent) 8%, transparent) }"
        + css"tr.pyro-action:hover > td { background-color: color-mix(in srgb, var(--pyro-tone-accent) 8%, transparent) }"
        + css"li.pyro-action { border-radius: var(--pyro-radius); padding: var(--pyro-space-1) var(--pyro-space-2); margin-left: calc(-1 * var(--pyro-space-2)) }"
        + css"a.pyro-action { text-decoration: none }"
        + css".pyro-selected { outline: 2px solid var(--pyro-tone-accent); outline-offset: 2px }"
        + css".pyro-controls { display: flex; flex-wrap: wrap; gap: var(--pyro-space-3); align-items: center; margin-top: var(--pyro-space-4); padding-top: var(--pyro-space-4); border-top: 1px solid var(--pyro-border) }"
        + css".pyro-controls > .pyro-field-group { flex: 1 1 100% }"
        + css".pyro-panel-content:empty + .pyro-controls { margin-top: 0; padding-top: 0; border-top: 0 }"

    // The buttons: filled with the accent, in small tracked capitals, rounded as pills, which
    // rise a little under the pointer. A toggle's label and a choice share the button's
    // proportions so a row of controls lines up.
    val controls: Css =
      css".pyro-button { display: inline-flex; align-items: center; justify-content: center; gap: var(--pyro-space-2); padding: 0.55rem 1rem; border: 1px solid color-mix(in srgb, var(--pyro-button) 75%, var(--pyro-fg)); border-radius: var(--pyro-radius); background-color: var(--pyro-button); background-image: var(--pyro-button-face); color: var(--pyro-on-button); font-size: var(--pyro-text-button); font-weight: 600; line-height: 1; letter-spacing: 0.1em; text-transform: uppercase; cursor: pointer; box-shadow: var(--pyro-shadow-button); transition: background-color 0.2s ease }"
        // The face's gradient is a translucent overlay, so the colour beneath it is what a
        // hover lightens, and what the transition animates.
        + css".pyro-button:hover { background-color: var(--pyro-button-hover) }"
        + css".pyro-button:active { background-color: color-mix(in srgb, var(--pyro-button) 88%, var(--pyro-fg)); box-shadow: none }"
        + css".pyro-button:focus-visible { outline: 2px solid var(--pyro-tone-accent); outline-offset: 2px }"
        + css".pyro-button:disabled { background-color: var(--pyro-surface); background-image: none; color: var(--pyro-muted); border-color: var(--pyro-border); box-shadow: none; cursor: default }"
        + css".pyro-control label { display: inline-flex; align-items: center; gap: var(--pyro-space-2); cursor: pointer; font-size: var(--pyro-text-small); font-weight: 600; letter-spacing: 0.08em; text-transform: uppercase; color: var(--pyro-muted); padding: 0.5rem 0.25rem }"
        + css".pyro-toggle { appearance: none; display: inline-grid; place-content: center; width: 1.2em; height: 1.2em; margin: 0; border: 2px solid var(--pyro-fg); border-radius: 0; background-color: var(--pyro-surface); cursor: pointer }"
        + css".pyro-toggle::before { content: ''; width: 0.6em; height: 0.6em; background-color: var(--pyro-fg); transform: scale(0); transition: transform 0.1s ease }"
        + css".pyro-toggle:checked::before { transform: scale(1) }"
        + css".pyro-choice { font-family: var(--pyro-font-body); font-size: var(--pyro-text-small); font-weight: 500; padding: 0.5rem 0.9rem; border: 1px solid var(--pyro-border); border-radius: var(--pyro-radius); background-color: var(--pyro-surface); color: var(--pyro-fg); cursor: pointer }"
        + css".pyro-choice:focus-visible, .pyro-toggle:focus-visible { outline: 2px solid var(--pyro-tone-accent); outline-offset: 2px }"
        + css".pyro-field { display: block; width: 100%; background-color: var(--pyro-bg); color: var(--pyro-fg); border: 1px solid var(--pyro-border); border-radius: var(--pyro-radius); padding: var(--pyro-space-3); font-family: var(--pyro-font-mono); font-size: var(--pyro-text-mono); line-height: 1.5; resize: vertical; transition: border-color 0.15s ease, box-shadow 0.15s ease }"
        + css".pyro-field:focus, .pyro-field:focus-within { outline: none; border-color: var(--pyro-tone-accent); box-shadow: var(--pyro-focus-ring) }"
        + css".pyro-field::placeholder { color: var(--pyro-muted) }"
        + css".pyro-field-group { position: relative }"
        + css".pyro-editor { white-space: pre-wrap; word-break: break-word; min-height: 1.5em; caret-color: var(--pyro-tone-accent) }"
        + css".pyro-suggestion { color: var(--pyro-muted) }"
        + css".pyro-placeholder { display: none; position: absolute; top: var(--pyro-space-3); left: var(--pyro-space-3); right: var(--pyro-space-3); color: var(--pyro-muted); pointer-events: none; line-height: 1.5; white-space: nowrap; overflow: hidden; text-overflow: ellipsis }"
        + css".pyro-empty .pyro-placeholder { display: block }"
        + css".pyro-note { font-size: var(--pyro-text-small); color: var(--pyro-muted); margin-top: var(--pyro-space-2) } .pyro-note p { margin: var(--pyro-space-1) 0 }"
        + css".pyro-output { white-space: pre-wrap; word-break: break-word; margin: var(--pyro-space-2) 0; line-height: 1.5 }"
        + css".pyro-output-stdout .pyro-gutter { color: var(--pyro-tone-info) } .pyro-output-stderr .pyro-gutter { color: var(--pyro-tone-failure) }"
        + css".pyro-completions { list-style: none; margin: var(--pyro-space-2) 0 0 0; padding: var(--pyro-space-1) 0; border: 1px solid var(--pyro-border); border-radius: var(--pyro-radius); background-color: var(--pyro-surface); box-shadow: var(--pyro-shadow-card); max-height: 16rem; overflow-y: auto }"
        + css".pyro-completions li { display: flex; align-items: baseline; gap: var(--pyro-space-3); padding: var(--pyro-space-1) var(--pyro-space-3); margin: 0; cursor: pointer }"
        + css".pyro-completions li:hover { background-color: color-mix(in srgb, var(--pyro-tone-accent) 8%, transparent) }"
        + css".pyro-completions li.pyro-selected { background-color: var(--pyro-selection); outline: none }"
        + css".pyro-completions .pyro-signature { color: var(--pyro-muted); margin-left: auto; font-size: var(--pyro-text-small) }"

    // Narrower than a desk: the columns stack, a side panel stops sticking. Narrower still, a
    // phone: the gutters tighten, the title shrinks, the toolbar's buttons share the width, a
    // table scrolls sideways, and a peripheral panel gives way.
    val responsive: Css =
      css"@media (max-width: 64rem) { .graffiti-verso-layout, .graffiti-recto-layout { grid-template-columns: 1fr } .graffiti-verso, .graffiti-recto { inline-size: auto; position: static } }"
        + css"@media (max-width: 40rem) { main.graffiti-mainstay { padding-top: var(--pyro-space-4); padding-bottom: var(--pyro-space-6) } .pyro-panel, .graffiti-verso .pyro-panel, .graffiti-recto .pyro-panel, .pyro-role-primary.pyro-panel { padding: var(--pyro-space-4); border-radius: var(--pyro-radius); margin-bottom: var(--pyro-space-4) } .pyro-title { font-size: 2.38rem } .pyro-masthead { gap: var(--pyro-space-2) var(--pyro-space-4); min-height: 0 } .pyro-toolbar > li { flex: 1 1 auto } .pyro-toolbar .pyro-button { width: 100% } .pyro-table { display: block; overflow-x: auto } .pyro-record { grid-template-columns: 1fr; gap: var(--pyro-space-1) } .pyro-record dd { margin-bottom: var(--pyro-space-2) } .pyro-priority-peripheral { display: none } }"

    val tones: Css = Tone.values.foldLeft(Css(Nil)) { (acc, tone0) => acc + tone(tone0) }
    val accents: Css = Token.Accent.values.foldLeft(Css(Nil)) { (acc, accent0) => acc + accent(accent0) }

    theme.variables + scale + base + layout + menubar + masthead + phrasing + label + flow + charts
    + panels + controls + responsive + tones + accents
