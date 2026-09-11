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
import symbolism.*
import spectacular.*
import turbulence.*

import contingency.strategies.throwUnsafely
import fulminate.errorDiagnostics.emptyDiagnostics

// The one stylesheet. Every rule is a `css"…"` value, validated as the code compiles; every
// colour is a `var(--pyro-…)` reference to the theme's `:root` block, so a second theme is a
// second block and no rule changes. Layout is a fluid dashboard: the page features (masthead,
// panels) come from graffiti; these rules style the model's nodes and the panels' cards.
object WebStyles:
  def css(theme: WebTheme): Css =
    // A tone's colour, its notice's edge and a tinted row; an accent's colour. These rules are
    // generated per case, so they are read from text like the theme's `:root` block; the tint
    // is a `color-mix` of the variable, which browsers resolve.
    def tone(tone: Tone): Css =
      val name = HtmlRenderer.toneClass(tone)
      val variable = t"var(--pyro-tone-${tone.toString.tt.lower})"
      t".$name { color: $variable } .pyro-notice.$name { border-left-color: $variable } tr.$name > td { background-color: color-mix(in srgb, $variable 12%, transparent) }"
      . read[Css]

    def accent(accent: Token.Accent): Css =
      t".${HtmlRenderer.accentClass(accent)} { color: var(--pyro-accent-${accent.toString.tt.lower}) }".read[Css]

    val base: Css =
      css"body { margin: 0; background-color: var(--pyro-bg); color: var(--pyro-fg); font-family: system-ui, sans-serif; line-height: 1.45 }"
        + css"code, kbd, pre, .pyro-figure, .pyro-reference, .pyro-sparkline { font-family: ui-monospace, monospace }"
        + css"a { color: var(--pyro-link) }"
        + css"h1, h2, h3, h4 { margin: 0.4rem 0; line-height: 1.2 }"
        + css"h1 { font-size: 1.5rem } h2 { font-size: 1.2rem } h3, h4 { font-size: 1rem }"
        + css"p { margin: 0.4rem 0 }"
        + css"hr { border: 0; border-top: 1px solid var(--pyro-border) }"
        + css"hr.pyro-rule-above { border-top: 0; border-bottom: 1px solid var(--pyro-border); margin-bottom: 0 }"
        + css"hr.pyro-rule-below { margin-top: 0 }"

    val phrasing: Css =
      css"em { font-weight: 600; font-style: normal }"
        + css".pyro-key { display: inline-block; padding: 0 0.35em; border: 1px solid var(--pyro-border); border-bottom-width: 2px; border-radius: 0.3em; font-size: 0.85em; color: var(--pyro-key) }"
        + css".pyro-reference { color: var(--pyro-reference); font-size: 0.9em }"
        + css".pyro-figure { color: var(--pyro-figure); font-variant-numeric: tabular-nums }"
        + css".pyro-units { color: var(--pyro-units); font-size: 0.85em }"
        + css".pyro-binding { font-style: italic }"
        + css".pyro-glyph-check { color: var(--pyro-tone-success) } .pyro-glyph-cross { color: var(--pyro-tone-failure) }"
        + css".pyro-glyph-warning, .pyro-glyph-winner { color: var(--pyro-tone-warning) } .pyro-glyph-pending { color: var(--pyro-muted) }"
        + css".pyro-math { font-family: math, serif }"

    val flow: Css =
      css".pyro-codeblock { background-color: var(--pyro-surface); padding: 0.6rem 0.8rem; border-radius: 0.4rem; overflow-x: auto; margin: 0.5rem 0 }"
        + css".pyro-note-erroneous { text-decoration: underline wavy var(--pyro-tone-failure) }"
        + css".pyro-note-caution { text-decoration: underline wavy var(--pyro-tone-warning) }"
        + css".pyro-note-highlight { background-color: var(--pyro-selection); border-radius: 0.2em }"
        + css".pyro-note-param { font-style: italic }"
        + css".pyro-table { border-collapse: collapse; margin: 0.5rem 0; width: 100% }"
        + css".pyro-table th, .pyro-table td { padding: 0.3rem 0.6rem; border-bottom: 1px solid var(--pyro-border); text-align: left; vertical-align: top }"
        + css".pyro-table th { color: var(--pyro-muted); font-weight: 600; font-size: 0.85rem; text-transform: uppercase; letter-spacing: 0.04em }"
        + css".pyro-table caption { caption-side: bottom; color: var(--pyro-muted); font-style: italic; font-size: 0.85rem; padding: 0.3rem }"
        + css".pyro-align-end, .pyro-numeric { text-align: right }"
        + css".pyro-rigid { white-space: nowrap; width: 1% }"
        + css".pyro-stretch { width: 100% }"
        + css".pyro-record { display: grid; grid-template-columns: max-content 1fr; gap: 0.2rem 1rem; margin: 0.5rem 0 }"
        + css".pyro-record dt { color: var(--pyro-muted); font-weight: 600 } .pyro-record dd { margin: 0 }"
        + css".pyro-notice { border-left: 4px solid var(--pyro-border); padding: 0.4rem 0.8rem; margin: 0.6rem 0; background-color: var(--pyro-surface); border-radius: 0 0.4rem 0.4rem 0 }"
        + css".pyro-notice header { margin-bottom: 0.2rem }"
        + css".pyro-quotation { border-left: 3px solid var(--pyro-muted); margin: 0.5rem 0; padding-left: 0.8rem; color: var(--pyro-muted) }"
        + css".pyro-disclosure summary { cursor: pointer; color: var(--pyro-muted) }"
        + css".pyro-tree, .pyro-tree ul { list-style: none; padding-left: 1.2rem; border-left: 1px dotted var(--pyro-border) }"
        + css".pyro-graph { list-style: none; padding: 0 } .pyro-arrow { color: var(--pyro-muted) }"
        + css".pyro-image img { max-width: 100%; border-radius: 0.4rem } .pyro-image figcaption { color: var(--pyro-muted); font-size: 0.85rem }"

    val charts: Css =
      css".pyro-chart { display: grid; gap: 0.3rem; margin: 0.5rem 0 }"
        + css".pyro-series { display: grid; grid-template-columns: max-content 1fr; gap: 0.2rem 0.8rem; align-items: center }"
        + css".pyro-series-label { color: var(--pyro-muted) }"
        + css".pyro-bar-row { display: flex; align-items: center; gap: 0.5rem }"
        + css".pyro-bar { height: 0.8rem; background-color: var(--pyro-tone-accent); border-radius: 0.2rem; min-width: 2px }"
        + css".pyro-sparkline { color: var(--pyro-tone-info); letter-spacing: 0.05em }"
        + css".pyro-gauge { margin: 0.4rem 0 } .pyro-caption { color: var(--pyro-muted); font-size: 0.85rem }"
        + css".pyro-progress, .pyro-reckoning progress { width: 100%; accent-color: var(--pyro-tone-accent); height: 0.6rem }"
        + css".pyro-steps { list-style: none; padding: 0 } .pyro-steps li { padding: 0.1rem 0 }"
        + css".pyro-standing-succeeded .pyro-standing { color: var(--pyro-tone-success) }"
        + css".pyro-standing-failed .pyro-standing { color: var(--pyro-tone-failure) }"
        + css".pyro-standing-running .pyro-standing { color: var(--pyro-tone-accent) }"
        + css".pyro-standing-pending .pyro-standing { color: var(--pyro-muted) }"

    val panels: Css =
      css".pyro-panel { background-color: var(--pyro-surface); border: 1px solid var(--pyro-border); border-radius: 0.6rem; padding: 0.8rem 1rem; margin: 0 0 1rem 0; overflow: auto }"
        + css".pyro-panel > h2 { color: var(--pyro-muted); font-size: 0.8rem; text-transform: uppercase; letter-spacing: 0.06em; margin: 0 0 0.5rem 0 }"
        + css".pyro-role-log .pyro-panel-content { max-height: 16rem; overflow-y: auto }"
        + css".pyro-role-status .pyro-panel { padding: 0.4rem 1rem }"
        + css".pyro-action { cursor: pointer } .pyro-action:hover { background-color: var(--pyro-selection) }"
        + css".pyro-selected { outline: 2px solid var(--pyro-tone-accent) }"
        + css".pyro-controls { display: flex; gap: 0.6rem; flex-wrap: wrap; align-items: center; margin-top: 0.6rem }"
        + css".pyro-button { background-color: var(--pyro-surface); color: var(--pyro-fg); border: 1px solid var(--pyro-border); border-radius: 0.4rem; padding: 0.3rem 0.8rem; cursor: pointer; font: inherit }"
        + css".pyro-button:hover { border-color: var(--pyro-tone-accent) } .pyro-button:disabled { color: var(--pyro-muted); cursor: default }"
        + css".pyro-field { width: 100%; box-sizing: border-box; background-color: var(--pyro-bg); color: var(--pyro-fg); border: 1px solid var(--pyro-border); border-radius: 0.4rem; padding: 0.5rem; font: inherit; resize: vertical }"
        + css".pyro-field-holder { position: relative }"
        + css".pyro-editor { display: block; white-space: pre-wrap; word-break: break-word; font-family: ui-monospace, monospace; min-height: 1.4em; outline: none; caret-color: var(--pyro-fg) }"
        + css".pyro-ghost { color: var(--pyro-muted) }"
        + css".pyro-placeholder { display: none; position: absolute; top: 0.5rem; left: 0.5rem; color: var(--pyro-muted); pointer-events: none; font-family: ui-monospace, monospace }"
        + css".pyro-empty .pyro-placeholder { display: block }"
        + css".pyro-note { font-size: 0.85rem; margin-top: 0.2rem } .pyro-note p { margin: 0.1rem 0 }"
        + css".pyro-output { white-space: pre-wrap; word-break: break-word; margin: 0.3rem 0; font-family: ui-monospace, monospace }"
        + css".pyro-gutter-out { color: var(--pyro-tone-info) } .pyro-gutter-err { color: var(--pyro-tone-failure) }"
        + css".pyro-completions { list-style: none; margin: 0.2rem 0 0 0; padding: 0; font-family: ui-monospace, monospace; font-size: 0.9rem }"
        + css".pyro-completions li { padding: 0.1rem 0.5rem; cursor: pointer } .pyro-completions li.pyro-selected { background-color: var(--pyro-selection) }"
        + css".pyro-completions .pyro-signature { color: var(--pyro-muted); margin-left: 1rem }"
        + css".pyro-status-bar { display: flex; gap: 2rem; align-items: center; color: var(--pyro-muted); font-size: 0.9rem }"
        + css".pyro-title { font-weight: 700; font-size: 1.1rem; color: var(--pyro-fg) }"
        + css".pyro-connection { margin-left: auto; font-size: 0.8rem; padding: 0.1rem 0.6rem; border-radius: 1rem; border: 1px solid var(--pyro-border) }"
        + css".pyro-connection.pyro-online { color: var(--pyro-tone-success) } .pyro-connection.pyro-offline { color: var(--pyro-tone-warning) }"
        + css"@media (max-width: 60rem) { .pyro-priority-peripheral { display: none } }"

    val tones: Css = Tone.values.foldLeft(Css(Nil)) { (acc, tone0) => acc + tone(tone0) }
    val accents: Css = Token.Accent.values.foldLeft(Css(Nil)) { (acc, accent0) => acc + accent(accent0) }

    theme.variables + base + phrasing + flow + charts + panels + tones + accents
