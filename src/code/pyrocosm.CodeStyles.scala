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

import errorDiagnostics.emptyDiagnostics
import strategies.throwUnsafely

// The rules of a block of code as `CodeRenderer` writes it: the block, its header and tabs, its
// notes and markers, and an animated block's atoms, steps and timeline. No rule names a colour,
// a face or a length of its own: each is a custom property, which the page declares — the web
// module's theme for a Pyrocosm application, and its own for any other page, which so gives
// code its own look. They are:
//
//   --pyro-code-bg, --pyro-code-fg     the block's ground and its text
//   --pyro-code-accent-<accent>        a token's colour, for each `Token.Accent` in lower case
//   --pyro-fg, --pyro-muted            the current step's label, and the others; line numbers
//   --pyro-surface, --pyro-rule        the timeline's ground and its lines
//   --pyro-selection                   a highlight outside a block
//   --pyro-tone-accent                 the current step, a focused block, a highlight's marker
//   --pyro-on-accent                   the number on the current step
//   --pyro-tone-warning, -failure      a caution's and an error's marks
//   --pyro-radius                      the block's corners
//   --pyro-space-1 … -4                the spacing scale
//   --pyro-text-small, --pyro-font-label   the size and face of tabs, steps and the timeline
//
// The face and size of the code itself are the page's, as any `pre`'s are.
object CodeStyles:
  // Every rule: the accents, then the blocks'.
  def rules: Css = accents + flow

  // A token's colour within a block, by its accent; a binding, and a comment, are set apart.
  def accents: Css =
    val coloured: List[Text] =
      Token.Accent.values.foldLeft(Nil: List[Text]): (rules: List[Text], accent: Token.Accent) =>
        val name = CodeRenderer.accentClass(accent)
        val lower = accent.toString.tt.lower
        t".pyro-codeblock .$name { color: var(--pyro-code-accent-$lower) }" :: rules

    (t".pyro-binding { font-style: italic }" :: coloured).join(t" ").read[Css]

  // The blocks' rules, which the web module's sheet has among its own, where they always were.
  def flow: Css =
      // The block's text aligns with the prose around it: its ground extends a gutter's
      // width beyond the text on either side, by a negative margin the padding cancels.
      css"""
        .pyro-codeblock {
          background-color: var(--pyro-code-bg); color: var(--pyro-code-fg); border: 0;
          border-radius: var(--pyro-radius); padding: var(--pyro-space-3) var(--pyro-space-4);
          margin: 0 calc(-1 * var(--pyro-space-4)) var(--pyro-space-4) calc(-1 * var(--pyro-space-4));
          overflow-x: auto; line-height: 1.5
        }
      """ +
        // A code block with a header — its path, and the tabs of its alternatives — in a strip
        // above it, of the block's own colours, the block flush beneath it. The tabs are set
        // as a table's headings are, the current one in the code's colour and underlined.
        css"""
          .pyro-code-group {
            margin: 0 calc(-1 * var(--pyro-space-4)) var(--pyro-space-4) calc(-1 * var(--pyro-space-4))
          }
        """ +
        css"""
          .pyro-code-group .pyro-codeblock {
            margin: 0; border-top-left-radius: 0; border-top-right-radius: 0
          }
        """ +
        css"""
          .pyro-code-header {
            display: flex; align-items: baseline; gap: var(--pyro-space-4);
            background-color: var(--pyro-code-bg); color: var(--pyro-code-fg);
            border-radius: var(--pyro-radius) var(--pyro-radius) 0 0;
            padding: var(--pyro-space-2) var(--pyro-space-4) 0 var(--pyro-space-4);
            border-bottom: 1px solid color-mix(in srgb, var(--pyro-code-fg) 18%, transparent)
          }
        """ +
        css"""
          .pyro-code-path {
            font-family: var(--pyro-font-label); font-size: var(--pyro-text-small); font-weight: 600;
            letter-spacing: 0.03em; padding-bottom: var(--pyro-space-2); margin-right: auto
          }
        """ +
        css"""
          .pyro-code-tabs {
            list-style: none; margin: 0; padding: 0; display: flex; gap: var(--pyro-space-4)
          }
        """ +
        css"""
          .pyro-code-tab button {
            font-family: var(--pyro-font-label); font-size: var(--pyro-text-small); font-weight: 600;
            letter-spacing: 0.06em; text-transform: uppercase; line-height: 1.25;
            color: color-mix(in srgb, var(--pyro-code-fg) 55%, transparent);
            background-color: transparent; border: 0; border-bottom: 2px solid transparent;
            padding: 0 0 calc(var(--pyro-space-2) - 2px) 0; cursor: pointer;
            transition: color 0.3s ease, border-color 0.3s ease
          }
        """ +
        css".pyro-code-tab button:hover { color: var(--pyro-code-fg) }" +
        css"""
          .pyro-code-tab.pyro-current button {
            color: var(--pyro-code-fg); border-bottom-color: var(--pyro-code-fg); cursor: default
          }
        """ +
        // An animated alternative's steps sit beneath the block as they do elsewhere, which the
        // group's bleed would otherwise put at the page's edge, and the block's lost margin
        // under the block.
        css"""
          .pyro-code-group .pyro-animation-steps {
            margin: var(--pyro-space-2) 0 0 0; padding: 0 var(--pyro-space-4)
          }
        """ +
        css".pyro-alternative { display: none }" +
        css".pyro-alternative.pyro-current { display: block }" +
        css"""
          .pyro-note-erroneous {
            text-decoration: underline wavy var(--pyro-tone-failure);
            background-color: color-mix(in srgb, var(--pyro-tone-failure) 15%, transparent)
          }
        """ +
        css".pyro-note-caution { text-decoration: underline wavy var(--pyro-tone-warning) }" +
        css".pyro-note-highlight { background-color: var(--pyro-selection); border-radius: 2px }" +
        css"""
          .pyro-codeblock .pyro-note-highlight {
            background-color: color-mix(in srgb, var(--pyro-code-fg) 22%, transparent)
          }
        """ +
        css"""
          .pyro-codeblock .pyro-note-erroneous {
            text-decoration-color: var(--pyro-code-accent-error);
            background-color: color-mix(in srgb, var(--pyro-code-accent-error) 15%, transparent)
          }
        """ +
        css".pyro-note-param { font-style: italic }" +
        // An animated block: a step fades its departing atoms out, moves the survivors, then
        // fades the arrivals in, each phase after the last (the durations are the script's);
        // the data the script reads is not shown.
        css".pyro-animated { position: relative; outline: none }" +
        css"""
          .pyro-animated:focus-visible .pyro-codeblock { box-shadow: 0 0 0 2px var(--pyro-tone-accent) }
        """ +
        css"""
          .pyro-atom {
            display: inline-block; white-space: pre; border-radius: 2px;
            text-decoration: underline wavy transparent;
            transition: opacity 0.75s ease, background-color 0.75s ease, text-decoration-color 0.75s ease
          }
        """ +
        // A frame's marks, on whole atoms: they fade out before a step and the target's fade
        // in after it, by the same transition; a marker row beneath a line is a block, as the
        // lines are, and fades likewise.
        css"""
          .pyro-marked-erroneous {
            text-decoration-color: var(--pyro-code-accent-error);
            background-color: color-mix(in srgb, var(--pyro-code-accent-error) 15%, transparent)
          }
        """ +
        css".pyro-marked-caution { text-decoration-color: var(--pyro-tone-warning) }" +
        css"""
          .pyro-marked-highlight {
            background-color: color-mix(in srgb, var(--pyro-code-fg) 22%, transparent)
          }
        """ +
        css".pyro-marked-param { font-style: italic }" +
        // A comment is set apart from the code, as is text standing in for code.
        css".pyro-accent-comment, .pyro-accent-substitute { font-style: italic }" +
        // A block with a focus mark fades every token but the focused ones towards the
        // background: a plain block's pieces, which a note wraps, and an animated block's
        // atoms, which carry their mark and already transition their opacity, so the fading
        // comes and goes with the frame's marks.
        css"""
          .pyro-focusing code span[class*="pyro-accent-"]:not(.pyro-note-focus *):not(.pyro-marked-focus) {
            opacity: 0.3; transition: opacity 0.75s ease
          }
        """ +
        css".pyro-animated .pyro-marker { display: block; transition: opacity 0.75s ease }" +
        // The page's timeline, which the script makes when any frame has a time: a bar at the
        // foot of the page with play, the clock and a scrubber.
        css"""
          .pyro-timeline {
            position: fixed; bottom: var(--pyro-space-4); left: 50%; transform: translateX(-50%);
            z-index: 10; display: flex; align-items: center; gap: var(--pyro-space-3);
            padding: var(--pyro-space-2) var(--pyro-space-4); border: 1px solid var(--pyro-rule);
            border-radius: var(--pyro-radius); background-color: var(--pyro-surface);
            font-family: var(--pyro-font-label); font-size: var(--pyro-text-small)
          }
        """ +
        css"""
          .pyro-timeline button {
            font: inherit; line-height: 1; padding: 0 var(--pyro-space-2);
            border: 1px solid var(--pyro-rule); border-radius: var(--pyro-radius);
            background-color: transparent; color: inherit; cursor: pointer
          }
        """ +
        css".pyro-timeline-clock { font-variant-numeric: tabular-nums; min-width: 5ch }" +
        css".pyro-timeline input { width: 14rem }" +
        // A line of an animated block is a block (an empty one keeps a line's height); the
        // block itself transitions between heights as lines appear and vanish.
        css".pyro-animated .pyro-line { display: block; min-height: 1.5em }" +
        css".pyro-animated .pyro-codeblock { will-change: height }" +
        css".pyro-animation-data { display: none }" +
        // An animated block's steps, beneath it, read down in order: each slide's number in a
        // circle beside its label. The current step's circle is filled with the accent and its
        // label is at full strength; the others are muted, and lift on hover, since a click on
        // one animates the block to it.
        css"""
          .pyro-animation-steps {
            list-style: none; padding: 0; display: flex; flex-direction: column;
            gap: var(--pyro-space-1); margin: calc(-1 * var(--pyro-space-3)) 0 var(--pyro-space-4) 0
          }
        """ +
        css"""
          .pyro-animation-step {
            display: flex; align-items: baseline; gap: var(--pyro-space-2);
            color: var(--pyro-muted); cursor: pointer; transition: color 0.3s ease
          }
        """ +
        css".pyro-animation-step.pyro-current { color: var(--pyro-fg); cursor: default }" +
        css".pyro-animation-step:not(.pyro-current):hover { color: var(--pyro-fg) }" +
        css"""
          .pyro-animation-number {
            flex: none; display: inline-flex; align-items: center; justify-content: center;
            width: 1.7em; height: 1.7em; padding: 0; border-radius: 50%;
            border: 1.5px solid currentColor; background-color: transparent; color: inherit;
            font-family: var(--pyro-font-label); font-size: var(--pyro-text-small);
            line-height: 1; cursor: inherit;
            transition: background-color 0.3s ease, border-color 0.3s ease, color 0.3s ease
          }
        """ +
        css"""
          .pyro-animation-step:not(.pyro-current):hover .pyro-animation-number {
            border-color: var(--pyro-tone-accent); color: var(--pyro-tone-accent)
          }
        """ +
        css"""
          .pyro-animation-step.pyro-current .pyro-animation-number {
            background-color: var(--pyro-tone-accent); border-color: var(--pyro-tone-accent);
            color: var(--pyro-on-accent); font-weight: 700
          }
        """ +
        css"""
          .pyro-animation-number:focus-visible {
            outline: 2px solid var(--pyro-tone-accent); outline-offset: 2px
          }
        """ +
        // A numbered block's margin: the numbers are not part of the code, so a selection
        // leaves them out; a marker row beneath a line keeps the line's columns and wraps
        // its caption, a diagnostic's at the size of the numbers.
        css"""
          .pyro-lineno {
            display: inline-block; user-select: none; color: var(--pyro-muted);
            font-size: 0.75em; vertical-align: baseline;
            border-right: 1px solid color-mix(in srgb, var(--pyro-code-fg) 25%, transparent);
            padding-right: 0.75ch; margin-right: 1ch; text-align: right
          }
        """ +
        css".pyro-marker { white-space: pre-wrap } .pyro-marker .pyro-marks { letter-spacing: 0 }" +
        css".pyro-marker-caption { font-size: 0.75em }" +
        css"""
          .pyro-marker-erroneous { color: var(--pyro-code-accent-error) }
          .pyro-marker-caution { color: var(--pyro-tone-warning) }
        """ +
        css"""
          .pyro-marker-highlight { color: var(--pyro-tone-accent) }
          .pyro-marker-param { color: var(--pyro-muted); font-style: italic }
        """
