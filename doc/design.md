# Pyrocosm: design and roadmap

Pyrocosm is a Soundness-based UI layer with one semantic model that renders to the terminal
(via Ultimatum) and to the web (via Honeycomb, Graffiti and one generic script over WebSockets),
so that Flame, Fume, Fury and Fluence are written **without any reference to a terminal or a
browser**. The same interface description renders as a TUI board *and* as a genuine web
dashboard, not as a terminal drawn into a browser.

## Why

Each application renders its own UI today. Flame has a terminal client (Ultimatum panes and
escapade) and a separate web client: several hundred lines of hand-written JavaScript over a
bespoke JSON protocol. Fume has a semantic report document (`fume.Doc`) with one terminal
renderer and a live board, and no web UI. Fury renders through a `FrontEnd` trait; Fluence emits
HTML directly. None shares an appearance, and adding a web UI to any of them means writing
JavaScript.

## Principles

1. **Semantic, not presentational.** The model says *what* something is (a keystroke, a
   duration, a failure, a dependency graph, a navigation panel), never how it looks or where it
   sits. Appearance lives in the two renderers and one shared palette.
2. **Three levels, strictly layered.** `Inline` ⊂ `Block` ⊂ `Interface`. Inline and Block
   values are pure, immutable data. Only `Interface` knows about liveness and interaction.
3. **Arrangement belongs to the renderer.** The interface level describes panels by *role*,
   *priority* and *relationship*; each renderer has its own solver that arranges them for the
   space it has. The model may carry *hints*, including hints only one renderer understands,
   but never instructions.
4. **Each medium is idiomatic.** The web uses cards, sidebars, sticky toolbars, disclosure,
   hover detail, real tables and SVG; the terminal uses panes, borders, focus rings and glyphs.
   Consistency comes from the shared palette, typographic scale, glyph vocabulary and semantic
   mapping, not from one medium imitating the other.
5. **The application is a server.** State and logic run on the JVM; a browser is a remote
   display, exactly as the terminal is. Several browser tabs, or a terminal and a browser, can
   watch the same Fume run.
6. **Reuse Soundness vocabularies**: `clavichord.Keypress`, `quantitative.Quantity`,
   `acyclicity.Dag`, `punctuation.Markdown`, `delicious.Markup`, `archimedes.Math` (Ergo),
   `digression.StackTrace`, `dissonance.Diff`, and graffiti's page features on the web.

## The model (`src/model`)

Every dependency of the model is Scala.js-capable, so it can later cross-compile for a
browser-side renderer. Two types are defined here rather than reused for that reason:

- `Token`/`Token.Accent` mirror harlequin's, because harlequin needs the compiler and is
  JVM-only. Highlighting happens at exhibition time; the model carries its result. The
  `compiler` module (JVM-only) bridges harlequin's `SourceCode` and delicious's diagnostic
  `Markup` into the model.
- `Status`/`Standing` mirror ultimatum's gauge statuses, so the model does not pull in the
  terminal stack. The terminal renderer converts.

### Wire form

TEL is the model's codec: `Inline` and `Block` carry anchored `Tel.Encodable`/`Tel.Decodable`
givens in their companions (derived in a sibling `Codecs` object; see below), so a value
serialises from any package, and BinTEL serves every JVM-to-JVM transport. JSON, if used at all,
is an edge detail of the web frontend. Several model shapes follow from being serialisable:

- Handles (`Action`, `Input`, `Choice`, `Toggle`) are case classes with a process-unique id, so
  they are plain values: an event naming one decodes to an equal handle and matches by equality.
  A frontend still validates incoming ids against those it has issued.
- `Inline.Amount(value, units)` captures a `Quantity`'s type-level units as text when the phrase
  is built (`Amount.of`), since a generic `Quantity` cannot be decoded.
- `Block.Graph(vertices, edges)` with `Graph.of(dag)` and `Graph#dag` for acyclicity's
  operations; `Status.Elapsed(seconds)` with `Elapsed.of(duration)`.
- A table cell and a code line are records (`Block.Cell`, `Block.Line`), not `List[List[_]]`: a
  repeated TEL field cannot nest, so nested lists flatten on the wire.
- `Keypress` and `Math` are scalars: clavichord's rendering (`[⌃]+[C]`, parsed back by
  `Keypresses.parse`, a candidate for clavichord itself) and Ergo shorthand.
- Every singleton-only enum (`Tone`, `Glyph`, `Standing`, accents, alignments, note styles,
  chart kinds, sizings) has a scalar codec keyed by its kebab-cased case name. This is nicer TEL
  than a nested select, and it sidesteps a compiler problem: deriving a `Tel.Decodable` for a
  sum with a singleton case fails under capture checking when the derivation is anchored to a
  `given` or `val` (propensive/soundness#1972), in an order-sensitive way. For the same reason
  the lone singleton cases `Inline.Break`, `Block.Rule` and `Status.Indeterminate` are fieldless
  products for now, and the derivations run in a sibling object with the companions holding
  only aliases.

### Inline

`Textual`, `Phrase`, `Emphasis` (one kind), `Toned(tone, …)`, `Code(language, tokens)`,
`Keystroke(keypress)`, `Link(External | Internal(action), …)`, `Math`, `Symbol(glyph)`,
`Reference(id)`, `Amount(value, units)`, `Figure(value, precision)`, `Break`.

`Tone` (Success, Failure, Warning, Muted, Accent, Info) and `Glyph` are the semantic hooks that
replace direct use of colours, weights and box characters.

### Block

`Paragraph`, `Heading`, `Listing` (items may carry an `Action`), `Quotation`, `Rule`, `Code`
(lines of tokens plus `Note` ranges for annotated samples), `Table` (columns with escritoire-style
`Sizing`; rows may carry a `Tone` and an `Action`, which is how master/detail is built),
`Record` (key/value facts), `Notice`, `Disclosure`, `Image`, `Tree`, `Graph(vertices, edges)`,
`Chart` (sparkline, bars, histogram only), `Gauge(status, caption)`, `Group`.

### Interface

An `Interface` is a title, a list of `Panel`s, global `Control`s, declared `Shortcut`s and
`Hints`. A `Panel` has a `Role` (Primary, Navigation, Detail, Inspector, Status, Log, Prompt), a
`Priority` (Essential, Important, Peripheral: what gives way first), an optional `Relation`
(`DetailOf(panel)`, `Grouped(group)`), `Live` content, its own controls and hints. Nothing says
where a panel goes.

`Control`s are `Button`, `Field` (line, multiline or code, with a `Live[Decoration]` of
highlighting tokens and completions supplied by the application), `Choice` and `Toggle`. Each
names a handle (`Action`, `Input`, `Choice`, `Toggle`) the application created, a value with a
process-unique id.

`Hints` is an open, typed bag. Three vocabularies live in the model so an application can attach
any of them without touching a renderer: `pyrocosm.hints.*` (medium-neutral: `Proportion`,
`Minimum`, `Beside`, `Below`, `Compact`, `Follow`), `hints.terminal.*` (`Occupancy`, `Border`,
`MaxRows`) and `hints.web.*` (`Span`, `Card`, `Sticky`, `Side`, `Icon`, `Collapsed`). The rule
for adding a hint: a renderer *may* honour it; an application must never depend on it.

### Liveness and events

`Live[T]` is a reactive cell modelled on ultimatum's `Reading`: assignment from any thread wakes
every frontend showing it. It carries the same localised `unsafeAssumePure` seam that `Reading`
and `Panes` use, for the same reasons.

`Event` is what a user did, in the vocabulary both media can honour: `Pressed(action)`,
`Edited(input, text, caret)`, `Submitted(input, text)`, `Chosen`, `Toggled`, `Key(keypress)`,
`Resized`, `Closed`. An application is one `handle(event)` function that mutates `Live` cells,
run under a `Frontend`:

```scala
trait Frontend:
  def run(interface: Interface)(handle: Event => Unit): Unit
```

### `Presentable`

```scala
trait Presentable extends Typeclass.Pure, Formal:
  type Form <: Inline | Block
  def exhibit(value: Self): Form
```

`value.exhibit` sits beside `show`, `inspect`, `teletype` and `render`. The verb is `exhibit`
because `present` is `Optional#present`. Resolution follows `Inspectable`: own instance, then
`Showable` as text, then a wisteria-derived structural rendering (product → `Record`, sum by
variant), then `toString`. Instances for Soundness types live in the companion: `Text`, numbers,
`Keypress`, `Quantity`, `Message`, `Error`, `StackTrace`, `List[T]`, `Markdown of Layout`.

## Renderers

### Terminal (`src/terminal`, on Ultimatum)

`Block → List[Teletype]` at a width (tessellate for wrapping, escritoire for tables, dendrology
for trees and graphs, ultimatum gauges, harlequin's teletype for tokens, Ergo text for maths).
The **arrangement solver** chooses from a small family of pane arrangements by role, priority,
`Minimum` hints and the current columns and rows: `Navigation` to a verso menu strip, `Primary`
to the main pane, `Detail`/`Inspector` to a recto strip when width allows (otherwise below,
otherwise behind a key), `Status` to a one-row bar, `Log`/`Prompt` to the bottom. `Peripheral`
panels are dropped first as space shrinks, then `Important`; `Essential` always shows. Panels
become `Pane.Widget`s whose `Fixture` re-renders their `Live` blocks; `Form.run` supplies the
event loop, Tab focus ring, throttled repaint, and inline/fullscreen occupancy.

### Web (`src/web`, on Honeycomb, Cataclysm, Graffiti, Perihelion)

`Block → Html of Flow` with semantic HTML and a checked stylesheet; web-native enrichment
follows from semantics alone (sortable tables, hover precision on amounts, disclosure, toasts
for live notices). The **arrangement solver** maps roles onto graffiti's page features:
`title` + `Status` → `Masthead`; `Navigation` → `VersoPanel`; `Detail`/`Inspector` →
`FoldableRectoPanel`; `Primary` → `Mainstay`, or `Dashboard` cards when there are several;
controls → a sticky toolbar; `Log` → a following card; `Prompt` → a docked bar. What the
terminal *drops* at narrow widths the web *collapses*.

One stylesheet, with the palette as CSS custom properties from the same iridescence colours the
terminal uses. One WebSocket per tab carrying JSON patches (`Replace`, `Append`, `Enable`,
`Decorate`, `Focus`, `Notify`) down and (`Press`, `Edit`, `Submit`, `Choose`, `Toggle`, `Key`,
`Resize`) up, coalesced per animation frame. One generic script, `res/web/pyrocosm.js`, with
nothing app-specific in it.

## The biggest challenges

1. **One description, two arrangement solvers.** The vocabulary must be rich enough for a Fume
   run to read as a dashboard on the web and a board in the terminal, yet small enough that
   applications do not smuggle layout in through hints. Expect revision after Fume and Flame;
   keep the solvers as rule tables. The gallery at several widths in both media is the judge.
2. **Two focus and input models.** Terminal: one focused widget, every key is ours. Browser:
   native focus, Tab and Ctrl-chords belong to the browser. Shortcuts are declared so the web
   intercepts only those; actionable rows give both media a native selection idiom.
3. **The code editor** (Flame): per-keystroke events, server-side decorations, completions,
   incomplete-line detection, in both `LineEditor` and contenteditable.
4. **Patch granularity.** `Live` cells are the unit of update; start coarse and coalesce.
5. **Capture checking.** `Live` needs the `Reading` seam; WebSocket handlers need the sealing
   already worked out in `flame_web.scala`; separation checking stays off in `web` for now.
6. **Graphs and charts on the web** in SVG, reusing dendrology's lane layout for topology.
7. **Highlighting is JVM-only**, hence tokens in the model.
8. **Sessions**: per-connection (Flame) and shared (Fume) interfaces from one `WebFrontend`.
9. **Degradation** to plain text and Markdown; keep `Block → Text` from day one.
10. **Verification** with yossarian `Pty` snapshots and tarantula screenshots of the gallery.

Found while building the model (M1):

- Two files whose names differ only in case (`pyrocosm.Hints.scala`, `pyrocosm.hints.scala`)
  collide on macOS; package-level files are named `pyrocosm_hints.scala`, as Soundness does.
- The fork compiler's `wildApprox` assertion fires on unannotated lambda parameters in
  murmuration `map`/`bind` calls; annotate them, as flame does.
- `Quantity[? <: Measure]` as a field type loses its bound; the `Amount` case is type-parameterised
  instead.
- A field named `notify` collides with `Object#notify`.

## Roadmap

- **M0 Scaffold** (done): `build.mill` on the flame pattern, modules `model`, `terminal`,
  `web`, `demo`, `test`; Makefile; shared CI workflow.
- **M1 Model** (done): the types above; `Presentable` with its fallback chain, product and sum
  derivation, and table derivation for lists of case classes; instances for Soundness types;
  TEL codecs for `Inline` and `Block` with a full-model round-trip test; the `compiler` module
  for harlequin and delicious. Not yet: stenography (types in diagnostics) and a schema
  fingerprint for version skew, as `probably.Streamer` does.
- **M2 Terminal renderer** (done, first cut): `TerminalRenderer` renders every block to styled
  lines at a width (tessellate, escritoire, dendrology, ultimatum gauges) with a plain-text
  path; `TerminalArrangement.plan` is the rule table from roles, priorities and `Minimum`
  hints to verso, centre, recto, log, prompt and status bands, and `build` makes the pane
  tree; `TerminalFrontend` conducts it on Ultimatum's `Form`, binding every `Live` cell to the
  form's redraw wake. Fixtures: `PanelFixture` (live content, selectable actions, scrolling),
  `ButtonFocus`, `ToggleFocus`, `ChoiceFocus` and `CodeField` (decorations, ghost text,
  completions, history, Enter-versus-newline). Two Ultimatum limits shaped it: `bindWake` is
  private to ultimatum, so the frontend binds cells itself and panels report a period so a
  fullscreen form repaints them; and Tab is the form's own focus key, so Right accepts a
  completion. The gallery's `static` mode prints the overview without a session.
  Not yet: inline links as focusables, mouse, and the `Resized` event.
- **M3 Web renderer** (done, first cut): `HtmlRenderer` renders every node to semantic HTML
  carrying `pyro-*` classes and handle ids; `WebArrangement.plan` maps roles to page features
  and `PyrocosmPage` is the graffiti page (masthead with title, status panels and a connection
  pill; global controls as the top menu; navigation verso; detail recto; primary, log and
  prompt as cards); `WebStyles` is the one stylesheet and `WebTheme` the palette; `WebFrontend`
  serves the page and script, binds every `Live` cell to a patch broadcast over one WebSocket
  per tab, and validates every incoming id against the interface's handles; `pyrocosm.js` is
  the generic script. Patches so far: `replace`, `value`, `enable`/`disable`, `check`/`uncheck`,
  `select`. `gallery serve [port]` runs the gallery on it, from the same `Session` as the
  terminal. Found while building it:
  - Cataclysm's `css"…"` checks a substitution against the property's MDN grammar and only
    accepts a hole as a *whole* value, so `border: 1px solid $colour` is rejected, and a lone
    colour is never valid for `background` (its grammar has a mandatory comma; use
    `background-color`). Hence the palette is emitted as the design said: `WebTheme.variables`
    is a `:root` block of `--pyro-*` custom properties, read from text at runtime (the parser
    accepts any value for a `--` property, and no interpolator knows custom properties), and
    every rule refers to `var(--pyro-…)` as literal text, which always validates. Per-case
    rules (tones, accents) are read from text the same way.
  - `urticose.Service` is stopped with `cancel()`; `Channel#send` `logs Websocket.Event`, so
    the frontend imports `silentLogging`; honeycomb boolean attributes take `= true`.
  - Reading CSS needs a `Diagnostics` given (`fulminate.errorDiagnostics`) as well as a
    `Tactic[Css.Errors]`; showing it needs a `cataclysm.formatting` given.
  Not yet: graphs as SVG (a graph is a dependency list), sortable tables, per-connection
  sessions, `Focus` and `Notify` patches, the `Resized` event, a `Decorate` patch finer than
  replacing the completions list, and clean shutdown on Ctrl+C under the Ethereal launcher.
- **M4 Gallery**: every node, every role and priority, a ticking gauge, a selectable table
  driving a detail panel, a code field with fake decorations; `demo terminal` and
  `demo serve` (both exist, driven by one `Session`); rendered at 40/80/160 columns and
  phone/laptop/wide viewports. Permanent visual regression fixture. Still to do: the width and
  viewport renderings, and tarantula screenshots.
- **M5 Fume**: `Doc.Document` → blocks; an `Interface` of results, log, status and detail
  panels fed from `Model.handle`; `fume serve`; palette and figures move here.
- **M6 Flame**: log panel of records and notices; a `Prompt` panel with `Field.Code(Scala)`
  decorated from `Repl.tokenize`; sessions as navigation; delete `replScript` and the
  `WebRequest`/`WebReply` protocol; `Repl.Rendering` collapses to `Presentable`.
- **M7 Fury and Fluence**: Fury's `FrontEnd` as an interface with the target DAG as a graph;
  Fluence pages as blocks with the API tree as navigation.
- **M8 Hardening**: Markdown renderer, themes, ARIA from roles, optional Scala.js client,
  finer `Live` granularity if profiling requires, hint vocabulary revised from real use.

## Decisions

- Arrangement is renderer-owned; the model carries roles, priorities, relations and hints.
- The browser side is a thin generic script, not Scala.js, for now.
- Reactive cells plus an event stream, not Elm-style pure views.
- Fume before Flame: Fume's `Doc` is already the block model; Flame's editor is the hardest
  control.
- Pyrocosm is its own repository depending on published Soundness components; the typeclass and
  every instance live here.
- `Presentable` / `exhibit`; `portray` is the fallback verb.
