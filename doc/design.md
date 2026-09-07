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
  JVM-only. Highlighting happens at exhibition time; the model carries its result.
- `Status`/`Standing` mirror ultimatum's gauge statuses, so the model does not pull in the
  terminal stack. The terminal renderer converts.

### Inline

`Textual`, `Phrase`, `Emphasis` (one kind), `Toned(tone, …)`, `Code(language, tokens)`,
`Keystroke(keypress)`, `Link(External | Internal(action), …)`, `Math`, `Symbol(glyph)`,
`Reference(id)`, `Amount(quantity)`, `Figure(value, precision)`, `Break`.

`Tone` (Success, Failure, Warning, Muted, Accent, Info) and `Glyph` are the semantic hooks that
replace direct use of colours, weights and box characters.

### Block

`Paragraph`, `Heading`, `Listing` (items may carry an `Action`), `Quotation`, `Rule`, `Code`
(lines of tokens plus `Note` ranges for annotated samples), `Table` (columns with escritoire-style
`Sizing`; rows may carry a `Tone` and an `Action`, which is how master/detail is built),
`Record` (key/value facts), `Notice`, `Disclosure`, `Image`, `Tree`, `Graph(Dag[Vertex])`,
`Chart` (sparkline, bars, histogram only), `Gauge(status, caption)`, `Group`.

### Interface

An `Interface` is a title, a list of `Panel`s, global `Control`s, declared `Shortcut`s and
`Hints`. A `Panel` has a `Role` (Primary, Navigation, Detail, Inspector, Status, Log, Prompt), a
`Priority` (Essential, Important, Peripheral: what gives way first), an optional `Relation`
(`DetailOf(panel)`, `Grouped(group)`), `Live` content, its own controls and hints. Nothing says
where a panel goes.

`Control`s are `Button`, `Field` (line, multiline or code, with a `Live[Decoration]` of
highlighting tokens and completions supplied by the application), `Choice` and `Toggle`. Each
names an opaque handle (`Action`, `Input`, `Choice`, `Toggle`) the application created, with
reference identity.

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
- **M1 Model** (in progress): the types above compile; `Presentable` with its fallback chain
  and derivation; instances for Soundness types. Remaining: TEL codecs (stratiform) for
  Inline/Block, derived in place (recursive sums derive without anchoring; verified for JSON and
  TEL in the test suite), BinTEL on JVM transports, JSON only at the browser edge; a `scala` module for harlequin/delicious/stenography instances; tests.
- **M2 Terminal renderer**: `TerminalRenderer` (`Block → List[Teletype]`),
  `TerminalArrangement`, `TerminalFrontend` on `Form.run`, `PanelFixture`, `ButtonFocus`,
  `SelectableFocus`, `CodeField`; a plain-text renderer for terse output.
- **M3 Web renderer**: `HtmlRenderer`, `Stylesheet`, `WebArrangement` on graffiti,
  `PyrocosmPage`, `WebFrontend` with the patch protocol, `res/web/pyrocosm.js`.
- **M4 Gallery**: every node, every role and priority, a ticking gauge, a selectable table
  driving a detail panel, a code field with fake decorations; `demo terminal` and
  `demo serve`; rendered at 40/80/160 columns and phone/laptop/wide viewports. Permanent
  visual regression fixture.
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
