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
`Chart` (sparkline, bars, histogram only), `Gauge(status, caption)`, `Trace` (an exception's
stack trace and its causes, as the model's own frames: `Block.Trace.of` converts digression's
`StackTrace`), `Group`, `Output`.

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
`Keypress`, `Quantity`, `Message`, `Error`, `StackTrace` and `Throwable` (both as a
`Block.Trace`), `List[T]`, `Markdown of Layout`.

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

One stylesheet, with the palette as CSS custom properties: `WebTheme.Ember` (the default: a
light, warm page with flame accents) or `WebTheme.SolarizedDark`, the terminal's colours. The
type is Marcellus for titles, Inter for text and IBM Plex Mono for code, from Google Fonts with
system fallbacks; text is one size but for small tracked labels and code. The spacing, radii,
measures and shadows are custom properties too (`--pyro-space-*`, `--pyro-measure`), so every
rule names a step of the scale. The masthead and its toolbar of global controls span the page;
the matter is centred within a measure, wider when a side column is present; a side column is
drawn only when a panel is arranged there; and every column stacks on a narrow viewport. One
WebSocket per tab carrying JSON patches (`Replace`, `Append`, `Enable`,
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

## Metadata in git notes (`src/notes`)

Every tool attaches metadata to source states through git notes, with one library,
`pyrocosm-notes`, so that no tool names a git command. A **Pyrocosm hash** (`Fingerprint`) is
the hash of a git tree: a commit's tree with a `Filter`'s exclusions removed (`read-tree` into a
throwaway index, `update-index --force-remove`, `write-tree`). Several commits share one
fingerprint (rebases, doc-only changes, squash merges) and a commit has as many fingerprints as
filters have been applied to it, so the relationship is many-to-many, and a fingerprint never
records which filter produced it: either a note exists for it or it does not.

| Ref | On | Body |
|---|---|---|
| `refs/notes/pyrocosm/commits` | a commit | the index: one fingerprint per line, oldest first, no duplicates |
| `refs/notes/pyrocosm/<kind>` (`bench`, `coverage`, …) | a filtered tree | the TEL document of a `Recordable` kind |

"The bench data for this commit" is `commit.record[Bench]`: the commit's fingerprints, newest
first, tried against the `bench` ref, the first with a note winning. "Record this run's bench
data" is `commit.record[Bench](filter, data)`: the fingerprint is computed, the note written on
the tree, and the fingerprint appended to the commit's index note if absent. `Notes` is the
capability for one repository (`fingerprint`, `fingerprints`, `bind`, `read`, `write`,
`commits`, `history`, `fetch`, `publish`); a `Recordable` names a kind and carries its TEL
codecs (`Recordable[Bench](t"bench")`).

Rules:

- The filter comes only from Pyrocosm-managed configuration, a tool's own
  `.pyrocosm/<tool>/config.tel` (a `notes` block of `exclude` and `include` globs), never from
  `.gitignore` or `.dockerignore`. Globs: `*` within a segment, `**` across segments, a bare
  name at any depth, a trailing `/` for a directory and its contents; an `include` overrides an
  `exclude`.
- A note on a tree does not need the tree to exist locally: git resolves a full hash without
  looking it up, so a clone that fetched the notes reads them without computing anything.
- The index is the only commit-keyed note and is append-only. Notes refs travel only by
  `fetch` and `publish` (`refs/notes/pyrocosm/*`); a push git rejects is reported as diverged,
  for `git notes merge` by hand until a merge strategy is chosen.
- `refs/notes/ci-attestation` (Soundness's signed in-toto envelope, keyed by its own
  `.dockerignore`-filtered tree) is a separate scheme and stays that way for as long as CI needs
  it; a fume-produced attestation will be a kind of its own under this layout, with no bridge.
- The opaque types (`Commit`, `Fingerprint`) live in objects (`Commits`, `Fingerprints`) and
  are exported from a file that imports nothing: an opaque type at package level can leak, and
  an alias resolved through a wildcard import in a file with an export of it is a cyclic
  reference (hence the root-qualified `_root_.anticipation.Text`).
- Proscenium's `List` is not Scala's: varargs reach it through `List(...)` literals or a
  fully-qualified `scala.collection.immutable.List`, a chained `::` resolves to the underlying
  list, and `has`, `reverse` and `filter` replace `contains`, `:+` and friends.


## The command line (`src/cli`)

Every Pyrocosm tool is an Ethereal daemon with an Exoskeleton command line, and each used to
hand-roll the same housekeeping differently. `pyrocosm-cli` gives them one `Tool`: a value
holding the command's name, the prose that opens its manpage, and the web front-end it can
serve, if any. The tool's own subcommands, flags and tab-completions are defined exactly as
before; `Tool.standard` wraps the dispatch and handles the standard subcommands first, falling
through to the tool's `arguments match` otherwise:

- `<name> about` prints the name and version, the executable, the daemon's pid and uptime, and
  the configuration files consulted; `<name> --version` (or `-v`) prints the version alone.
- `<name> install` installs the shell tab-completions and the manpage (`--force` overwrites an
  installed manpage). The manpage is generated from the same help tree the completions
  register, so it lists the standard subcommands too.
- `<name> quit` stops any web front-end the daemon serves, then the daemon itself.

Matching a `Subcommand` also suggests it, so the standard subcommands are tab-completed
alongside the tool's own. `--version` is read only when the first argument is a flag, so it is
offered for `<name> -<TAB>` without being attached to every subcommand.

Configuration comes from two TEL files, and `Tool` locates, parses and stat-caches both across
daemon invocations, so an edit is honoured by the next command: the repository's
`.pyrocosm/<name>/config.tel`, found by walking up from the invocation's working directory as
`.git` is found; and the user's `$XDG_CONFIG_HOME/<name>/config.tel`
(`~/.config/<name>/config.tel`). Both feed the `Configurator` cascade a `Setting` reads — flag,
then `<name>.*` property, `<NAME>_*` variable, repository file, user file — under the rules fume
established: a setting's camelCase name is a kebab-case keyword, a bare keyword reads as
`true`, and a repeated keyword's atoms join with `:`. Two keywords `Tool` reads itself, for a
tool with a web front-end: `port`, and a bare `serve`, which launches the front-end when the
daemon starts (that is, on the first real invocation, under the daemon's own monitor, so it
outlives the client that happened to start it) and keeps it until `quit`.

The version is not a constant in the code, where it drifts, but a `META-INF/pyrocosm/<name>/
version` resource the tool's build writes: the version a release or snapshot is published as,
when `<NAME>_RELEASE_VERSION` names it (the release and snapshot scripts both set it), else the
pinned version with the filtered tree hash of HEAD — the identity `make snapshot` would give
that commit — and `-dirty` if the working tree has uncommitted changes.

## Roadmap

- **M0 Scaffold** (done): `build.mill` on the flame pattern, modules `model`, `terminal`,
  `web`, `demo`, `test` (and later `notes`); Makefile; shared CI workflow.
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
  private to ultimatum, so the frontend binds cells itself; and Tab is the form's own focus
  key, so Right accepts a completion. The gallery's `static` mode prints the overview without
  a session. Not yet: inline links as focusables, mouse, and the `Resized` event.

  Found while chasing a lag that grew the longer the gallery ran: a fullscreen `Form` repaints
  only the entry that handled a key and any entry with a `period`, and it re-arms its
  animation timer after every refresh but clears its "timer pending" flag on *every* `Redraw`
  event, including an application's. So a redraw request while the timer is pending arms a
  second timer, each timer re-arms itself forever, and a `Live` cell assigned seven times a
  second (the gallery's gauge) leaks seven permanent timers a second: within twenty seconds
  the form was refreshing a thousand times a second, every refresh re-rendering every panel
  twice (once to measure, once to paint), and keystrokes queued behind it. The upstream fix
  is for the timer's wake to be distinguishable from an application's (`Terminal.Info.Tick`,
  say), or for `Form` to clear the flag only for its own wake. Pyrocosm's side, which stands
  on its own: `Refreshable` fixtures report a period only while *marked* (a cell of theirs was
  assigned) or while their content genuinely animates (`pulse`); the frontend binds each cell
  to a wake that marks its fixture and queues a `Redraw` only when no refresh is already
  coming (no timer armed, and nothing else marked); and `PanelFixture` caches its rendering
  by content identity, width, focus, selection and animation frame, so a refresh costs a
  render only for what changed. With this the gallery refreshes ten times a second at a few
  per cent of a core, however long it runs.
- **M3 Web renderer** (done, first cut): `HtmlRenderer` renders every node to semantic HTML
  carrying `pyro-*` classes and handle ids; `WebArrangement.plan` maps roles to page features
  and `PyrocosmPage` is the graffiti page (masthead with title, status panels and a connection
  pill; global controls as the top menu; navigation verso; detail recto; primary, log and
  prompt as cards); `WebStyles` is the one stylesheet and `WebTheme` the palette; `WebFrontend`
  serves the page and script, binds every `Live` cell to a patch broadcast over one WebSocket
  per tab, and validates every incoming id against the interface's handles; `pyrocosm.js` is
  the generic script. Patches so far: `replace`, `value`, `enable`/`disable`, `check`/`uncheck`,
  `select`, `class`/`unclass`. `gallery serve [port]` runs the gallery on it, from the same
  `Session` as the terminal. The markup is meant to be restyled by serving another sheet, so
  it keeps to these rules: every class names what an element *is* (`pyro-gauge`,
  `pyro-series-value`) or a state it is in (`pyro-empty`, `pyro-selected`, `pyro-online`,
  `pyro-incomplete`), never how it looks; nothing carries a `style` attribute; and the
  stylesheet is linked, not embedded. Graffiti's `html` inlines a `<style>`, so `PyrocosmPage`
  composes its own `markup` from the same `frame` and `head` seams with a `<link>` to
  `/pyrocosm.css`, which `WebFrontend` serves from `css`. A bar chart is a `figure` holding a
  `dl` (a series is a `dt`, each value a `dd` with a `meter`), a gauge a `figure` with its
  `figcaption`, a duration a `time` with a `PT…S` datetime, a field's history a `data-history`
  attribute, and an incomplete field a `pyro-incomplete` class on the field itself (its
  decoration is replaced by innerHTML, so the state travels by its own `class`/`unclass`
  patch). A field's tokens still ride in a hidden `span`: honeycomb registers `template` as a
  void tag, and the serializer decides voidness from the registry by name, so a container
  defined here serializes empty; once honeycomb's `Template` is a container the span can
  become one. `Block.Heading(1)` inside a panel still emits `<h1>`, as the content asks.
  Found while building it:
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
  Not yet: graphs as SVG (a graph is a dependency list), sortable tables, `Focus` and
  `Notify` patches, the `Resized` event, and clean shutdown on Ctrl+C under the Ethereal
  launcher. (Per-page sessions and the editor came with M6's groundwork.)
- **M4 Gallery**: every node, every role and priority, a ticking gauge, a selectable table
  driving a detail panel, a code field with fake decorations; `demo terminal` and
  `demo serve` (both exist, driven by one `Session`); rendered at 40/80/160 columns and
  phone/laptop/wide viewports. Permanent visual regression fixture. Still to do: the width and
  viewport renderings, and tarantula screenshots.
- **M5 Fume** (started): fume's `Blocks` converts its `Doc` (statuses, data, tables,
  sparklines, histograms, pending lists, groups) and its live table of checks to blocks, and
  `Board` is an `Interface` of a following primary panel and a progress gauge, rebuilt from
  `Model.state()` at most ten times a second and shown by `TerminalFrontend` in place of the
  hand-rolled `Live` board (its own `ScreenRoot`, byte-level key parser, resize probe and
  ticker are gone). Leaving the board aborts the run; the run finishing stops the board through
  `Frontend.stop`, added for this. Found while doing it: profanity's `interactive` closes the
  console's stdin at the end of a session, which under an Ethereal daemon is the client's
  socket, so anything printed afterwards (fume's report) was lost; the terminal frontend now
  gives the session a detachable stdin whose close only unblocks the key pump. `fume serve`
  serves a `Dashboard` interface (the journal's runs as navigation, the chosen run's blocks
  as the primary panel, its progress as status) on `WebFrontend`; a run started while it
  serves registers its `Board`, so the page tails the very cells the terminal shows, which
  is principle 5 made real. Not yet: the static report still goes through fume's `Render`
  (terse mode and GitHub annotations depend on it); palette and figures moving here; detail
  and log panels; per-tab selection on the dashboard.
- **M6 Flame** (done): Flame's user interface is one `ReplInterface` in flame.core, an
  `Interface` (a transcript, a code prompt, and on the web a navigation panel of sessions) and
  an event handler, driven by an `Engine`: the socket to the server process in the terminal
  (`SocketEngine`), the in-process `Sessions.Connection` on the web (`LocalEngine`). Every
  submission appends a pending entry ending in a `Gauge(Indeterminate)`, which streamed output
  grows with `Block.Output` chunks and the reply replaces in place, so an async fill and a
  synchronous answer are the same path; the entry is made as the request is, once its id is
  known, since an in-process engine answers before `request` returns. The server, under
  `Repl.Rendering.Exhibit`, sends results as blocks: the value through Pyrocosm's
  `Presentable` cascade inside the compiled wrapper (`ExhibitRender`), diagnostics as notices
  with highlighted types and code, captured output by stream, stack traces through the model's
  own `StackTrace` exhibit; carried on the wire as the TEL text of a product wrapping the
  blocks, since BinTEL cannot yet derive a codec for `List[Block]` and a TEL document is a
  record, not a sum. The hand-written terminal client (its live-highlighting replay, completion
  and scope tables, transcript replay) and the web's bespoke JSON protocol, script, page and
  `HtmlRender` are gone; `--basic` and the REST API are kept, on `Inspect`. Exercised by
  `gallery repl`, `gallery serve repl`, Flame's suite (a fake engine drives the interface; an
  exhibiting session's replies are checked as blocks) and pseudo-terminal and raw WebSocket
  sessions against a running server.
  - Added here for it: `Decoration.marks` (error spans underlined in the field),
    `Block.Output` (captured output behind a stream gutter, wrapped hard), an
    application-owned `Field.history`, transcript clearing (a transcript shrinking below what
    is committed clears the screen and scrollback at the next cycle), `WebFrontend`'s
    `fallback` for an application's own routes, `TerminalFrontend` taking the inline anchoring,
    growth and shrink policies, the products-only structural fallback of `Presentable`, a Tab
    with no candidates reaching the application as `Event.Key(Tab)`, a keep-alive ping and a
    reload after refused handshakes in the script, atomic `Live.amend`, and
    `Token.Accent.Command`.
  - Found: a wake from any cell but the transcript's reported no entries, which read as a
    cleared transcript once anything was committed, so every keystroke after the first commit
    reset the screen (the blank rows it left looked like a separator arriving one commit late);
    a bordered panel hid its content but not its border during a commit, leaving an empty box
    in the scrollback (the border is now built of edges that hide with the content, and its
    bands take no share of the stack's height); a `Group`'s members are rendered tightly, with
    no blank line between a submission and its result; a completion row longer than the field
    wrapped and pushed the value row out (rows are cut to the width); stratiform wrote an empty
    atom's preceding space, which its own parser refused as a trailing space, so a document
    holding an empty token, cell or phrase could never be read back, and a compiler message
    with a blank line inside it made Flame drop the whole reply's blocks. Fixed in the
    Soundness tree (pending a release), and sidestepped here: the model's `Text` encoder
    writes an empty text as a compound with no atom, which reads back as the empty string.
  - A resize reflows the committed scrollback unpredictably, so an inline session replays:
    once the size has settled (a drag fires many events), the cycle ends, the screen and
    scrollback are cleared, and the settled entries are committed again at the new width in
    one frame painted once, then the rest and the prompt resume. The form never sees a
    resize: each event clears the screen and resets the block at once, so a drag shows a blank
    screen rather than frames painted against a geometry that no longer holds (which read as a
    slow response, since the picture did not change until the replay). Code lines and a field's
    value wrap hard at the width, with the caret following, so nothing is clipped at a narrow
    width. An async run's streamed chunks show while it is pending; its reply carries the
    whole output itself, so the chunks are not kept beside it.
  - Knowingly left: the code-versus-prose border colour (the verdict shows as a detail note);
    a Flame theme (the default palettes are used); `/tasty` and `/bytecode` as tables (they
    arrive as output text).
- **M6b Git notes** (done): the `notes` module above, with a suite driving a scratch
  repository through fingerprints, index binding, the reverse index, history, and a publish and
  fetch through a bare remote. Fume's roadmap items 3.1 and 3.2 build on it.
- **M6c Stack traces** (done): `Block.Trace`, an exception chain as the model's own frames,
  replacing the notice-and-table exhibit. The terminal lays it out as digression's own
  rendering does (accents by package, bold last segment, repeated names and plumbing subdued,
  `↳` rows for inlining, `caused by:`), at the panel's width, with the file, colon and line as
  one word aligned on the colon; the web is a section and a frame table with `pyro-trace-*`
  classes and `--pyro-trace-*` theme variables, its source column folding on a narrow viewport.
  `Throwable` exhibits through it.
- **M7 Fury and Fluence**: Fury's `FrontEnd` as an interface with the target DAG as a graph;
  Fluence pages as blocks with the API tree as navigation.
- **M8 Hardening**: Markdown renderer, themes, ARIA from roles, optional Scala.js client,
  finer `Live` granularity if profiling requires, hint vocabulary revised from real use.

## Releasing

Pyrocosm releases to GitHub Releases, as the rest of the ecosystem does since Soundness #1929:
a release is cut by tagging. Bump `pyrocosmVersion` in `build.mill`, merge it, wait for CI to
go green on that commit, and then `git tag -s X.Y.Z && git push --tags`. The tag fires
`.github/workflows/release.yml`, which runs the shared `release.sh` in propensive/.github: it
gates (a signed tag, a green CI run on that very commit, every pin a released version), builds
from a cold tree, stages the library jars with their POM and ivy.xml embedded under
`META-INF/maven/`, uploads them into a draft, checks every asset's digest against the local
file, and only then publishes. A consumer installs a release into its local ivy repository with Soundness's
sync script pointed here (`SOUNDNESS_RELEASE_REPO=propensive/pyrocosm python3 sync_releases.py
X.Y.Z`; the shared CI workflow does this from an `extra_releases` input), and names this
repository among Burdock's hints when repackaging a launcher (`--github propensive/pyrocosm`),
so the jars on its classpath are matched to the release's assets by digest and externalized.

`make sync-releases` wraps that script here and in each consumer, defaulting to the version
`build.mill` pins: in a consumer it installs the released jars a local build then resolves,
instead of whatever a Pyrocosm checkout's `publishLocal` last left in `~/.ivy2/local`; here it
installs the published bytes, which is how a release is verified once it is out. `make
sync-staged` installs the jars of a local `make stage` under the same version, so a release
candidate can be tried in Fume or Flame before anything is tagged. Both overwrite what
`publishLocal` installed, and `publishLocal` puts it back.

## Decisions

- Arrangement is renderer-owned; the model carries roles, priorities, relations and hints.
- The browser side is a thin generic script, not Scala.js, for now.
- Reactive cells plus an event stream, not Elm-style pure views.
- Fume before Flame: Fume's `Doc` is already the block model; Flame's editor is the hardest
  control.
- Pyrocosm is its own repository depending on published Soundness components; the typeclass and
  every instance live here.
- Every source file imports the ecosystem through the umbrella, `import soundness.*`, as Fume and
  Flame do, rather than naming each library. A name the umbrella also carries is excluded from it
  (`import soundness.{Token as _, *}`), since a wildcard import outranks a definition made in
  another file of the same package; what the umbrella leaves out (`murmuration.zip`,
  `perihelion.Channel`, `clavichord.Keypress`, whose re-export widens its cases) is imported by
  name, with the reason stated where it is not obvious.
- `Presentable` / `exhibit`; `portray` is the fallback verb.
