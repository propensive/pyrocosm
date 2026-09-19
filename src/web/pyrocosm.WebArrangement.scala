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

// Excluded from the umbrella: `Control` (coaxial), which would outrank this package's own
// definitions, since a wildcard import beats a package member declared in another file. Excluded
// too: `Span` (denominative), so the name is the HTML element `htmlDoms` supplies.
import soundness.{Control as _, Span as _, *}

import dysasymptotics.{linearAccess, linearSize}
import attributives.textAttributive
import formatting.compactJsonFormatting
import htmlDoms.whatwg.*
import nomenclature.CssClass.nominative

// The web's arrangement: the same roles as the terminal's, mapped onto graffiti's page features
// rather than onto pane bands. Nothing is dropped: a peripheral panel is collapsed by the
// stylesheet on a narrow viewport instead.
object WebArrangement:
  case class Plan
    ( navigation: List[Panel],
      primary:    List[Panel],
      detail:     List[Panel],
      log:        List[Panel],
      prompt:     List[Panel],
      status:     List[Panel] )

  def plan(interface: Interface): Plan =
    def role(role: Panel.Role): List[Panel] = interface.panels.filter(_.role == role)

    Plan
      ( navigation = role(Panel.Role.Navigation),
        primary = role(Panel.Role.Primary) + role(Panel.Role.Transcript),
        detail = role(Panel.Role.Detail) + role(Panel.Role.Inspector),
        log = role(Panel.Role.Log),
        prompt = role(Panel.Role.Prompt),
        status = role(Panel.Role.Status) )

// The page: a masthead holding the title, the status panels and the connection indicator, with
// the global controls beneath it as a toolbar; navigation on the verso side; detail on the
// recto side; and the primary, log and prompt panels as the main matter. A side whose panels
// are absent is not drawn, so a page without detail has no right-hand column. Each panel is a
// card whose content element the frontend replaces by id when its `Live` cell changes.
//
// The features are mixed in so that the chrome encloses the matter: `Mainstay` wraps the
// columns in `<main>`, `Masthead` puts its `<header>` before it, and `TopMenu`, last, puts the
// menu bar — the wordmark and a link to the configuration page — before that.
object PyrocosmPage:
  // `<meta name="pyro-session" content="…">`, naming the page's session for the script.
  val SessionMeta = Tag.void["meta", Whatwg](presets = proscenium.Map(t"name" -> t"pyro-session"))

  // `data-history="…"` on a field: the application's history, seeded for the script.
  given history: ("data-history" is Attribute of Whatwg.Textual in Whatwg) = Whatwg.globalAttribute()

  // A `crossorigin` value on a `link`, as honeycomb declares the attribute but no way to
  // attribute a value to it.
  given crossorigin: Crossorigin is Attributive to Whatwg.Crossorigin = (key, value) => (key, value.show)

class PyrocosmPage(interface: Interface, renderer: HtmlRenderer, theme: WebTheme, session: Optional[Text] = Unset)
extends Archetype, Viewport, VersoPanel, RectoPanel, Mainstay, Masthead, TopMenu:
  import HtmlRenderer.{cls, panelId, classes}
  import PyrocosmPage.{history, crossorigin}

  private val plan: WebArrangement.Plan = WebArrangement.plan(interface)

  override def pageTitle: Text = Inline.plain(interface.title)
  override def versoWidth: Quantity[Rems[1]] = 15.0*Rem
  override def rectoWidth: Quantity[Rems[1]] = 20.0*Rem

  // A side column exists only when a panel is arranged there: the matter alone, otherwise.
  protected override def versoArrangement
    ( panel: Html of (? <: Flow), content: Html of (? <: Flow) )
  :   Html of (? <: Flow) =

    if plan.navigation.nil then content else super.versoArrangement(panel, content)

  protected override def rectoArrangement
    ( content: Html of (? <: Flow), panel: Html of (? <: Flow) )
  :   Html of (? <: Flow) =

    if plan.detail.nil then content else super.rectoArrangement(content, panel)

  // The controls other than a field are phrasing, so they fit the top menu; a field is flow.
  def controlFlow(control: Control): Html of Flow = control match
    case field: Control.Field => fieldHtml(field)
    case other                => this.control(other)

  def control(control: Control): Html of Phrasing = control match
    case Control.Button(label, action, enabled) =>
      if enabled() then Button(id = action.id, `class` = cls(t"pyro-button"))(renderer.phrase(label))
      else Button(id = action.id, `class` = cls(t"pyro-button"), disabled = true)(renderer.phrase(label))

    case Control.Toggle(toggle, label, state) =>
      val box =
        if state() then Input.Checkbox(id = toggle.id, `class` = cls(t"pyro-toggle"), checked = true)
        else Input.Checkbox(id = toggle.id, `class` = cls(t"pyro-toggle"))
      Label(box, t" ", renderer.phrase(label))

    case Control.Choice(choice, options, current) =>
      Select(id = choice.id, `class` = cls(t"pyro-choice"))(options.indexed.map { (option, index) =>
          if index.n0 == current() then Option(value = index.n0.toString.tt, selected = true)(Inline.plain(option))
          else Option(value = index.n0.toString.tt)(Inline.plain(option))
        }*)

    case _: Control.Field => Fragment[Phrasing]()

  // A line or multiline field is a textarea. A code field is an editable `code` element, which
  // the script paints with the decoration's tokens: a textarea cannot carry styled spans. A
  // field whose text is an incomplete prefix says so in a state class, which a patch keeps
  // current; its history is seeded in a data attribute.
  def fieldHtml(field: Control.Field): Html of Flow =
    val group = cls(t"pyro-field-group")
    val decorated = Div(id = t"${field.input.id}-decoration", `class` = cls(t"pyro-decoration"))(decoration(field))
    val state: List[Name[CssClass]] = if field.decoration().incomplete then List(cls(t"pyro-incomplete")) else Nil

    field.kind match
      case Control.Field.Kind.Code(_) =>
        Div(`class` = List(group, cls(t"pyro-empty")))
          ( Code
              ( id = field.input.id,
                `class` = cls(t"pyro-field") :: cls(t"pyro-editor") :: state,
                contenteditable = t"true",
                spellcheck = t"false",
                `data-history` = field.history().in[Json].show )
              (field.value()),
            Span(`class` = cls(t"pyro-placeholder"))(field.placeholder.or(t"")),
            decorated )

      case kind =>
        val rows = if kind == Control.Field.Kind.Line then 1 else 3
        Div(`class` = group)
          ( Textarea(id = field.input.id, `class` = cls(t"pyro-field") :: state, rows = rows, placeholder = field.placeholder.or(t""))(field.value()),
            decorated )

  // What the script reads to decorate a field: the highlighting tokens (hidden; the script
  // paints an editor with them when they cover its text), the note, and the
  // completions, which the script also turns into suggestion text.
  def decoration(field: Control.Field): Html of Flow =
    val decoration0 = field.decoration()

    // The tokens as code lines, so the decoration's marks lay over them as a code block's notes.
    val tokens: Html of Flow =
      if decoration0.tokens.nil then Fragment[Flow]()
      else
        val lines: List[Block.Line] = Block.Line.split(decoration0.tokens)
        val count: Int = lines.size
        val rendered: List[Html of Phrasing] =
          lines.indexed.map { (line: Block.Line, index: Ordinal) =>
            val notes: List[Block.Note] = decoration0.marks.filter(_.line == index.n0)
            renderer.codeLine(line, notes, index.n0 == count - 1) }

        Span(`class` = cls(t"pyro-tokens"), hidden = t"")(rendered*)

    val detail: Html of Flow =
      if decoration0.detail.nil then Fragment[Flow]()
      else Div(`class` = cls(t"pyro-note"))(renderer.blocks(decoration0.detail))

    // A candidate that replaces the whole text says so, for the script.
    val list: Html of Flow =
      if decoration0.completions.nil then Fragment[Flow]()
      else Ul(`class` = cls(t"pyro-completions"))(decoration0.completions.map { (completion: Control.Field.Completion) =>
        val classes: List[Name[CssClass]] = if completion.whole then List(cls(t"pyro-replacement")) else Nil
        Li(`class` = classes)(Code(completion.name), Span(`class` = cls(t"pyro-signature"))(completion.signature)) }*)

    Fragment(tokens, detail, list)

  def panelContent(panel: Panel): Html of Flow =
    Div(id = t"${panelId(panel)}-content", `class` = cls(t"pyro-panel-content"))(renderer.blocks(panel.content()))

  def card(panel: Panel): Html of Flow =
    val classes = List
      ( cls(t"pyro-panel"),
        cls(t"pyro-role-${panel.role.toString.tt.lower}"),
        cls(t"pyro-priority-${panel.priority.toString.tt.lower}") )

    val heading: Html of Flow = panel.title.lay(Fragment[Flow]()) { title => H2(renderer.phrase(title)) }
    val controls: Html of Flow =
      if panel.controls.nil then Fragment[Flow]()
      else Footer(`class` = cls(t"pyro-controls"))(panel.controls.map(controlFlow)*)

    Section(id = panelId(panel), `class` = classes)(heading, panelContent(panel), controls)

  private def cards(panels: List[Panel]): Html of Flow = Fragment(panels.map(card)*)

  // The menu bar: a full-width band whose contents are centred to the wide measure, holding
  // the wordmark, which is the way home, and the link to the configuration page.
  protected override def menu: Html of "nav" =
    Nav(`class` = List(TopMenu.menuClass, cls(t"pyro-menubar")))
      ( Div(`class` = cls(t"pyro-menubar-inner"))
          ( A(href = t"/", `class` = cls(t"pyro-wordmark"))(t"Pyrocosm"),
            A(href = t"/config", `class` = cls(t"pyro-menubar-link"))(t"Configuration") ) )

  // The global controls other than a field, each an item of the masthead's toolbar.
  private def toolbarItems: List[Html of "li"] =
    interface.controls.filter { (control0: Control) => !control0.isInstanceOf[Control.Field] }
    . map { (control0: Control) => Li(`class` = cls(t"pyro-control"))(control(control0)) }

  // The brand row: the title, the status panels' content and the connection indicator; then,
  // when the interface has any, the global controls as a `menu` of commands.
  override def masthead: Html of Flow =
    val brand: Html of Flow =
      Div(`class` = cls(t"pyro-masthead"))
        ( Div(`class` = cls(t"pyro-brand"))
            ( H1(`class` = cls(t"pyro-title"))(renderer.phrase(interface.title)),
              Fragment(plan.status.map { (panel: Panel) => Div(id = panelId(panel), `class` = cls(t"pyro-status"))(panelContent(panel)) }*) ),
          Output(id = t"pyro-connection", `class` = List(cls(t"pyro-connection"), cls(t"pyro-offline")))(t"connecting") )

    val items = toolbarItems
    if items.nil then brand else Fragment(brand, Menu(`class` = cls(t"pyro-toolbar"))(items*))

  override def verso: Html of Flow = cards(plan.navigation)
  override def recto: Html of Flow = cards(plan.detail)
  def content: Html of Flow = Fragment(cards(plan.primary), cards(plan.log), cards(plan.prompt))

  // The session this page belongs to, if it has one of its own, for the script to name when it
  // opens its socket.
  protected override def head: Html of Metadata =
    val named: Html of Metadata = session match
      case id: Text => PyrocosmPage.SessionMeta(content = id)
      case _        => Fragment[Metadata]()

    // The fonts' origins, so the browser opens their connections while it reads the sheet.
    val fonts: Html of Metadata =
      Fragment[Metadata]
        ( Link.Preconnect(href = t"https://fonts.googleapis.com"),
          Link.Preconnect(href = t"https://fonts.gstatic.com", crossorigin = Crossorigin.Anonymous) )

    Fragment[Metadata](fonts, Script(src = t"/pyrocosm.js", defer = true), named, super.head)

  // The fonts' import first, as CSS requires; then graffiti's rules; then this page's own.
  protected override def styles: Css = WebStyles.fonts + super.styles + WebStyles.rules(theme)

  // The page as served: graffiti's `html` inlines the stylesheet in a `<style>` element, but
  // this page links it, so a Pyrocosm application is restyled by serving another sheet. The
  // sheet itself is `css`, which the frontend serves at `/pyrocosm.css`.
  // The body says which side columns the page has, so the sheet can widen its measure for them.
  def markup: Html of "html" =
    val verso: List[Name[CssClass]] = if plan.navigation.nil then Nil else List(cls(t"pyro-has-verso"))
    val recto: List[Name[CssClass]] = if plan.detail.nil then Nil else List(cls(t"pyro-has-recto"))
    val sides: List[Name[CssClass]] = verso + recto

    Html(Head(Title(pageTitle), Link.Stylesheet(href = t"/pyrocosm.css"), head), Body(dir = direction.show, `class` = sides)(frame))
