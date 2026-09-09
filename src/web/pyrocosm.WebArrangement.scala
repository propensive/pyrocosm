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
import denominative.{Span as _, *}
import prepositional.*
import gossamer.*
import graffiti.*
import honeycomb.*
import nomenclature.*
import quantitative.*
import rudiments.*
import symbolism.*
import vacuous.*

import honeycomb.attributives.textAttributive
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

// The page: a masthead holding the title, the status panels and the connection indicator; the
// global controls as the top menu; navigation on the verso side; detail on the recto side; and
// the primary, log and prompt panels as the main matter. Each panel is a card whose content
// element the frontend replaces by id when its `Live` cell changes.
object PyrocosmPage:
  // `<meta name="pyro-session" content="…">`, naming the page's session for the script.
  val SessionMeta = Tag.void["meta", Whatwg](presets = proscenium.Map(t"name" -> t"pyro-session"))

class PyrocosmPage(interface: Interface, renderer: HtmlRenderer, theme: WebTheme, session: Optional[Text] = Unset)
extends Archetype, Masthead, TopMenu, VersoPanel, RectoPanel, Mainstay:
  import HtmlRenderer.{cls, panelId}

  private val plan: WebArrangement.Plan = WebArrangement.plan(interface)

  override def pageTitle: Text = Inline.plain(interface.title)
  override def versoWidth: Quantity[Rems[1]] = 14.0*Rem
  override def rectoWidth: Quantity[Rems[1]] = 20.0*Rem

  // The controls other than a field are phrasing, so they fit the top menu; a field is flow.
  def controlFlow(control: Control): Html of Flow = control match
    case field: Control.Field => fieldHtml(field)
    case other                => this.control(other)

  def control(control: Control): Html of Phrasing = control match
    case Control.Button(label, action, enabled) =>
      if enabled() then Button(id = action.id, `class` = List(cls(t"pyro-button"), cls(t"pyro-press")))(renderer.phrase(label))
      else Button(id = action.id, `class` = List(cls(t"pyro-button"), cls(t"pyro-press")), disabled = true)(renderer.phrase(label))

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
  // the script paints with the decoration's tokens: a textarea cannot carry styled spans.
  def fieldHtml(field: Control.Field): Html of Flow =
    val holder = cls(t"pyro-field-holder")
    val decorated = Div(id = t"${field.input.id}-decoration", `class` = cls(t"pyro-decoration"))(decoration(field))

    field.kind match
      case Control.Field.Kind.Code(_) =>
        Div(`class` = List(holder, cls(t"pyro-empty")))
          ( Code(id = field.input.id, `class` = List(cls(t"pyro-field"), cls(t"pyro-editor")), contenteditable = t"true", spellcheck = t"false")(field.value()),
            Span(`class` = cls(t"pyro-placeholder"))(field.placeholder.or(t"")),
            decorated )

      case kind =>
        val rows = if kind == Control.Field.Kind.Line then 1 else 3
        Div(`class` = holder)
          ( Textarea(id = field.input.id, `class` = cls(t"pyro-field"), rows = rows, placeholder = field.placeholder.or(t""))(field.value()),
            decorated )

  // What the script reads to decorate a field: the highlighting tokens (hidden; the script
  // paints an editor with them when they cover its text), the note, a hidden marker when the
  // text is an incomplete prefix (so Enter inserts a newline rather than submitting), and the
  // completions, which the script also turns into ghost text.
  def decoration(field: Control.Field): Html of Flow =
    val decoration0 = field.decoration()

    val tokens: Html of Flow =
      if decoration0.tokens.nil then Fragment[Flow]()
      else Span(`class` = cls(t"pyro-tokens"), hidden = t"")(renderer.tokens(decoration0.tokens))

    val marker: Html of Flow =
      if decoration0.incomplete then Span(`class` = cls(t"pyro-incomplete"), hidden = t"")(t"")
      else Fragment[Flow]()

    val detail: Html of Flow =
      if decoration0.detail.nil then Fragment[Flow]()
      else Div(`class` = cls(t"pyro-note"))(renderer.blocks(decoration0.detail))

    // A candidate that replaces the whole text says so, for the script.
    val list: Html of Flow =
      if decoration0.completions.nil then Fragment[Flow]()
      else Ul(`class` = cls(t"pyro-completions"))(decoration0.completions.map { (completion: Control.Field.Completion) =>
        val classes: List[Name[CssClass]] = if completion.whole then List(cls(t"pyro-whole")) else Nil
        Li(`class` = classes)(Code(completion.name), Span(`class` = cls(t"pyro-signature"))(completion.signature)) }*)

    Fragment(tokens, marker, detail, list)

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
      else Div(`class` = cls(t"pyro-controls"))(panel.controls.map(controlFlow)*)

    Section(id = panelId(panel), `class` = classes)(heading, panelContent(panel), controls)

  private def cards(panels: List[Panel]): Html of Flow = Fragment(panels.map(card)*)

  override def masthead: Html of Flow =
    Div(`class` = cls(t"pyro-status-bar"))
      ( Span(`class` = cls(t"pyro-title"))(renderer.phrase(interface.title)),
        Fragment(plan.status.map { (panel: Panel) => Div(id = panelId(panel))(panelContent(panel)) }*),
        Span(id = t"pyro-connection", `class` = List(cls(t"pyro-connection"), cls(t"pyro-offline")))(t"connecting") )

  override def menuItems: List[Html of Phrasing] =
    interface.controls.filter { (control0: Control) => !control0.isInstanceOf[Control.Field] }.map { (control0: Control) => Span(`class` = cls(t"pyro-menu-item"))(control(control0)) }

  override def verso: Html of Flow = cards(plan.navigation)
  override def recto: Html of Flow = cards(plan.detail)
  def content: Html of Flow = Fragment(cards(plan.primary), cards(plan.log), cards(plan.prompt))

  // The session this page belongs to, if it has one of its own, for the script to name when it
  // opens its socket.
  protected override def head: Html of Metadata =
    val named: Html of Metadata = session match
      case id: Text => PyrocosmPage.SessionMeta(content = id)
      case _        => Fragment[Metadata]()
    Fragment[Metadata](Script(src = t"/pyrocosm.js", defer = true), named, super.head)

  protected override def styles: Css = super.styles + WebStyles.css(theme)
