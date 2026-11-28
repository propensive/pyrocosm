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

import soundness.{Channel as _, Control as _, Filter as _, Glyph as _, Language as _, Origin as _,
  Status as _, *}

import dysasymptotics.{linearAccess, linearSize}
import formatting.indentedCssFormatting
import palettes.solarizedDarkGaugePalette
import tableStyles.thickTableStyle
import textMetrics.uniformMetric

// Scrolling a panel's content in the terminal with Up and Down: bounded at both ends, so that a
// key pressed at an end is not owed back before the content moves the other way.
// And, on the web, a side column which scrolls within itself, apart from the matter beside it.
object ScrollTests extends Suite(m"Pyrocosm scroll tests"):
  def run(): Unit =
    val renderer = TerminalRenderer()

    // Paragraphs of one line each, a blank row between each: nineteen rows, on a board of four.
    def paragraphs(count: Int): List[Block] =
      List.range(0, count).map { (index: Int) => Block.paragraph(t"p$index") }

    def scroller(content: Live[List[Block]], hints: Hints = Hints.none): PanelFixture =
      val panel = Panel(Panel.Id(t"scroller"), Panel.Role.Primary, Unset, content, hints = hints)
      PanelFixture(panel, renderer, (_: Event) => (), _root_.pyrocosm.Session())

    def window(fixture: PanelFixture): List[Text] =
      val grid = MemoryBoard(20, 4)
      fixture.render(grid, true)
      List.range(0, 4).map { (row: Int) => grid.line(row).trim }

    test(m"a panel starts at the top of its content"):
      window(scroller(Live(paragraphs(10))))

    . assert(_ == List(t"p0", t"", t"p1", t""))

    test(m"Down pressed past the bottom is not owed back before Up scrolls"):
      val fixture = scroller(Live(paragraphs(10)))
      window(fixture)
      (0 until 40).foreach { _ => fixture.handle(Keypress.Down) }
      val bottom = window(fixture)
      fixture.handle(Keypress.Up)
      (bottom, window(fixture))

    . assert(_ == (List(t"", t"p8", t"", t"p9"), List(t"p7", t"", t"p8", t"")))

    test(m"Up pressed past the top is not owed back before Down scrolls"):
      val fixture = scroller(Live(paragraphs(10)))
      window(fixture)
      (0 until 40).foreach { _ => fixture.handle(Keypress.Up) }
      fixture.handle(Keypress.Down)
      window(fixture)

    . assert(_ == List(t"", t"p1", t"", t"p2"))

    test(m"content which fits its panel does not scroll"):
      val fixture = scroller(Live(paragraphs(2)))
      window(fixture)
      (0 until 5).foreach { _ => fixture.handle(Keypress.Down) }
      window(fixture)

    . assert(_ == List(t"p0", t"", t"p1", t""))

    test(m"a position beyond content which has shrunk is not owed back either"):
      val content: Live[List[Block]] = Live(paragraphs(10))
      val fixture = scroller(content)
      window(fixture)
      (0 until 40).foreach { _ => fixture.handle(Keypress.Down) }
      window(fixture)
      content() = paragraphs(5)
      window(fixture)
      fixture.handle(Keypress.Up)
      window(fixture)

    . assert(_ == List(t"p2", t"", t"p3", t""))

    test(m"a following panel starts at the bottom, and Up moves it one row from there"):
      val fixture = scroller(Live(paragraphs(10)), Hints(hints.Follow))
      val bottom = window(fixture)
      fixture.handle(Keypress.Up)
      (bottom, window(fixture))

    . assert(_ == (List(t"", t"p8", t"", t"p9"), List(t"p7", t"", t"p8", t"")))

    test(m"a following panel scrolled up stays where it is as content arrives"):
      val content: Live[List[Block]] = Live(paragraphs(10))
      val fixture = scroller(content, Hints(hints.Follow))
      window(fixture)
      fixture.handle(Keypress.Up)
      window(fixture)
      content() = paragraphs(12)
      window(fixture)

    . assert(_ == List(t"p7", t"", t"p8", t""))

    test(m"a following panel brought back to the bottom follows again"):
      val content: Live[List[Block]] = Live(paragraphs(10))
      val fixture = scroller(content, Hints(hints.Follow))
      window(fixture)
      fixture.handle(Keypress.Up)
      window(fixture)
      (0 until 5).foreach { _ => fixture.handle(Keypress.Down) }
      window(fixture)
      content() = paragraphs(12)
      window(fixture)

    . assert(_ == List(t"", t"p10", t"", t"p11"))

    // The rule for the side columns, as it is beside the matter and as it is on a narrow page,
    // where the columns are stacked and the page alone scrolls.
    val stylesheet: Text = WebStyles.css(WebTheme.default).show
    val rules: List[Text] = stylesheet.cut(t".graffiti-verso, .graffiti-recto {")
    def body(rule: Optional[Text]): Text = rule.let(_.cut(t"}").at(Ordinal.zerary(0))).or(t"")
    val beside: Text = body(rules.at(Ordinal.zerary(1)))
    val stacked: Text = body(rules.reverse.at(Ordinal.zerary(0)))

    test(m"a side column taller than the window scrolls within itself, beside the matter"):
      ( beside.contains(t"position: sticky"),
        beside.contains(t"overflow-y: auto"),
        beside.contains(t"max-height: calc(100vh - 2*var(--pyro-space-5))"),
        beside.contains(t"overscroll-behavior: contain") )

    . assert(_ == (true, true, true, true))

    test(m"a side column stacked above the matter on a narrow page scrolls with the page"):
      (stacked.contains(t"position: static"), stacked.contains(t"max-height: none"))

    . assert(_ == (true, true))

    // A page of an application with a sheet of its own links it after Pyrocosm's.
    test(m"an application's own stylesheet is linked after Pyrocosm's, and only where it has one"):
      val interface: Interface = Interface(Inline.text(t"Talk"), Nil)
      def page(styled: Boolean): Text =
        PyrocosmPage(interface, HtmlRenderer(), WebTheme.default, prefix = t"/talk", styled = styled).markup.show

      val own: Text = t"""href="/talk/styles.css""""
      val first: Text = page(true).cut(own).at(Ordinal.zerary(0)).or(t"")
      (page(true).contains(own), first.contains(t"""href="/talk/pyrocosm.css""""), page(false).contains(own))

    . assert(_ == (true, true, false))

    test(m"an application's own script is run after Pyrocosm's, and only where it has one"):
      val interface: Interface = Interface(Inline.text(t"Talk"), Nil)
      def page(scripted: Boolean): Text =
        PyrocosmPage(interface, HtmlRenderer(), WebTheme.default, prefix = t"/talk", scripted = scripted).markup.show

      val own: Text = t"""src="/talk/scripts.js""""
      val first: Text = page(true).cut(own).at(Ordinal.zerary(0)).or(t"")
      (page(true).contains(own), first.contains(t"""src="/talk/pyrocosm.js""""), page(false).contains(own))

    . assert(_ == (true, true, false))
