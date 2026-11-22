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
import palettes.solarizedDarkGaugePalette
import tableStyles.thickTableStyle
import termcapDefinitions.xtermTrueColorTermcap
import textMetrics.uniformMetric

// Code's colours: the same in a terminal as on the web, since both take them from one palette,
// and a block of code painted on that palette's ground.
object CodeThemeTests extends Suite(m"Pyrocosm code theme tests"):
  def run(): Unit =
    val renderer: TerminalRenderer = TerminalRenderer()
    val bare: TerminalRenderer = TerminalRenderer(TerminalTheme.Solarized)

    // Teletypes compare by identity; their renderings compare by content.
    def shown(lines: List[Teletype]): List[Text] = lines.map(_.render(xtermTrueColorTermcap))

    val sample: Block.Code =
      Block.Code
        ( Language.Scala,
          List
            ( Block.Line
                ( List
                    ( Token(t"val", Token.Accent.Keyword),
                      Token.plain(t" x = 1 "),
                      Token(t"// one", Token.Accent.Comment) ) ) ) )

    test(m"a keyword in the terminal has the colour it has on the web"):
      val keyword: Text = shown(List(renderer.token(Token(t"val", Token.Accent.Keyword)))).join
      val web: Text = WebTheme.hex(WebTheme.Ember.codeAccent(Token.Accent.Keyword))
      (keyword.contains(t"38;2;255;102;51"), web)

    . assert(_ == (true, t"#ff6633"))

    test(m"every accent is one colour in the terminal and on the web"):
      Token.Accent.values.foldLeft(true): (same: Boolean, accent: Token.Accent) =>
        same && TerminalTheme.default.codeAccent(accent) == WebTheme.default.codeAccent(accent)

    . assert(_ == true)

    test(m"a block of code is painted on the web's ground, to the width"):
      val rows: List[Teletype] = renderer.block(sample, 40)
      val painted: Boolean = shown(rows).all(_.contains(t"48;2;27;1;33"))
      (rows.map(_.plain.length), rows.map(_.plain.trim), painted)

    . assert(_ == (List(40), List(t"val x = 1 // one"), true))

    test(m"a comment is set in italics"):
      val comment: Teletype = renderer.token(Token(t"// one", Token.Accent.Comment))
      val plain: Teletype = renderer.token(Token(t"one", Token.Accent.Term))
      shown(List(comment, plain))

    . assert(_ == List(t"\e[38;2;159;184;58;3m// one\e[39;23m", t"\e[38;2;255;204;153mone\e[39m"))

    test(m"a theme with no ground for code leaves the terminal's"):
      shown(bare.block(sample, 40)).all(_.contains(t"48;2;"))

    . assert(_ == false)

    test(m"code notes leave the text intact"):
      val code =
        Block.Code
          ( Language.Scala,
            List(Block.Line(List(Token(t"val", Token.Accent.Keyword), Token.plain(t" xs = 1")))),
            List(Block.Note(0, 2, 6, Block.Note.Style.Erroneous)) )

      bare.block(code, 80).at(Prim).let(_.plain)

    . assert(_ == t"val xs = 1")

    test(m"a numbered code block carries its line numbers in the margin"):
      val lines: List[Block.Line] =
        List(Block.Line(List(Token.plain(t"a"))), Block.Line(List(Token.plain(t"b"))))

      val code = Block.Code(Language.Plain, lines, Nil, 9)
      bare.block(code, 80).map(_.plain)

    . assert(_ == List(t" 9│ a", t"10│ b"))

    test(m"a caret is drawn beneath its position, with its caption"):
      val note = Block.Note(0, 8, 8, Block.Note.Style.Erroneous, Inline.text(t"not found"))
      val line: Block.Line = Block.Line(List(Token.plain(t"val x = y")))
      val code = Block.Code(Language.Plain, List(line), List(note))
      bare.block(code, 80).map(_.plain)

    . assert(_ == List(t"val x = y", t"       ╱╲ not found"))

    test(m"a captioned segment is underscored beneath its line, within the margin"):
      val note = Block.Note(0, 4, 5, Block.Note.Style.Caution, Inline.text(t"unused"))
      val line: Block.Line = Block.Line(List(Token.plain(t"val x = 1")))
      val code = Block.Code(Language.Plain, List(line), List(note), 12)

      bare.block(code, 80).map(_.plain)

    . assert(_ == List(t"12│ val x = 1", t"  │     ‾ unused"))
