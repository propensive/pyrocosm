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

import formatting.indentedCssFormatting

// The code module by itself, as a page which is not a Pyrocosm application's uses it: the same
// HTML as the web renderer's, rules which fetch nothing, and a script which speaks to no server.
object CodeTests extends Suite(m"Pyrocosm code tests"):
  def run(): Unit =
    def line(words: List[Text]): Block.Line = Block.Line(words.map(Token.plain(_)))

    val frames: List[List[Block.Line]] =
      List
        ( List(line(List(t"val", t"x"))),
          List(line(List(t"val", t"x", t"=", t"1"))),
          List(line(List(t"val", t"y")), line(List(t"val", t"x", t"=", t"y"))) )

    val animated: Block.Code =
      Block.Code(Language.Plain, frames.head.or(Nil), Nil, Unset, Block.Animation.of(frames))

    val plain: Block.Code = Block.Code(Language.Plain, frames.head.or(Nil), Nil, 3)

    test(m"the code renderer alone draws an animated block as the web renderer does"):
      val alone: Text = CodeRenderer.Standalone().code(animated).show
      (alone == HtmlRenderer().block(animated).show, alone.contains(t"pyro-animation-data"))

    . assert(_ == (true, true))

    test(m"the code renderer alone draws a plain block as the web renderer does"):
      val alone: Text = CodeRenderer.Standalone().code(plain).show
      (alone == HtmlRenderer().block(plain).show, alone.contains(t"pyro-lineno"))

    . assert(_ == (true, true))

    test(m"the code rules name no font to fetch, and colour every accent by a property"):
      val rules: Text = CodeStyles.rules.show

      ( rules.contains(t"@import"), rules.contains(t"@font-face"),
        rules.contains(t"var(--pyro-code-accent-keyword)"), rules.contains(t".pyro-animated") )

    . assert(_ == (false, false, true, true))

    test(m"the code script speaks to no server, and the frontend's script begins with it"):
      ( CodeScript.text.contains(t"WebSocket"), CodeScript.text.contains(t"PyrocosmCode"),
        WebFrontend.script.starts(CodeScript.text), WebFrontend.script.contains(t"WebSocket") )

    . assert(_ == (false, true, true, true))
