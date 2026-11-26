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

import soundness.{Channel as _, Control as _, Service as _, Status as _, *}

import codepages.utf8Codepage
import dysasymptotics.{linearAccess, linearSize}
import httpBackends.javaNetHttp
import internetAccess.online
import logging.silentLogging
import perihelion.Websocket.Message

// What a page can press: the actions its content offers for selection, and those its links
// lead to.
object ActionsTests extends Suite(m"Pyrocosm actions tests"):
  def run(): Unit =
    val turn = Action(t"turn")
    val open = Action(t"open")
    val view = Action(t"view")

    val link: Inline =
      Inline.Link(Inline.Destination.Internal(turn), List(Inline.Icon(t"<svg/>", t"Turn")))

    val caption: Block =
      Block.Paragraph(List(Inline.Emphasis(List(Inline.Textual(t"A test "), link))))

    val table: Block =
      Block.Table
        ( List(Block.Column(Inline.text(t"N"))),
          List(Block.Row(List(Block.Cell(Inline.text(t"4"))), action = open)) )

    val content: List[Block] = List(Block.Group(List(caption, table)))

    test(m"a link's internal destination is not an action to select"):
      Actions.of(content)

    . assert(_ == List(open))

    test(m"a link's internal destination is an action its blocks lead to"):
      Actions.linked(content)

    . assert(_ == List(turn))

    test(m"a page can press what its panels' links and its activities' cards lead to"):
      val panel = Panel(Panel.Id(t"main"), Panel.Role.Primary, Unset, Live(content))
      val interface = Interface(Inline.text(t"Test"), List(panel))
      val destination = Inline.Destination.Internal(view)
      val activity = Activity(t"a", Inline.text(t"a"), Status.Fraction(0.5), destination)
      interface.activities() = List(activity)
      interface.actions

    . assert(_ == List(open, turn, view))
