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
import perihelion.{wsClient, wsSessional}
import probates.cancelProbate
import strategies.throwUnsafely
import threads.platformThreads
import webserverErrorPages.minimalErrorPage

// A panel's content sent as what changed of it: the nodes of its trees, found by `Graft`, and
// the numbering of a cell's assignments, by which a tab knows what a change is from.
object GraftTests extends Suite(m"Pyrocosm graft tests"):
  def run(): Unit =
    // A tree of two packages, the first with two types, drawn for one page and then for
    // another as an application draws it: the nodes on the way to the page anew, and every
    // other the very one drawn before.
    def leaf(name: Text, tone: Optional[Tone] = Unset): Block.TreeNode =
      Block.TreeNode(Inline.text(name), Nil, tone)

    def branch(name: Text, open: Boolean, children: Block.TreeNode*): Block.TreeNode =
      Block.TreeNode(Inline.text(name), List(children*), Unset, Unset, open)

    val alpha: Block.TreeNode = leaf(t"Alpha")
    val beta: Block.TreeNode = leaf(t"Beta")
    val others: Block.TreeNode = branch(t"others", false, leaf(t"Gamma"))

    val closed: List[Block] =
      List(Block.Tree(List(branch(t"letters", false, alpha, beta), others)))

    val marked: List[Block] =
      List(Block.Tree(List(branch(t"letters", true, alpha, leaf(t"Beta", Tone.Accent)), others)))

    def grafts(before: List[Block], after: List[Block]): Optional[List[(Int, Text, Boolean)]] =
      Graft.between(before, after, HtmlRenderer()).let: (grafts: List[Graft]) =>
        grafts.map: (graft: Graft) =>
          (graft.tree, graft.path, graft.whole)

    test(m"content which is the same has nothing to graft"):
      val again: List[Block] =
        List(Block.Tree(List(branch(t"letters", false, alpha, beta), others)))

      (grafts(closed, closed), grafts(closed, again))

    . assert(_ == (List[(Int, Text, Boolean)](), List[(Int, Text, Boolean)]()))

    test(m"a tree's changed nodes alone are grafted, each at its path, not whole"):
      grafts(closed, marked)

    . assert(_ == List((0, t"0", false), (0, t"0.1", false)))

    // The markup of the first node grafted.
    def grafted(before: List[Block], after: List[Block]): Text =
      Graft.between(before, after, HtmlRenderer()).let(_.head).let(_.html).or(t"")

    test(m"a node grafted alone has its label, its state and an empty list for its children"):
      val html: Text = grafted(closed, marked)

      ( html.contains(t"""<details class="pyro-node" open data-node="0">"""),
        html.contains(t"<ul></ul>"), html.contains(t"letters"), html.contains(t"Alpha") )

    . assert(_ == (true, true, true, false))

    test(m"a node with other children than it had is grafted whole, and nothing beneath it"):
      val grown: List[Block] =
        List(Block.Tree(List(branch(t"letters", true, alpha, beta, leaf(t"Delta")), others)))

      (grafts(closed, grown), grafted(closed, grown).contains(t"Delta"))

    . assert(_ == (List((0, t"0", true)), true))

    test(m"a tree is named by its place among the trees at the top of its content"):
      val heading: Block = Block.Heading(3, Inline.text(t"Names"))
      val first: Block = Block.Tree(List(leaf(t"Guide")))

      def content(tone: Optional[Tone]): List[Block] =
        List(first, heading, Block.Tree(List(leaf(t"Name", tone))))

      grafts(content(Unset), content(Tone.Accent))

    . assert(_ == List((1, t"0", false)))

    test(m"content which differs otherwise than in its trees' nodes is not grafted"):
      val paragraph: Block = Block.paragraph(t"One")
      val other: Block = Block.paragraph(t"Two")
      val fewer: List[Block] = List(Block.Tree(List(others)))

      ( grafts(paragraph :: closed, other :: marked), grafts(closed, paragraph :: marked),
        grafts(closed, fewer) )

    . assert(_ == (Unset, Unset, Unset))

    test(m"more changed nodes than the limit are not grafted"):
      def wide(tone: Optional[Tone]): List[Block] =
        List(Block.Tree(List.range(0, Graft.limit + 1).map { index => leaf(index.show, tone) }))

      (grafts(wide(Unset), wide(Tone.Accent)), grafts(wide(Unset), wide(Unset)).let(_.size))

    . assert(_ == (Unset, 0))

    test(m"a cell numbers its assignments, and gives each with its value"):
      val cell: Live[Text] = Live(t"one")
      val first: (Text, Int) = cell.versioned
      cell() = t"two"
      cell.amend(_ + t"!")
      (first, cell.versioned)

    . assert(_ == ((t"one", 0), (t"two!", 2)))

    // A front-end with one panel, whose content is the tree: a tab is sent the nodes of it
    // which changed, as a change from the content its page was sent with; and one which asks
    // for the whole of it, having another, is sent that, numbered as the change was.
    test(m"a tab is sent a tree's changed nodes, and the whole content when it asks"):
      supervise:
        val content: Live[List[Block]] = Live(closed)

        val interface: Interface =
          Interface
            ( Inline.text(t"Trees"),
              List
                ( Panel
                    ( Panel.Id(t"main"), Panel.Role.Primary, Unset, content,
                      Panel.Priority.Essential ) ) )

        val presence: Presence = Presence()
        val frontend: WebFrontend = WebFrontend(0, presence = presence)
        async(frontend.run(interface) { _ => () })

        def waiting(attempts: Int)(condition: => Boolean): Unit =
          if attempts > 0 && !condition then
            snooze(0.2*Second)
            waiting(attempts - 1)(condition)

        waiting(25)(presence.web().present)
        val port: Int = presence.web().or(0)
        def page(): Text = t"http://127.0.0.1:${port.show}/".as[HttpUrl].fetch().receive[Text]
        val url: Websocket.Url = t"ws://127.0.0.1:${port.show}/socket".as[Websocket.Url]

        def message(kind: Text, id: Text): Text =
          t"""{"kind":"$kind","id":"$id","text":"","caret":0,"index":0,"version":0,"label":""}"""

        def text(message: Message): Text = message match
          case Message.Binary(data) => data.utf8
          case Message.Text(text)   => text

        val first: Text = page()
        val attached: Promise[Unit] = Promise()
        val grafted: Promise[Text] = Promise()

        def changed(message: Message): coaxial.Control[Unit] =
          val patch: Text = text(message)
          if patch.contains(t"pong") then attached.offer(())

          if !patch.contains(t"graft") then coaxial.Control.Continue(()) else
            grafted.offer(patch)
            coaxial.Control.Terminate

        async:
          url.exchange(())[Message](changed(_)): transmitter =>
            transmitter.send(Message.Text(message(t"ping", t"")))

        safely(attached.await(5.0*Second))
        content() = marked
        val graft: Text = safely(grafted.await(5.0*Second)).or(t"")
        val second: Text = page()
        val repainted: Promise[Text] = Promise()

        def replaced(message: Message): coaxial.Control[Unit] =
          val patch: Text = text(message)

          if !patch.contains(t"replace") then coaxial.Control.Continue(()) else
            repainted.offer(patch)
            coaxial.Control.Terminate

        async:
          url.exchange(())[Message](replaced(_)): transmitter =>
            transmitter.send(Message.Text(message(t"repaint", t"pyro-panel-main-content")))

        val whole: Text = safely(repainted.await(5.0*Second)).or(t"")
        frontend.stop()

        ( first.contains(t"""data-serial="0""""), second.contains(t"""data-serial="1""""),
          graft.contains(t""""version":1"""), graft.contains(t""""base":0"""),
          graft.contains(t"Alpha"), whole.contains(t""""version":1"""), whole.contains(t"Alpha") )

    . assert(_ == (true, true, true, true, false, true, true))

    // A tab which says where it is hears that the session shows that page, after the page's
    // own patches and under the number the tab gave its asking: what tells its script that
    // the address it shows and the page it shows are one.
    test(m"a tab which asks for a page is sent the page, and then that it is shown"):
      supervise:
        val content: Live[List[Block]] = Live(closed)

        val interface: Interface =
          Interface
            ( Inline.text(t"Trees"),
              List
                ( Panel
                    ( Panel.Id(t"main"), Panel.Role.Primary, Unset, content,
                      Panel.Priority.Essential ) ) )

        val presence: Presence = Presence()
        val frontend: WebFrontend = WebFrontend(0, presence = presence)

        async:
          frontend.run(interface):
            case Event.Located(t"/marked") => content() = marked
            case _                         => ()

        def waiting(attempts: Int)(condition: => Boolean): Unit =
          if attempts > 0 && !condition then
            snooze(0.2*Second)
            waiting(attempts - 1)(condition)

        waiting(25)(presence.web().present)
        val port: Int = presence.web().or(0)
        val url: Websocket.Url = t"ws://127.0.0.1:${port.show}/socket".as[Websocket.Url]

        val asking: Text =
          t"""{"kind":"locate","id":"","text":"/marked","caret":0,"index":0,"version":7,"label":""}"""

        def text(message: Message): Text = message match
          case Message.Binary(data) => data.utf8
          case Message.Text(text)   => text

        val heard: Promise[Text] = Promise()
        val patches: java.util.concurrent.atomic.AtomicReference[Text] =
          java.util.concurrent.atomic.AtomicReference(t"")

        def collect(message: Message): coaxial.Control[Unit] =
          val patch: Text = text(message)
          val seen: Text = patches.updateAndGet { earlier => t"${earlier.nn}$patch\n" }.nn

          if !patch.contains(t"located") then coaxial.Control.Continue(()) else
            heard.offer(seen)
            coaxial.Control.Terminate

        async:
          url.exchange(())[Message](collect(_)): transmitter =>
            transmitter.send(Message.Text(asking))

        val sent: List[Text] = safely(heard.await(5.0*Second)).or(t"").cut(t"\n")
        val first: Text = sent.at(Prim).or(t"")
        val second: Text = sent.at(Sec).or(t"")
        frontend.stop()

        ( first.contains(t""""kind":"graft""""), second.contains(t""""kind":"located""""),
          second.contains(t""""html":"/marked""""), second.contains(t""""version":7""") )

    . assert(_ == (true, true, true, true))
