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

import java.util.concurrent as juc
import java.util.concurrent.atomic as juca

import scala.caps

import anticipation.*
import clavichord.*
import contingency.*
import gossamer.*
import hieroglyph.*
import honeycomb.*
import jacinta.*
import parasite.*
import perihelion.*
import quantitative.*
import rudiments.*
import scintillate.*
import spectacular.*
import symbolism.*
import telekinesis.*
import turbulence.*
import vacuous.*

import denominative.dysasymptotics.linearSize
import hieroglyph.charEncoders.utf8Encoder
import jacinta.formatting.compactJsonFormatting
import contingency.strategies.throwUnsafely
import eucalyptus.logging.silentLogging

// The messages the frontend and its script exchange as JSON. Flat case classes, so the shape is
// plain for the script: a patch replaces the inner HTML of an element by id; an incoming message
// names a handle by id.
case class Patch(kind: Text, id: Text, html: Text = t"")
case class Incoming(kind: Text, id: Text = t"", text: Text = t"", caret: Int = 0, index: Int = 0)

object WebFrontend:
  // The script, from the module's resources.
  lazy val script: Text =
    val stream = getClass.getResourceAsStream("/pyrocosm.js")
    if stream == null then t"" else new String(stream.nn.readAllBytes(), "UTF-8").tt

  // How long a session outlives its last tab, so a reload or a dropped connection resumes it.
  private val grace: Quantity[Seconds[1]] = 30.0*Second

// The web as a `Frontend`, in two modes. `run` shows one interface to every tab, which is how
// a Fume run is watched. `serve` opens an interface per page: each tab gets a session of its
// own, named in the page and by its socket, which is how a REPL is used; a session ends, with
// `Event.Closed`, once its last tab has been gone for a while. In both, every `Live` cell is
// bound to a wake that re-renders the element it belongs to and pushes the patch to the
// session's tabs, and every incoming message is checked against the handles its interface
// actually offers before it becomes an `Event`. Both serve until `stop()`.
// `fallback` answers any request the frontend does not (an application's own API, say); a
// request it declines is a 404.
class WebFrontend
  ( port:     Int,
    theme:    WebTheme = WebTheme.default,
    fallback: Http.Request => Optional[Http.Response] = _ => Unset )
  ( using monitor: Monitor, probate: Probate, errorPage: WebserverErrorPage )
extends pyrocosm.Frontend:

  private val stopped: juc.CountDownLatch = juc.CountDownLatch(1)
  private val counter: juca.AtomicInteger = juca.AtomicInteger(0)
  private val sessions: juc.ConcurrentHashMap[Text, Session] = juc.ConcurrentHashMap()

  @caps.unsafe.untrackedCaptures
  @volatile
  private var shared: Optional[Session] = Unset

  @caps.unsafe.untrackedCaptures
  @volatile
  private var opener: (() -> (Interface, Event -> Unit)) | Null = null

  def stop(): Unit = stopped.countDown()

  // One interface and the tabs showing it. The handler is vouched pure, as the terminal
  // frontend's is: it lives exactly as long as the session.
  private class Session(val id: Text, val interface: Interface, handle: Event -> Unit):
    val renderer: HtmlRenderer = HtmlRenderer()
    val page: PyrocosmPage = PyrocosmPage(interface, renderer, theme, if shared.present then Unset else id)
    private val channels: juc.ConcurrentHashMap[Int, Channel] = juc.ConcurrentHashMap()

    @caps.unsafe.untrackedCaptures
    @volatile
    private var closed: Boolean = false

    // A tab whose channel can no longer be sent to has gone.
    def broadcast(patch: Patch): Unit =
      val text = patch.in[Json].show
      channels.forEach: (connection, channel) =>
        try channel.nn.send(Message.Text(text))
        catch case _: Exception => detach(connection.nn.intValue)

    // What each cell repaints: a panel's content, or a control's holder.
    def bind(): Unit =
      interface.panels.each: panel =>
        panel.content.bindWake { () => broadcast(Patch(t"replace", t"${HtmlRenderer.panelId(panel)}-content", renderer.blocks(panel.content()).show)) }

      (interface.controls + interface.panels.bind(_.controls)).each:
        case Control.Field(input, _, value, decoration, _, _, history) =>
          decoration.bindWake { () =>
            interface.fields.seek(_.input == input).let: field =>
              broadcast(Patch(t"replace", t"${input.id}-decoration", page.decoration(field).show)) }
          value.bindWake { () => broadcast(Patch(t"value", input.id, value())) }
          history.bindWake { () => broadcast(Patch(t"history", input.id, history().in[Json].show)) }

        case Control.Button(_, action, enabled) =>
          enabled.bindWake { () => broadcast(Patch(if enabled() then t"enable" else t"disable", action.id)) }

        case Control.Toggle(toggle, _, state) =>
          state.bindWake { () => broadcast(Patch(if state() then t"check" else t"uncheck", toggle.id)) }

        case Control.Choice(choice, _, current) =>
          current.bindWake { () => broadcast(Patch(t"select", choice.id, current().toString.tt)) }

    // An incoming message, checked against the interface, becomes an event.
    def receive(payload: Text): Unit =
      safely(payload.read[Json].as[Incoming]).let: incoming =>
        incoming.kind match
          case t"press" =>
            interface.actions.seek(_.id == incoming.id).let { action => handle(Event.Pressed(action)) }

          case t"edit" =>
            interface.fields.seek(_.input.id == incoming.id).let: field =>
              field.value() = incoming.text
              if field.notification == Control.Field.Notify.Keystrokes
              then handle(Event.Edited(field.input, incoming.text, incoming.caret))

          case t"submit" =>
            interface.fields.seek(_.input.id == incoming.id).let: field =>
              field.value() = t""
              handle(Event.Submitted(field.input, incoming.text))

          case t"toggle" =>
            interface.toggles.seek(_.toggle.id == incoming.id).let: toggle =>
              toggle.state() = !toggle.state()
              handle(Event.Toggled(toggle.toggle, toggle.state()))

          case t"choose" =>
            interface.choices.seek(_.choice.id == incoming.id).let: choice =>
              choice.current() = incoming.index
              handle(Event.Chosen(choice.choice, incoming.index))

          case t"key" =>
            Keypresses.parse(incoming.text).let { keypress => handle(Event.Key(keypress)) }

          // The script's keep-alive; answered so the connection sees traffic both ways.
          case t"ping" =>
            broadcast(Patch(t"pong", t""))

          case _ =>
            ()

    def attach(connection: Int, channel: Channel): Unit = channels.put(connection, channel)

    // A tab has gone (its channel refused a patch); a per-page session ends once none is left
    // for the grace period. A tab that leaves silently is noticed at the next patch.
    def detach(connection: Int): Unit =
      channels.remove(connection)

      if channels.isEmpty && shared.absent then
        async:
          snooze(WebFrontend.grace)
          if channels.isEmpty then close()

        ()

    def close(): Unit = synchronized:
      if !closed then
        closed = true
        sessions.remove(id)
        interface.cells.each(_.unbindWakes())
        handle(Event.Closed)

  // A session holds the monitor (for its grace timer) and its handler, both of which outlive
  // it; it is vouched pure so it can be kept and looked up.
  private def open(interface: Interface, handle: Event -> Unit): Session =
    val session: Session = caps.unsafe.unsafeAssumePure(new Session(Handles.fresh('s'), interface, handle))
    session.bind()
    sessions.put(session.id, session)
    session

  // One interface for every tab.
  def run(interface: Interface)(handle: Event => Unit): Unit =
    shared = open(interface, caps.unsafe.unsafeAssumePure(handle))
    serving()

  // An interface per page, from `open`, each with its own handler. Both are vouched pure, as
  // `run`'s handler is: they live as long as the frontend serves.
  def serve(open: () => (Interface, Event => Unit)): Unit =
    val open0: () -> (Interface, Event => Unit) = caps.unsafe.unsafeAssumePure(open)

    val opener0: () -> (Interface, Event -> Unit) =
      caps.unsafe.unsafeAssumePure: () =>
        val pair = open0()
        (pair(0), caps.unsafe.unsafeAssumePure(pair(1)))

    opener = opener0
    serving()

  // The session a request is for: the shared one, the one its `session` parameter names, or,
  // for a page in per-page mode, a new one.
  private def sessionFor(target: Text, fresh: Boolean): Optional[Session] =
    val query: Text = target.cut(t"?").stdlib.lift(1).getOrElse(t"")

    val named: Optional[Text] =
      query.cut(t"&").seek(_.starts(t"session=")).let(_.skip(t"session=".length))

    shared.or:
      named.let { id => Optional(sessions.get(id)) }.or:
        val open0 = opener
        if fresh && open0 != null then
          val (interface, handle) = open0()
          this.open(interface, handle)
        else Unset

  private def serving(): Unit =
    val service = SocketServer(port).handle:
      val path: Text = request.target.cut(t"?").stdlib.headOption.getOrElse(t"/")

      path match
        case t"/" | t"/index.html" =>
          sessionFor(request.target, fresh = true).lay(Http.Response(Http.NotFound)(t"No session")): session =>
            val html: Text = t"<!DOCTYPE html>${session.page.html.show}"
            Http.Ok(List(Http.Header(t"content-type", t"text/html; charset=utf-8")), Http.Body.Fixed(html.in[Data]))

        case t"/pyrocosm.js" =>
          Http.Ok(List(Http.Header(t"content-type", t"text/javascript; charset=utf-8")), Http.Body.Fixed(WebFrontend.script.in[Data]))

        case t"/socket" =>
          sessionFor(request.target, fresh = false).lay(Http.Response(Http.NotFound)(t"No session")): session =>
            Http.Response:
              val id = counter.incrementAndGet()

              // As flame's web front-end: the handler is vouched pure, the request sealed, and
              // the websocket's tracked captures discarded so it can be the response body.
              val handler: Message -> coaxial.Control[Unit] =
                caps.unsafe.unsafeAssumePure: (message: Message) =>
                  message match
                    case Message.Text(payload) => session.receive(payload)
                    case _                     => ()
                  coaxial.Control.Continue(())

              val handle0: (state: Unit) ?=> Message -> coaxial.Control[Unit] = handler
              val request0: Http.Request = caps.unsafe.unsafeAssumePure(summon[Http.Request])
              val ws = Websocket(request0, (), (message: Message) => message, handle0)
              session.attach(id, ws.channel)
              ws.asInstanceOf[Websocket[Message, Unit]]

        case _ =>
          val request0: Http.Request = caps.unsafe.unsafeAssumePure(summon[Http.Request])
          fallback(request0).or(Http.Response(Http.NotFound)(t"Not found"))

    try stopped.await()
    finally
      service.cancel()
      sessions.values.nn.forEach { session => session.nn.close() }
      shared = Unset
      opener = null

// The handles an interface offers, for validating what a browser sends. Every control counts:
// the interface's own and those of its panels (the name says so, since `controls` unqualified
// is the interface's own).
extension (interface: Interface)
  def fields: List[Control.Field] = interface.allControls.sweep { case field: Control.Field => field }
  def toggles: List[Control.Toggle] = interface.allControls.sweep { case toggle: Control.Toggle => toggle }
  def choices: List[Control.Choice] = interface.allControls.sweep { case choice: Control.Choice => choice }
  def buttons: List[Control.Button] = interface.allControls.sweep { case button: Control.Button => button }
  def allControls: List[Control] = interface.controls + interface.panels.bind(_.controls)

  def actions: List[Action] =
    interface.buttons.map(_.action) + interface.panels.bind { (panel: Panel) => Actions.of(panel.content()) }
      + interface.shortcuts.map(_.action)
