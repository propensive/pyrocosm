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

// The web as a `Frontend`: one page for the interface, one WebSocket per tab. Every `Live` cell
// is bound, for the duration, to a wake that re-renders the element it belongs to and pushes the
// patch to every connected tab; every incoming message is checked against the handles the
// interface actually offers before it becomes an `Event`. `run` serves until `stop()`.
class WebFrontend(port: Int, theme: WebTheme = WebTheme.default)
  ( using monitor: Monitor, probate: Probate, errorPage: WebserverErrorPage )
extends pyrocosm.Frontend:

  private val stopped: juc.CountDownLatch = juc.CountDownLatch(1)
  private val connections: juc.ConcurrentHashMap[Int, Channel] = juc.ConcurrentHashMap()
  private val counter: juca.AtomicInteger = juca.AtomicInteger(0)

  def stop(): Unit = stopped.countDown()

  def run(interface: Interface)(handle: Event => Unit): Unit =
    val renderer = HtmlRenderer()
    val page = PyrocosmPage(interface, renderer, theme)

    def broadcast(patch: Patch): Unit =
      val text = patch.in[Json].show
      connections.values.nn.forEach { channel => channel.nn.send(Message.Text(text)) }

    // What each cell repaints: a panel's content, or a control's holder.
    def bind(): Unit =
      interface.panels.each: panel =>
        panel.content.bindWake { () => broadcast(Patch(t"replace", t"${HtmlRenderer.panelId(panel)}-content", renderer.blocks(panel.content()).show)) }

      (interface.controls + interface.panels.bind(_.controls)).each:
        case Control.Field(input, _, value, decoration, _, _) =>
          decoration.bindWake { () =>
            interface.fields.seek(_.input == input).let: field =>
              broadcast(Patch(t"replace", t"${input.id}-decoration", page.decoration(field).show)) }
          value.bindWake { () => broadcast(Patch(t"value", input.id, value())) }

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

          case _ =>
            ()

    bind()

    val service = SocketServer(port).handle:
      request.target match
        case t"/" | t"/index.html" =>
          val html: Text = t"<!DOCTYPE html>${page.html.show}"
          Http.Ok(List(Http.Header(t"content-type", t"text/html; charset=utf-8")), Http.Body.Fixed(html.in[Data]))

        case t"/pyrocosm.js" =>
          Http.Ok(List(Http.Header(t"content-type", t"text/javascript; charset=utf-8")), Http.Body.Fixed(WebFrontend.script.in[Data]))

        case t"/socket" =>
          Http.Response:
            val id = counter.incrementAndGet()
            val channelHolder: juca.AtomicReference[Channel | Null] = juca.AtomicReference()

            // As flame's web front-end: the handler is vouched pure, the request sealed, and
            // the websocket's tracked captures discarded so it can be the response body.
            val handler: Message -> coaxial.Control[Unit] =
              caps.unsafe.unsafeAssumePure: (message: Message) =>
                message match
                  case Message.Text(payload) => receive(payload)
                  case _                     => ()
                coaxial.Control.Continue(())

            val handle0: (state: Unit) ?=> Message -> coaxial.Control[Unit] = handler
            val request0: Http.Request = caps.unsafe.unsafeAssumePure(summon[Http.Request])
            val ws = Websocket(request0, (), (message: Message) => message, handle0)
            channelHolder.set(ws.channel)
            connections.put(id, ws.channel)
            ws.asInstanceOf[Websocket[Message, Unit]]

        case _ =>
          Http.Response(Http.NotFound)(t"Not found")

    try stopped.await()
    finally
      service.stop()
      interface.cells.each(_.unbindWakes())
      handle(Event.Closed)

// The handles an interface offers, for validating what a browser sends.
extension (interface: Interface)
  def fields: List[Control.Field] = controls.sweep { case field: Control.Field => field }
  def toggles: List[Control.Toggle] = controls.sweep { case toggle: Control.Toggle => toggle }
  def choices: List[Control.Choice] = controls.sweep { case choice: Control.Choice => choice }
  def buttons: List[Control.Button] = controls.sweep { case button: Control.Button => button }
  def controls: List[Control] = interface.controls + interface.panels.bind(_.controls)

  def actions: List[Action] =
    interface.buttons.map(_.action) + interface.panels.bind { (panel: Panel) => Actions.of(panel.content()) }
      + interface.shortcuts.map(_.action)
