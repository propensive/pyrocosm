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

import soundness.*

import webserverErrorPages.minimalErrorPage

// A tool's web front-end as the daemon serves it, the way every tool serves one: a
// `WebFrontend` on the port, with a session for every tab (`open` gives each its interface and
// its handler), and a ticker driving every live session a few times a second for as long as
// the front-end runs, which is what notices a change the daemon made meanwhile. `stop` ends it
// from another invocation, so the frontend is vouched pure. The port `0` means the hub's
// origin, under `/<tool>/`, which `Tool.address` names.
object WebService:
  // One tab's session: its interface, its handler, and what the ticker does for it.
  case class Session(interface: Interface, handle: Event => Unit, tick: () => Unit = () => ())

  // `fallback` answers the requests the front-end does not — an API the same origin serves.
  def apply
    ( port0:    Int                                           = 0,
      every:    Duration                                      = 0.25*Second,
      fallback: Http.Request -> Optional[Http.Response]       = _ => Unset )
    ( open: () -> Session )
  :   Tool.Web =

    new Tool.Web:
      def port: Int = port0

      @scala.caps.unsafe.untrackedCaptures
      @volatile
      private var frontend: Optional[WebFrontend] = Unset

      def serve(port: Int)(using Monitor, Probate): Unit =
        val serving: Atomic[Boolean] = Atomic(true)
        val live: juc.ConcurrentHashMap[Session, Boolean] = juc.ConcurrentHashMap()

        val running: WebFrontend =
          scala.caps.unsafe.unsafeAssumePure(WebFrontend(port, fallback = fallback))

        frontend = running

        try
          async:
            while serving() do
              live.keySet.nn.forEach: session => session.nn.tick()
              snooze(every)

          running.serve: () =>
            val session: Session = open()
            live.put(session, true)

            // The handler lives as long as the session; vouched pure, as the frontend's are.
            val handle: Event -> Unit = scala.caps.unsafe.unsafeAssumePure: (event: Event) =>
              event match
                case Event.Closed => live.remove(session)
                case _            => ()

              session.handle(event)

            (session.interface, handle)
        finally serving() = false

      def stop(): Unit = frontend.let(_.stop())
