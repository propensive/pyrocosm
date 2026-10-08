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

import scala.caps

import soundness.*

// Where a running tool, or an activity of one, comes from, as a frontend needs it to label a
// card and link a menu item — and nothing of the transport. `key` is unique per node and safe
// in an HTML id; `url` is where a browser finds the tool's web front-end, absent when it
// serves none; `local` says it runs on this machine, where its `url` names `localhost`.
case class Origin
  ( key:     Text,
    tool:    Text,
    machine: Text,
    url:     Optional[Text],
    self:    Boolean,
    local:   Boolean )

// The seam between the frontends and the swarm, which neither depends on the other across: the
// frontends attach the interfaces they show, so that every activity of this process is
// published, and show what the swarm has heard — the other tools running, and their
// activities — without knowing how it heard it. There is one per process, `shared`, as the
// swarm has one node per process.
object Presence:
  val shared: Presence = Presence()

class Presence():
  // Every activity of every interface a frontend in this process is showing.
  val local: Live[List[Activity]] = Live(Nil)

  // Other nodes' activities, each with where it comes from; an activity's destination, if it
  // has one, is a URL a browser anywhere can open.
  val remote: Live[List[(Origin, Activity)]] = Live(Nil)

  // Every node on the swarm, this one included, in the order a menu lists them.
  val peers: Live[List[Origin]] = Live(Nil)

  // The port this process's web front-end is serving on, while it is.
  val web: Live[Optional[Int]] = Live(Unset)

  @caps.unsafe.untrackedCaptures
  private var attached: List[Interface] = Nil

  // Follows `interface`'s activities, as long as it is attached.
  def attach(interface: Interface): Unit =
    synchronized { attached = interface :: attached }
    interface.activities.bindWake(recompute)
    recompute()

  def detach(interface: Interface): Unit =
    synchronized { attached = attached.filter(_ ne interface) }
    interface.activities.unbindWake(recompute)
    recompute()

  private val recompute: () -> Unit = caps.unsafe.unsafeAssumePure: () =>
    local() = synchronized(attached).bind(_.activities())
