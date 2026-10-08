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

import soundness.{Service as _, *}

// The one place which knows both the swarm and the frontends' `Presence`: what this process's
// frontends show goes out on the bus, and what the bus brings in is laid out for the frontends.
// An activity of this process which opens a view here — an internal destination — is
// published with the relative URL which opens it (`/open?activity=<id>`), which a viewer
// prefixes with this node's web URL; one of a node without a web front-end is published with
// no destination at all.
object Presences:
  // Binds the shared presence to `swarm`, the process's node.
  def start(swarm: Swarm)(using Monitor): Unit =
    val presence: Presence = Presence.shared
    val portable: Live[List[Activity]] = Live(Nil)

    presence.local.bindWake { () => portable() = presence.local().map(outgoing(_)) }
    swarm.expose(portable)

    presence.web.bindWake { () => swarm.advertise() }
    swarm.activities.bindWake { () => presence.remote() = incoming(swarm, swarm.activities()) }
    swarm.nodes.bindWake { () => presence.peers() = origins(swarm, swarm.nodes()) }

  // An activity as it travels: an internal destination becomes the relative URL which opens it.
  private def outgoing(activity: Activity): Activity = activity.destination match
    case Inline.Destination.Internal(_) => activity.copy(destination = Inline.Destination.External(opener(activity.id)))
    case _                              => activity

  private def opener(id: Text): Text = t"/open?activity=$id"

  // Where a node's web front-end is, as seen from here: `localhost` for a node of this machine,
  // and the nearest of its addresses otherwise.
  private def url(swarm: Swarm, advert: Advert): Optional[Text] =
    advert.services.filter(_.name == t"serve").prim.let: service =>
      val host: Text =
        if advert.node.shares(swarm.node) then t"localhost"
        else Machine.ordered(advert.node.hosts).prim.or(advert.node.machine)

      located(host, service.port)

  private def located(host: Text, port: Int): Text = t"http://$host:${port.show}/"

  private def key(node: Node): Text = t"${node.tool}-${node.identity.keep(8)}-${node.pid.show}"

  private def origin(swarm: Swarm, advert: Advert): Origin =
    val node: Node = advert.node

    Origin
      ( key(node), node.tool, node.machine, url(swarm, advert), node.key == swarm.node.key,
        node.shares(swarm.node) )

  // Every node, this one included, in menu order: by tool, then machine, then address. Two
  // nodes of one tool on one machine are told apart by their ports.
  private def origins(swarm: Swarm, adverts: List[Advert]): List[Origin] =
    val all: scala.List[(Origin, Advert)] = adverts.map { advert => (origin(swarm, advert), advert) }.stdlib

    def named(origin: Origin, advert: Advert): Origin =
      val twins: Int = all.count { (other, _) => other.tool == origin.tool && other.machine == origin.machine }
      val port: Optional[Int] = advert.services.filter(_.name == t"serve").prim.let(_.port)
      if twins > 1 then port.lay(origin) { port => origin.copy(tool = t"${origin.tool}:${port.show}") } else origin

    val labelled: scala.List[Origin] = all.map(named(_, _))
    List(labelled.sortBy { origin => (origin.tool.s, origin.machine.s, origin.url.or(t"").s) }*)

  // Other nodes' activities, each with its origin, a relative destination made absolute by the
  // origin's web URL, or dropped where there is none.
  private def incoming(swarm: Swarm, activities: List[(Node, Activity)]): List[(Origin, Activity)] =
    val adverts: List[Advert] = swarm.nodes()

    val placed: List[(Origin, Activity)] = activities.bind: (node, activity) =>
      adverts.seek(_.node.key == node.key).lay(Nil: List[(Origin, Activity)]): advert =>
        val from: Origin = origin(swarm, advert)

        val destination: Optional[Inline.Destination] = activity.destination match
          case Inline.Destination.External(path) if path.starts(t"/") =>
            from.url.let { base => Inline.Destination.External(t"${base.keep(base.length - 1)}$path") }

          case Inline.Destination.Internal(_) => Unset
          case other                          => other

        List((from, activity.copy(destination = destination)))

    val sorted: scala.List[(Origin, Activity)] =
      placed.stdlib.sortBy { (origin, _) => (origin.tool.s, origin.machine.s, origin.url.or(t"").s) }

    List(sorted*)
