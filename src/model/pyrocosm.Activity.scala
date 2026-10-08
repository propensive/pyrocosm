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

import soundness.*

// One unit of concurrent work kept in view while it lasts: a test run, a build, a session.
// Every frontend shows it as a small card — its title, a gauge of its progress and a short
// phrase about how it is going ("green so far", "2 failures"), carrying its own tone — which
// is a link to the work's own view when it has a destination. It is gone once it leaves
// `Interface.activities`.
object Activity:
  // The activity as a block, for a frontend with no card of its own: a gauge captioned by the
  // title and the state.
  def block(activity: Activity): Block =
    val caption: List[Inline] =
      if activity.state.nil then activity.title
      else activity.title + List(Inline.Textual(t" ")) + activity.state

    Block.Gauge(activity.status, caption)

case class Activity
  ( id:          Text,
    title:       List[Inline],
    status:      Status,
    destination: Optional[Inline.Destination] = Unset,
    state:       List[Inline]                 = Nil )
