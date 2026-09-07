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

import scala.reflect.ClassTag

import denominative.dysasymptotics.linearSize
import rudiments.*
import vacuous.*

// A hint is something a renderer *may* honour, never something the application depends on. The
// bag is open and typed: any renderer looks up the hint types it understands and ignores the
// rest, so a web-only hint on a panel shown in the terminal costs nothing.
trait Hint

object Hints:
  val none: Hints = Hints(Nil)
  def apply(hints: Hint*): Hints = Hints(List(hints*))

case class Hints(hints: List[Hint]):
  def apply[hint <: Hint: ClassTag]: Optional[hint] =
    hints.seek { (hint: Hint) => summon[ClassTag[hint]].runtimeClass.isInstance(hint) }
    . let(_.asInstanceOf[hint])

  def has[hint <: Hint: ClassTag]: Boolean = apply[hint].present
  def +(hint: Hint): Hints = Hints(hints :+ hint)
