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

// The bolt codes of Pyrocosm and its tools, which begin `↯/f`. Each module of Pyrocosm is a
// subdivision, numbered so that it comes after those it depends on, and the modules of the tools
// follow them, from 15; the record of what every code means is `bolt.tel`, at the root.
object Bolts:
  final val letter = 'f'

  final val harness  = 1
  final val model    = 2
  final val compiler = 3
  final val media    = 4
  final val script   = 5
  final val system   = 6
  final val notes    = 7
  final val remote   = 8
  final val bus      = 9
  final val swarm    = 10
  final val terminal = 11
  final val web      = 12
  final val hub      = 13
  final val cli      = 14

  // The code of the `error`th error of `subdivision`, or of its `variant`th variant.
  def apply(subdivision: Int, error: Int, variant: Int = 0): fulminate.Bolt =
    fulminate.Bolt(letter, subdivision, error, variant)
