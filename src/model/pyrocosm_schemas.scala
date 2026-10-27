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

import stratiform.TelSchematic

// The derived schemas of the model's sums, which the givens in
// `pyrocosm_codecs.scala` wrap. Each is an object of its own, since a derivation inlines the
// whole of its type, and two in one class (or these with the codecs) exceed what a class may
// hold; each is lazy, since the sums are recursive through the givens which wrap them.
private[pyrocosm] object InlineSchema:
  lazy val derived: Inline is TelSchematic over Tels.Type =
    stratiform.TelsDerivation.derived[Inline]

private[pyrocosm] object BlockSchema:
  lazy val derived: Block is TelSchematic over Tels.Type = stratiform.TelsDerivation.derived[Block]

private[pyrocosm] object DestinationSchema:
  lazy val derived: Inline.Destination is TelSchematic over Tels.Type =
    stratiform.TelsDerivation.derived[Inline.Destination]

private[pyrocosm] object StatusSchema:
  lazy val derived: Status is TelSchematic over Tels.Type =
    stratiform.TelsDerivation.derived[Status]

private[pyrocosm] object TransitionKindSchema:
  lazy val derived: Block.Transition.Kind is TelSchematic over Tels.Type =
    stratiform.TelsDerivation.derived[Block.Transition.Kind]
