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

import soundness.{Control as _, *}

// The handles an interface offers, for validating what a browser sends. Every control counts:
// the interface's own and those of its panels (the name says so, since `controls` unqualified
// is the interface's own).
extension (interface: Interface)
  def fields: List[Control.Field] =
    interface.allControls.sweep { case field: Control.Field => field }

  def toggles: List[Control.Toggle] =
    interface.allControls.sweep { case toggle: Control.Toggle => toggle }

  def choices: List[Control.Choice] =
    interface.allControls.sweep { case choice: Control.Choice => choice }

  def buttons: List[Control.Button] =
    interface.allControls.sweep { case button: Control.Button => button }

  def allControls: List[Control] = interface.controls + interface.panels.bind(_.controls)

  def actions: List[Action] =
    interface.buttons.map(_.action) +
      interface.panels.bind { (panel: Panel) => Actions.of(panel.content()) } +
      interface.shortcuts.map(_.action)
