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

import anticipation.*
import vacuous.*

// The interactive elements. Each names a handle the application created, and holds its state
// in `Live` cells so that the application can enable a button, replace a field's text or move
// a selection from anywhere, and the frontend follows.
object Control:
  object Field:
    enum Kind:
      case Line
      case Multiline
      case Code(language: Language)

    // What a renderer draws over the user's text: highlighting tokens covering the whole value,
    // completion candidates at the caret, and whether the text is an incomplete prefix (which
    // decides what Enter does). Supplied by the application, usually in response to `Edited`.
    case class Decoration
      ( tokens:      List[Token]      = Nil,
        completions: List[Completion] = Nil,
        incomplete:  Boolean          = false )

    case class Completion(name: Text, kind: Text, signature: Text)

    // Whether the application wants to hear every keystroke (`Edited`) or only submissions.
    enum Notify:
      case Keystrokes, Submissions

enum Control:
  case Button(label: List[Inline], action: Action, enabled: Live[Boolean] = Live(true))

  case Field
    ( input:      Input,
      kind:       Control.Field.Kind,
      value:      Live[Text]                     = Live(""),
      decoration: Live[Control.Field.Decoration] = Live(Control.Field.Decoration()),
      notification: Control.Field.Notify         = Control.Field.Notify.Submissions,
      placeholder: Optional[Text]                = Unset )

  case Choice(choice: pyrocosm.Choice, options: List[List[Inline]], current: Live[Int] = Live(0))
  case Toggle(toggle: pyrocosm.Toggle, label: List[Inline], state: Live[Boolean] = Live(false))
