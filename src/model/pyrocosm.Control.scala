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
    // completion candidates at the caret, whether the text is an incomplete prefix (which
    // decides what Enter does), and `detail`: blocks shown beneath the text while it is being
    // typed, wrapped to the field's width: what the input is being read as, what the unfinished
    // line has brought into scope, and so on. Supplied by the application, usually in response
    // to `Edited`.
    case class Decoration
      ( tokens:      List[Token]      = Nil,
        completions: List[Completion] = Nil,
        incomplete:  Boolean          = false,
        detail:      List[Block]      = Nil,
        marks:       List[Block.Note] = Nil )  // error and warning spans over the tokens, by line

    // A candidate at the caret. Accepting it replaces the identifier before the caret with
    // `name`, or, when `whole` is set, the whole text: a REPL's `/session name` completes the
    // line, and a path drills further on the next Tab.
    case class Completion(name: Text, kind: Text, signature: Text, whole: Boolean = false)

    // Whether the application wants to hear every keystroke (`Edited`) or only submissions.
    enum Notify:
      case Keystrokes, Submissions

enum Control:
  case Button(label: List[Inline], action: Action, enabled: Live[Boolean] = Live(true))

  // `history` is the field's recall list, oldest first, owned by the application: it seeds it
  // (from a file, say) and appends to it on `Submitted`; a frontend only walks it.
  case Field
    ( input:      Input,
      kind:       Control.Field.Kind,
      value:      Live[Text]                     = Live(""),
      decoration: Live[Control.Field.Decoration] = Live(Control.Field.Decoration()),
      notification: Control.Field.Notify         = Control.Field.Notify.Submissions,
      placeholder: Optional[Text]                = Unset,
      history:    Live[List[Text]]               = Live(Nil) )

  case Choice(choice: pyrocosm.Choice, options: List[List[Inline]], current: Live[Int] = Live(0))
  case Toggle(toggle: pyrocosm.Toggle, label: List[Inline], state: Live[Boolean] = Live(false))
