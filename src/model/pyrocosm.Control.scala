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

// Excluded from the umbrella: `Language` (cosmopolite), which would outrank this package's own
// definitions, since a wildcard import beats a package member declared in another file.
import soundness.{Language as _, *}

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
    // decides what Enter does), `detail`: blocks shown beneath the text while it is being
    // typed, wrapped to the field's width: what the input is being read as, what the unfinished
    // line has brought into scope, and so on; and `prompt`: a short phrase drawn before the
    // first row of the text, as a shell's prompt is, which the text starts past. Supplied by
    // the application, usually in response to `Edited`; and `version`: the version of the text
    // (as `Edited` reported it) the decoration describes. A frontend paints a decoration of the
    // current version as it is, and one of an older version rebased through the edits made
    // since (see `Token.rebase`), so the text is never shown plain while a decoration is
    // awaited, and never with the decoration of some other text. Zero means unversioned: the
    // decoration is used while its tokens spell out the text exactly.
    case class Decoration
      ( tokens:      List[Token]      = Nil,
        completions: List[Completion] = Nil,
        incomplete:  Boolean          = false,
        detail:      List[Block]      = Nil,
        marks:       List[Block.Note] = Nil, // error and warning spans over the tokens, by line
        prompt:      List[Inline]     = Nil,
        version:     Int              = 0 )

    // A candidate at the caret. Accepting it replaces the identifier before the caret with
    // `name`, or, when `whole` is set, the whole text: a REPL's `/session name` completes the
    // line, and a path drills further on the next Tab.
    case class Completion(name: Text, kind: Text, signature: Text, whole: Boolean = false)

    // One edit of a field's text: `removed` characters from `start` (possibly none) replaced
    // by `inserted` (possibly empty). A keystroke, a paste or a recall is one splice; the
    // frontend keeps the recent ones so a decoration of an older version can be rebased.
    case class Splice(start: Int, removed: Int, inserted: Text)

    // The application's proposal to replace the text, as it does when a completion is accepted.
    // The frontend owns the text and numbers its versions, so it honours a proposal only while
    // `base` is still the text's version: one overtaken by a keystroke is dropped, and the
    // application, hearing that keystroke's `Edited`, may propose again.
    case class Revision(base: Int, text: Text, caret: Int)

    extension (field: Field)
      def revise(base: Int, text: Text, caret: Int): Unit =
        field.revision() = Revision(base, text, caret)

    // Whether the application wants to hear every keystroke (`Edited`) or only submissions.
    enum Notify:
      case Keystrokes, Submissions

enum Control:
  case Button(label: List[Inline], action: Action, enabled: Live[Boolean] = Live(true))

  // `history` is the field's recall list, oldest first, owned by the application: it seeds it
  // (from a file, say) and appends to it on `Submitted`; a frontend only walks it.
  // The text itself is the frontend's: it is edited there, and each edit gives it a new version,
  // reported by `Edited`. `value` mirrors it for the application to read; the application
  // changes it through `revision` (see `Field.revise`), a replacement conditional on the
  // version it was based on, so a proposal and a keystroke never overwrite one another.
  case Field
    ( input:      Input,
      kind:       Control.Field.Kind,
      value:      Live[Text]                     = Live(""),
      decoration: Live[Control.Field.Decoration] = Live(Control.Field.Decoration()),
      notification: Control.Field.Notify         = Control.Field.Notify.Submissions,
      placeholder: Optional[Text]                = Unset,
      history:    Live[List[Text]]               = Live(Nil),
      revision:   Live[Optional[Control.Field.Revision]] = Live(Unset) )

  case Choice(choice: pyrocosm.Choice, options: List[List[Inline]], current: Live[Int] = Live(0))
  case Toggle(toggle: pyrocosm.Toggle, label: List[Inline], state: Live[Boolean] = Live(false))
