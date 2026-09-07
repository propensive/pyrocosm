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
import clavichord.*
import contingency.*
import distillate.*
import gossamer.*
import rudiments.*
import spectacular.*
import vacuous.*

// The inverse of clavichord's rendering of a keypress: `[⌃]+[C]` back to `Ctrl('C')`, `[⇧]+[↵]`
// to `Shift(Enter)`, `[F5]` to `FunctionKey(5)`, `[a]` to `CharKey('a')`. Every piece of the
// rendering is bracketed and pieces are joined by `+`, so the text splits on `]+[`; modifiers
// are applied from the innermost (rightmost) outwards, each checked against what clavichord
// permits it to modify. This belongs in clavichord as a `Decodable in Text` beside its
// `Showable`; it lives here until then.
object Keypresses:
  private type CtrlChar =
    'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' |
    'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | '[' | '\\' | ']' | '^' | '_' | '@'

  private val ctrlChars: Text = t"ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_@"

  private val named: Map[Text, Keypress.EditKey] =
    Map
      ( t"⇥" -> Keypress.Tab,      t"↵" -> Keypress.Enter,    t"⌫" -> Keypress.Backspace,
        t"⌦" -> Keypress.Delete,   t"⎋" -> Keypress.Escape,   t"↑" -> Keypress.Up,
        t"↓" -> Keypress.Down,     t"←" -> Keypress.Left,     t"→" -> Keypress.Right,
        t"↖" -> Keypress.Home,     t"↘" -> Keypress.End,      t"⇞" -> Keypress.PageUp,
        t"⇟" -> Keypress.PageDown, t"⎀" -> Keypress.Insert )

  private def editKey(keypress: Keypress): Optional[Keypress.EditKey] = keypress match
    case Keypress.Tab       => Keypress.Tab
    case Keypress.Enter     => Keypress.Enter
    case Keypress.Backspace => Keypress.Backspace
    case Keypress.Delete    => Keypress.Delete
    case Keypress.Escape    => Keypress.Escape
    case Keypress.Up        => Keypress.Up
    case Keypress.Down      => Keypress.Down
    case Keypress.Left      => Keypress.Left
    case Keypress.Right     => Keypress.Right
    case Keypress.Home      => Keypress.Home
    case Keypress.End       => Keypress.End
    case Keypress.PageUp    => Keypress.PageUp
    case Keypress.PageDown  => Keypress.PageDown
    case Keypress.Insert    => Keypress.Insert
    case _                  => Unset

  private def shiftable(keypress: Keypress): Optional[Keypress.EditKey | Keypress.FunctionKey] =
    keypress match
      case key: Keypress.FunctionKey => key
      case other                     => editKey(other)

  private def shift(keypress: Keypress): Optional[Keypress] = shiftable(keypress).let { (key: Keypress.EditKey | Keypress.FunctionKey) => Keypress.Shift(key) }

  private def alt(keypress: Keypress): Optional[Keypress] = keypress match
    case key: Keypress.Shift => Keypress.Alt(key)
    case other               => shiftable(other).let { (key: Keypress.EditKey | Keypress.FunctionKey) => Keypress.Alt(key) }

  private def ctrl(keypress: Keypress): Optional[Keypress] = keypress match
    case key: Keypress.Alt   => Keypress.Ctrl(key)
    case key: Keypress.Shift => Keypress.Ctrl(key)

    case Keypress.CharKey(char) =>
      if ctrlChars.contains(char) then Keypress.Ctrl(char.asInstanceOf[CtrlChar]) else Unset

    case other =>
      shiftable(other).let { (key: Keypress.EditKey | Keypress.FunctionKey) => Keypress.Ctrl(key) }

  private def meta(keypress: Keypress): Optional[Keypress] = keypress match
    case key: Keypress.Ctrl  => Keypress.Meta(key)
    case key: Keypress.Alt   => Keypress.Meta(key)
    case key: Keypress.Shift => Keypress.Meta(key)
    case other               => shiftable(other).let { (key: Keypress.EditKey | Keypress.FunctionKey) => Keypress.Meta(key) }

  // One bracketed piece, without its brackets: a modifier symbol, or a key.
  private def piece(content: Text): Optional[Piece] =
    if content == t"⌃" then Piece.Modifier(ctrl)
    else if content == t"⌥" then Piece.Modifier(alt)
    else if content == t"⇧" then Piece.Modifier(shift)
    else if content == t"⌘" then Piece.Modifier(meta)
    else if content == t"␣" then Piece.Key(Keypress.CharKey(' '))
    else if named(content).present then named(content).let { (key: Keypress.EditKey) => Piece.Key(key) }
    else if content.length == 1 then Piece.Key(Keypress.CharKey(content.s.charAt(0)))
    else if content.starts(t"F") && content.length > 1 then
      safely(content.s.substring(1).nn.tt.as[Int]).let { (number: Int) => Piece.Key(Keypress.FunctionKey(number)) }
    else if content.starts(t"⎋") && content.length == 2 then
      Piece.Key(Keypress.EscapeSeq(content.s.charAt(1)))
    else Unset

  private enum Piece:
    case Modifier(apply: Keypress => Optional[Keypress])
    case Key(keypress: Keypress)

  def parse(text: Text): Optional[Keypress] =
    if !(text.starts(t"[") && text.ends(t"]")) then Unset else
      val inner: Text = text.s.substring(1, text.s.length - 1).nn.tt
      val pieces: List[Optional[Piece]] = inner.cut(t"]+[").map { (content: Text) => piece(content) }

      val present: List[Piece] = pieces.sweep { case piece: Piece => piece }

      if present.stdlib.length != pieces.stdlib.length then Unset else

        present.reverse match
          case Piece.Key(key) :: modifiers =>
            modifiers.stdlib.foldLeft(key: Optional[Keypress]):
              case (acc, Piece.Modifier(apply)) => acc.let(apply)
              case (_, Piece.Key(_))            => Unset

          case _ =>
            Unset
