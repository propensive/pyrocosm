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
import fulminate.*
import gossamer.*
import jacinta.*
import probably.*
import punctuation.*
import spectacular.*
import stratiform.*
import turbulence.*
import vacuous.*

import contingency.strategies.throwUnsafely
import hieroglyph.charEncoders.utf8Encoder
import jacinta.discriminables.jsonByKindDiscriminable
import jacinta.formatting.compactJsonFormatting

case class Person(name: Text, age: Int)

// The shape of the model's `Inline` and `Block`: a sum whose variants recurse through a `List` of
// the sum itself. Both codecs must derive it in place, with no hand-anchored instance.
enum Node derives CanEqual:
  case Leaf(text: Text)
  case Branch(children: List[Node])

enum Direct derives CanEqual:
  case Leaf
  case Branch(left: Direct, value: Int, right: Direct)

object Tests extends Suite(m"Pyrocosm tests"):
  def run(): Unit =
    test(m"a live cell's assignment publishes the value"):
      val live = Live(1)
      live() = 2
      live()
    . assert(_ == 2)

    test(m"a live cell's assignment wakes every bound frontend"):
      val live = Live(t"a")
      var wakes = 0
      live.bindWake(() => wakes += 1)
      live.bindWake(() => wakes += 1)
      live() = t"b"
      wakes
    . assert(_ == 2)

    test(m"append extends a live list cell"):
      // `List(1, 2)` is a `List[Int] & Populated`; the cell must be typed as the plain list.
      val live = Live(List(1, 2): List[Int])
      live.append(3)
      live()
    . assert(_ == List(1, 2, 3))

    test(m"a hint is found by its type"):
      Hints(hints.Proportion(0.3), hints.web.Card)[hints.Proportion]
    . assert(_ == hints.Proportion(0.3))

    test(m"an absent hint is Unset"):
      Hints(hints.Proportion(0.3))[hints.terminal.MaxRows]
    . assert(_ == Unset)

    test(m"a renderer-specific hint is found by that renderer"):
      Hints(hints.web.Card, hints.terminal.Occupancy.Inline)[hints.terminal.Occupancy]
    . assert(_ == hints.terminal.Occupancy.Inline)

    test(m"text exhibits as itself"):
      t"hello".exhibit
    . assert(_ == Inline.Textual(t"hello"))

    test(m"a case class derives a record titled by its type"):
      val exhibit: Inline | Block = Person(t"Simon", 72).exhibit

      exhibit match
        case Block.Record(entries, title) =>
          entries.stdlib.length == 2 && title == Inline.text(t"Person")

        case _ =>
          false
    . assert(_ == true)

    test(m"a list of values exhibits as a listing"):
      val exhibit: Inline | Block = (List(1, 2, 3): List[Int]).exhibit

      exhibit match
        case Block.Listing(false, items) => items.stdlib.length == 3
        case _                           => false
    . assert(_ == true)

    test(m"markdown converts node for node"):
      val exhibit: Block = Parser.parse(t"# Title\n\nSome *emphasis* here.").exhibit

      exhibit match
        case Block.Group(Block.Heading(1, _) :: Block.Paragraph(content) :: Nil) =>
          content.stdlib.exists:
            case Inline.Emphasis(_) => true
            case _                  => false

        case _ =>
          false
    . assert(_ == true)

    test(m"every live cell is reachable from the interface"):
      val run = Action(t"run")

      val panel =
        Panel
          ( Panel.Id(t"main"),
            Panel.Role.Primary,
            Unset,
            Live(Nil: List[Block]),
            controls = List(Control.Button(Inline.text(t"Run"), run)) )

      val verbose = Control.Toggle(Toggle(), Inline.text(t"Verbose"))
      Interface(Inline.text(t"Gallery"), List(panel), List(verbose)).cells.stdlib.length
    . assert(_ == 3)

    val tree: Node =
      Node.Branch(List(Node.Leaf(t"a"), Node.Branch(List(Node.Leaf(t"b"), Node.Leaf(t"c")))))

    test(m"a sum recursive through a list round-trips as JSON"):
      tree.in[Json].show.read[Json].as[Node]
    . assert(_ == tree)

    test(m"a sum recursive through a list round-trips as TEL"):
      tree.in[Tel].as[Node]
    . assert(_ == tree)

    test(m"a sum recursive through a list round-trips as TEL text"):
      tree.in[Tel].show.read[Tel].as[Node]
    . assert(_ == tree)

    // The derivation tutorial's own example of a type that "cannot be derived in place": direct
    // recursion, with no collection or `Optional` between the sum and itself.
    val direct: Direct = Direct.Branch(Direct.Leaf, 1, Direct.Branch(Direct.Leaf, 2, Direct.Leaf))

    test(m"a directly-recursive sum round-trips as JSON"):
      direct.in[Json].show.read[Json].as[Direct]
    . assert(_ == direct)

    test(m"a directly-recursive sum round-trips as TEL"):
      direct.in[Tel].show.read[Tel].as[Direct]
    . assert(_ == direct)
