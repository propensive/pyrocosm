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

import dysasymptotics.linearAccess

// A human language, as it is written: `fr`, or with a region, `fr-CA`. (`Language` is a
// programming language here, which a code block is written in.) An application says which
// languages its matter exists in, and is told which a reader would have; Pyrocosm's own
// interface exists in a few (`Tongue.Chrome`), and follows the reader's as nearly as it can:
// a regional variant reads as its language, and a language it has no words for as English.
object Tongue:
  // The languages Pyrocosm's own interface is written in.
  type Chrome = en & fr & de

  val chrome: List[Text] = List(t"en", t"fr", t"de")

  // The name of the cookie which keeps a reader's choice, for every tool of an origin.
  val cookieName: Text = t"pyro-language"

  private val pattern: java.util.regex.Pattern =
    java.util.regex.Pattern.compile("[A-Za-z]{2,3}(-[A-Za-z0-9]{2,8})*").nn

  def valid(code: Text): Boolean = pattern.matcher(code.s).nn.matches

  // The language of a code, without its region: `fr`, of `fr-CA`.
  def primary(code: Text): Text = code.cut(t"-").at(Prim).or(code).lower

  // The first of the languages `wanted`, in order, which is among those `available`: itself,
  // or failing that one of the same language, a regional variant standing for its language
  // and the language for a variant of it.
  def choose(wanted: List[Text], available: List[Text]): Optional[Text] =
    wanted.bind: (code: Text) =>
      val exact: List[Text] = available.filter(_.lower == code.lower)
      if !exact.nil then exact else available.filter(primary(_) == primary(code))

    . at(Prim)

  // The locale Pyrocosm's own words are read in, for a reader who would have these languages,
  // in order: English, when it has none of them.
  def locale(wanted: List[Text]): Locale[Chrome] = choose(wanted, chrome).or(t"en") match
    case t"fr" => Locale(fr)
    case t"de" => Locale(de)
    case _     => Locale(en)

  // The code of the language a locale reads in.
  def code(locale: Locale[Chrome]): Text = locale.language.code

  // The languages an `Accept-Language` header asks for, the most wanted first: each range's
  // language, ordered by its `q` (one, unless it says), without `*` or a range refused with a
  // `q` of zero. Garbage reads as nothing.
  def accepted(header: Text): List[Text] =
    val ranges: List[(Text, Double)] = header.cut(t",").bind: (range: Text) =>
      val parts: List[Text] = range.cut(t";").map(_.trim)

      val quality: Double =
        parts.seek(_.lower.starts(t"q=")).let: (part: Text) => safely(part.skip(2).as[Double])
        . or(1.0)

      parts.at(Prim).lay(List[(Text, Double)]()): (code: Text) =>
        if valid(code) && quality > 0.0 then List((code, quality)) else List[(Text, Double)]()

    List.from(ranges.stdlib.sortBy { (_, quality) => -quality }.map(_(0)))

  // The language a `Cookie` header keeps as the reader's choice, if it keeps a valid one.
  def kept(cookies: Text): Optional[Text] =
    cookies.cut(t";").map(_.trim).seek(_.starts(t"$cookieName=")).let: (pair: Text) =>
      val code: Text = pair.skip(cookieName.length + 1)
      if valid(code) then code else Unset

  // How each of some languages is named where a reader chooses among them: by its two letters,
  // or in full where another of them has the same two.
  def labels(languages: List[Text]): List[(Text, Text)] = languages.map: (code: Text) =>
    val alone: Boolean = languages.filter(primary(_) == primary(code)).stdlib.size == 1
    (code, if alone then primary(code) else code)

// Who is reading: the languages they would have, the most wanted first — what they chose, then
// what their configuration says, then what their browser asks for. An application which has
// its matter in several languages opens a session in the one `language` picks.
case class Audience(languages: List[Text] = Nil):
  // The language to show, of those an application has: the reader's first choice it has, or
  // its own first, which is the one its matter is written in.
  def language(available: List[Text]): Optional[Text] =
    Tongue.choose(languages, available).or(available.at(Prim))

  // The locale of Pyrocosm's own words for this reader, shown matter in `shown`, if it says.
  def locale(shown: Optional[Text] = Unset): Locale[Tongue.Chrome] =
    Tongue.locale(shown.lay(languages)(_ :: languages))
