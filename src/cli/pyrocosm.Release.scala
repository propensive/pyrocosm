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

import alphabets.hexLowerCase
import charsets.utf8Charset
import httpBackends.javaNetHttp
import internetAccess.online
import logging.silentLogging
import providers.javaBaseProvider

object Release:
  case class Executable(url: Text, sha256: Text)

  // The newest non-prerelease release's manifest. GitHub's `latest` redirect needs no API call,
  // and so is not rate-limited; snapshots are pre-releases, and are never `latest`.
  def manifestUrl(name: Text): Text =
    t"https://github.com/propensive/$name/releases/latest/download/upgrade.tsv"

  // Parses a manifest: tab-separated rows of `version`, `build`, `signed-by` (which may be
  // empty) and `<platform> <url> <sha256>`; blank lines and `#` comments are skipped. `Unset`
  // if the version or build is missing, or the build is not a number.
  def parse(text: Text): Optional[Release] =
    val rows: List[List[Text]] =
      text.cut(t"\n").map(_.trim).filter: line =>
        line != t"" && !line.starts(t"#")
      . map(_.cut(t"\t"))

    def field(key: Text): Optional[Text] =
      rows.filter(_.prim == key).prim.let:
        case _ :: value :: _ => value
        case _               => t""

    val executables: Map[Text, Executable] =
      rows.bind:
        case List(platform, url, sha256) if platform.contains(t"-") =>
          List(platform -> Executable(url, sha256))

        case _ =>
          Nil

      . to[Map]

    field(t"version").let: version =>
      field(t"build").let { build => safely(build.as[Long]) }.let: build =>
        val signedBy: Optional[Text] =
          field(t"signed-by").let { key => if key == t"" then Unset else key }
        Release(version, build, signedBy, executables)

  // Fetches and parses the manifest of `name`'s newest release; `Unset` if it cannot be fetched
  // or read, for any reason: a check is advisory, and never a failure of the command it runs in.
  def fetch(name: Text): Optional[Release] =
    safely(manifestUrl(name).as[HttpUrl].fetch().receive[Text]).let(parse(_))

  // Downloads `executable` and checks its digest; `Unset` if the download fails or the bytes do
  // not have the SHA-256 the manifest promised.
  def download(executable: Executable): Optional[Data] =
    safely(executable.url.as[HttpUrl].fetch().receive[Data]).let: data =>
      if data.digest[Sha2[256]].serialize[Hex] == executable.sha256.lower then data else Unset

  // The platform label of the running JVM, as the release names its executables: `linux-x64`,
  // `linux-arm64`, `macos-x64`, `macos-arm64` or `windows-x64`; `Unset` for anything else.
  def platform(using System): Optional[Text] =
    val os: Text = safely(System.properties.os.name[Text]()).or(t"").lower
    val arch: Text = safely(System.properties.os.arch[Text]()).or(t"").lower

    val system: Optional[Text] =
      if os.contains(t"mac") || os.contains(t"darwin") then t"macos"
      else if os.contains(t"win") then t"windows"
      else if os.contains(t"linux") then t"linux"
      else Unset

    val architecture: Optional[Text] =
      if arch == t"x86_64" || arch == t"amd64" then t"x64"
      else if arch == t"aarch64" || arch == t"arm64" then t"arm64"
      else Unset

    system.let: system =>
      architecture.let { architecture => t"$system-$architecture" }

// A published release of a tool, as its `upgrade.tsv` manifest describes it (see
// propensive/.github#32): the version, the build id its executables carry — the number the
// launcher orders upgrades by — the key they were signed with, if any, and one executable per
// platform, with the SHA-256 the download is checked against. The manifest is not signed and
// need not be: the executable it names is verified by the launcher against the key in the
// RUNNING binary, so a forged manifest can make an upgrade fail but never make a bad one apply.
case class Release
  ( version:     Text,
    build:       Long,
    signedBy:    Optional[Text],
    executables: Map[Text, Release.Executable] ):

  def executable(platform: Text): Optional[Release.Executable] =
    executables.at(platform)
