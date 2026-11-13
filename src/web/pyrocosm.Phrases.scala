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

// Pyrocosm's own words, in each language its interface is written in (`Tongue.Chrome`): what
// the chrome of a page says, as distinct from the matter an application shows, which is the
// application's to translate. Each is read under the reader's `Locale`, as `Phrases.imports()`.
// A phrase which names something is a whole sentence for each language, never parts joined.
//
// No `Locale` is in scope here, and none should be: `|` on a `Polyglot` picks one language at
// once where it finds one.
object Phrases:
  type Phrase = Polyglot[Text, Tongue.Chrome]

  // The menu bar and the masthead.
  val configuration: Phrase = en(t"Configuration") | fr(t"Configuration") | de(t"Konfiguration")
  val language: Phrase = en(t"Language") | fr(t"Langue") | de(t"Sprache")

  val mode: Phrase =
    en(t"Light, dark or mixed") | fr(t"Clair, sombre ou mixte") | de(t"Hell, dunkel oder gemischt")

  val connecting: Phrase = en(t"connecting") | fr(t"connexion") | de(t"verbinde")

  // What the script says, which the page carries for it (`scripted`).
  val connected: Phrase = en(t"connected") | fr(t"connecté") | de(t"verbunden")
  val reconnecting: Phrase = en(t"reconnecting") | fr(t"reconnexion") | de(t"verbinde erneut")

  val lightMode: Phrase =
    en(t"Light mode (click for dark)") | fr(t"Mode clair (cliquer pour le mode sombre)") |
      de(t"Heller Modus (klicken für dunkel)")

  val darkMode: Phrase =
    en(t"Dark mode (click for mixed)") | fr(t"Mode sombre (cliquer pour le mode mixte)") |
      de(t"Dunkler Modus (klicken für gemischt)")

  val mixedMode: Phrase =
    en(t"Mixed mode (click for light)") | fr(t"Mode mixte (cliquer pour le mode clair)") |
      de(t"Gemischter Modus (klicken für hell)")

  // The script's words, by the names it knows them by: a flat record, so that it is plain
  // JSON for the script.
  case class Scripted(connected: Text, reconnecting: Text, light: Text, dark: Text, mixed: Text)

  def scripted(using Locale[Tongue.Chrome]): Scripted =
    Scripted(connected(), reconnecting(), lightMode(), darkMode(), mixedMode())

  // The configuration page.
  val unconfigurable: Phrase =
    en
      ( t"There is nothing to configure here yet. A tool is configured by its config.tel file: " +
        t".pyrocosm/<tool>/config.tel in a project, or ~/.config/<tool>/config.tel for the " +
        t"user. The language every tool is read in, unless a reader chooses another, is the " +
        t"`language` of ~/.config/pyrocosm/config.tel." ) |
      fr
        ( t"Il n'y a encore rien à configurer ici. Un outil se configure par son fichier " +
          t"config.tel : .pyrocosm/<outil>/config.tel dans un projet, ou " +
          t"~/.config/<outil>/config.tel pour l'utilisateur. La langue dans laquelle chaque " +
          t"outil est lu, à moins qu'un lecteur n'en choisisse une autre, est le `language` " +
          t"de ~/.config/pyrocosm/config.tel." ) |
      de
        ( t"Hier gibt es noch nichts zu konfigurieren. Ein Werkzeug wird durch seine Datei " +
          t"config.tel konfiguriert: .pyrocosm/<werkzeug>/config.tel in einem Projekt oder " +
          t"~/.config/<werkzeug>/config.tel für den Benutzer. Die Sprache, in der jedes " +
          t"Werkzeug gelesen wird, sofern ein Leser keine andere wählt, ist das `language` " +
          t"in ~/.config/pyrocosm/config.tel." )

  val port: Phrase = en(t"port") | fr(t"port") | de(t"Port")
  val theme: Phrase = en(t"theme") | fr(t"thème") | de(t"Design")
  val languageSetting: Phrase = en(t"language") | fr(t"langue") | de(t"Sprache")
  val thisFrontend: Phrase = en(t"This frontend") | fr(t"Cette interface") | de(t"Diese Oberfläche")

  // The reader's purview.
  val imports: Phrase = en(t"Imports") | fr(t"Imports") | de(t"Importe")
  val bookmarks: Phrase = en(t"Bookmarks") | fr(t"Signets") | de(t"Lesezeichen")

  val noImports: Phrase =
    en(t"Nothing is imported: every name is written in full.") |
      fr(t"Rien n'est importé : chaque nom est écrit en entier.") |
      de(t"Nichts ist importiert: Jeder Name wird vollständig ausgeschrieben.")

  val noBookmarks: Phrase =
    en(t"No page is bookmarked.") | fr(t"Aucune page n'a de signet.") |
      de(t"Keine Seite hat ein Lesezeichen.")

  def addImport(label: Text): Phrase =
    en(t"Import $label.*") | fr(t"Importer $label.*") | de(t"$label.* importieren")

  def removeImport(label: Text): Phrase =
    en(t"Remove the import of $label.*") | fr(t"Retirer l'import de $label.*") |
      de(t"Import von $label.* entfernen")

  def addBookmark(label: Text): Phrase =
    en(t"Bookmark $label") | fr(t"Ajouter un signet pour $label") |
      de(t"Lesezeichen für $label setzen")

  def removeBookmark(label: Text): Phrase =
    en(t"Remove the bookmark of $label") | fr(t"Retirer le signet de $label") |
      de(t"Lesezeichen für $label entfernen")

  def remove(label: Text): Phrase =
    en(t"Remove $label") | fr(t"Retirer $label") | de(t"$label entfernen")

  // A diagnostic's severity, and a trace's causes.
  val error: Phrase = en(t"error") | fr(t"erreur") | de(t"Fehler")
  val warning: Phrase = en(t"warning") | fr(t"avertissement") | de(t"Warnung")
  val ok: Phrase = en(t"ok") | fr(t"ok") | de(t"ok")
  val info: Phrase = en(t"info") | fr(t"info") | de(t"Info")
  val note: Phrase = en(t"note") | fr(t"note") | de(t"Hinweis")
  val causedBy: Phrase = en(t"caused by:") | fr(t"causé par :") | de(t"verursacht durch:")

  // The hub's landing page.
  def on(machine: Text): Phrase =
    en(t"Pyrocosm on $machine") | fr(t"Pyrocosm sur $machine") | de(t"Pyrocosm auf $machine")

  val toolsHere: Phrase =
    en(t"Tools on this machine") | fr(t"Outils sur cette machine") |
      de(t"Werkzeuge auf diesem Rechner")

  val toolsElsewhere: Phrase =
    en(t"Tools on other machines") | fr(t"Outils sur d'autres machines") |
      de(t"Werkzeuge auf anderen Rechnern")
