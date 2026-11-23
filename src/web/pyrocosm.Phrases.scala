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
  import Tongue.{it, pt, ru, zh, ja}

  type Phrase = Polyglot[Text, Tongue.Chrome]

  // The menu bar and the masthead.
  val configuration: Phrase =
    en(t"Configuration") | fr(t"Configuration") | de(t"Konfiguration") | es(t"Configuración") |
      it(t"Configurazione") | pt(t"Configuração") | pl(t"Konfiguracja") | ru(t"Конфигурация") |
      zh(t"配置") | ja(t"設定")

  val language: Phrase =
    en(t"Language") | fr(t"Langue") | de(t"Sprache") | es(t"Idioma") | it(t"Lingua") |
      pt(t"Idioma") | pl(t"Język") | ru(t"Язык") | zh(t"语言") | ja(t"言語")

  val mode: Phrase =
    en(t"Light, dark or mixed") | fr(t"Clair, sombre ou mixte") |
      de(t"Hell, dunkel oder gemischt") | es(t"Claro, oscuro o mixto") |
      it(t"Chiaro, scuro o misto") | pt(t"Claro, escuro ou misto") |
      pl(t"Jasny, ciemny lub mieszany") | ru(t"Светлый, тёмный или смешанный") |
      zh(t"浅色、深色或混合") | ja(t"ライト、ダーク、ミックス")

  val connecting: Phrase =
    en(t"connecting") | fr(t"connexion") | de(t"verbinde") | es(t"conectando") |
      it(t"connessione") | pt(t"conectando") | pl(t"łączenie") | ru(t"подключение") |
      zh(t"正在连接") | ja(t"接続中")

  // What the script says, which the page carries for it (`scripted`).
  val connected: Phrase =
    en(t"connected") | fr(t"connecté") | de(t"verbunden") | es(t"conectado") | it(t"connesso") |
      pt(t"conectado") | pl(t"połączono") | ru(t"подключено") | zh(t"已连接") | ja(t"接続済み")

  val reconnecting: Phrase =
    en(t"reconnecting") | fr(t"reconnexion") | de(t"verbinde erneut") | es(t"reconectando") |
      it(t"riconnessione") | pt(t"reconectando") | pl(t"ponowne łączenie") |
      ru(t"переподключение") | zh(t"正在重新连接") | ja(t"再接続中")

  val lightMode: Phrase =
    en(t"Light mode (click for dark)") | fr(t"Mode clair (cliquer pour le mode sombre)") |
      de(t"Heller Modus (klicken für dunkel)") | es(t"Modo claro (clic para el oscuro)") |
      it(t"Modalità chiara (clic per la scura)") | pt(t"Modo claro (clique para o escuro)") |
      pl(t"Tryb jasny (kliknij, aby włączyć ciemny)") |
      ru(t"Светлый режим (нажмите для тёмного)") | zh(t"浅色模式（点击切换到深色）") |
      ja(t"ライトモード（クリックでダーク）")

  val darkMode: Phrase =
    en(t"Dark mode (click for mixed)") | fr(t"Mode sombre (cliquer pour le mode mixte)") |
      de(t"Dunkler Modus (klicken für gemischt)") | es(t"Modo oscuro (clic para el mixto)") |
      it(t"Modalità scura (clic per la mista)") | pt(t"Modo escuro (clique para o misto)") |
      pl(t"Tryb ciemny (kliknij, aby włączyć mieszany)") |
      ru(t"Тёмный режим (нажмите для смешанного)") | zh(t"深色模式（点击切换到混合）") |
      ja(t"ダークモード（クリックでミックス）")

  val mixedMode: Phrase =
    en(t"Mixed mode (click for light)") | fr(t"Mode mixte (cliquer pour le mode clair)") |
      de(t"Gemischter Modus (klicken für hell)") | es(t"Modo mixto (clic para el claro)") |
      it(t"Modalità mista (clic per la chiara)") | pt(t"Modo misto (clique para o claro)") |
      pl(t"Tryb mieszany (kliknij, aby włączyć jasny)") |
      ru(t"Смешанный режим (нажмите для светлого)") | zh(t"混合模式（点击切换到浅色）") |
      ja(t"ミックスモード（クリックでライト）")

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
          t"in ~/.config/pyrocosm/config.tel." ) |
      es
        ( t"Todavía no hay nada que configurar aquí. Una herramienta se configura con su " +
          t"archivo config.tel: .pyrocosm/<herramienta>/config.tel en un proyecto, o " +
          t"~/.config/<herramienta>/config.tel para el usuario. El idioma en el que se lee " +
          t"cada herramienta, salvo que un lector elija otro, es el `language` de " +
          t"~/.config/pyrocosm/config.tel." ) |
      it
        ( t"Qui non c'è ancora nulla da configurare. Uno strumento si configura con il suo " +
          t"file config.tel: .pyrocosm/<strumento>/config.tel in un progetto, oppure " +
          t"~/.config/<strumento>/config.tel per l'utente. La lingua in cui ogni strumento " +
          t"viene letto, a meno che un lettore non ne scelga un'altra, è il `language` di " +
          t"~/.config/pyrocosm/config.tel." ) |
      pt
        ( t"Ainda não há nada para configurar aqui. Uma ferramenta é configurada pelo seu " +
          t"arquivo config.tel: .pyrocosm/<ferramenta>/config.tel em um projeto, ou " +
          t"~/.config/<ferramenta>/config.tel para o usuário. O idioma em que cada ferramenta " +
          t"é lida, a menos que um leitor escolha outro, é o `language` de " +
          t"~/.config/pyrocosm/config.tel." ) |
      pl
        ( t"Nie ma tu jeszcze nic do skonfigurowania. Narzędzie konfiguruje się w jego pliku " +
          t"config.tel: .pyrocosm/<narzędzie>/config.tel w projekcie albo " +
          t"~/.config/<narzędzie>/config.tel dla użytkownika. Język, w którym czytane jest " +
          t"każde narzędzie, o ile czytelnik nie wybierze innego, to `language` w " +
          t"~/.config/pyrocosm/config.tel." ) |
      ru
        ( t"Здесь пока нечего настраивать. Инструмент настраивается своим файлом config.tel: " +
          t".pyrocosm/<инструмент>/config.tel в проекте или ~/.config/<инструмент>/config.tel " +
          t"для пользователя. Язык, на котором читается каждый инструмент, если читатель не " +
          t"выберет другой, задаётся параметром `language` в ~/.config/pyrocosm/config.tel." ) |
      zh
        ( t"这里暂时没有可配置的内容。工具通过其 config.tel 文件配置：项目中的 " +
          t".pyrocosm/<工具>/config.tel，或用户的 ~/.config/<工具>/config.tel。" +
          t"除非读者另行选择，每个工具所用的语言由 ~/.config/pyrocosm/config.tel 中的 " +
          t"`language` 决定。" ) |
      ja
        ( t"ここで設定できるものはまだありません。ツールは config.tel ファイルで設定します。" +
          t"プロジェクトでは .pyrocosm/<ツール>/config.tel、ユーザーには " +
          t"~/.config/<ツール>/config.tel です。読み手が別の言語を選ばないかぎり、" +
          t"各ツールを表示する言語は ~/.config/pyrocosm/config.tel の `language` です。" )

  val port: Phrase =
    en(t"port") | fr(t"port") | de(t"Port") | es(t"puerto") | it(t"porta") | pt(t"porta") |
      pl(t"port") | ru(t"порт") | zh(t"端口") | ja(t"ポート")

  val theme: Phrase =
    en(t"theme") | fr(t"thème") | de(t"Design") | es(t"tema") | it(t"tema") | pt(t"tema") |
      pl(t"motyw") | ru(t"тема") | zh(t"主题") | ja(t"テーマ")

  val languageSetting: Phrase =
    en(t"language") | fr(t"langue") | de(t"Sprache") | es(t"idioma") | it(t"lingua") |
      pt(t"idioma") | pl(t"język") | ru(t"язык") | zh(t"语言") | ja(t"言語")

  val thisFrontend: Phrase =
    en(t"This frontend") | fr(t"Cette interface") | de(t"Diese Oberfläche") |
      es(t"Esta interfaz") | it(t"Questa interfaccia") | pt(t"Esta interface") |
      pl(t"Ten interfejs") | ru(t"Этот интерфейс") | zh(t"此界面") | ja(t"このフロントエンド")

  // The reader's purview.
  val imports: Phrase =
    en(t"Imports") | fr(t"Imports") | de(t"Importe") | es(t"Importaciones") |
      it(t"Importazioni") | pt(t"Importações") | pl(t"Importy") | ru(t"Импорты") | zh(t"导入") |
      ja(t"インポート")

  val bookmarks: Phrase =
    en(t"Bookmarks") | fr(t"Signets") | de(t"Lesezeichen") | es(t"Marcadores") |
      it(t"Segnalibri") | pt(t"Favoritos") | pl(t"Zakładki") | ru(t"Закладки") | zh(t"书签") |
      ja(t"ブックマーク")

  val noImports: Phrase =
    en(t"Nothing is imported: every name is written in full.") |
      fr(t"Rien n'est importé : chaque nom est écrit en entier.") |
      de(t"Nichts ist importiert: Jeder Name wird vollständig ausgeschrieben.") |
      es(t"No se importa nada: cada nombre se escribe completo.") |
      it(t"Nulla è importato: ogni nome è scritto per intero.") |
      pt(t"Nada é importado: cada nome é escrito por extenso.") |
      pl(t"Nic nie jest zaimportowane: każda nazwa jest zapisana w całości.") |
      ru(t"Ничего не импортировано: каждое имя записано полностью.") |
      zh(t"未导入任何内容：每个名称都完整写出。") |
      ja(t"何もインポートされていません。すべての名前は完全な形で書かれます。")

  val noBookmarks: Phrase =
    en(t"No page is bookmarked.") | fr(t"Aucune page n'a de signet.") |
      de(t"Keine Seite hat ein Lesezeichen.") | es(t"Ninguna página tiene marcador.") |
      it(t"Nessuna pagina ha un segnalibro.") | pt(t"Nenhuma página está nos favoritos.") |
      pl(t"Żadna strona nie ma zakładki.") | ru(t"Ни одна страница не добавлена в закладки.") |
      zh(t"没有页面加入书签。") | ja(t"ブックマークされたページはありません。")

  def addImport(label: Text): Phrase =
    en(t"Import $label.*") | fr(t"Importer $label.*") | de(t"$label.* importieren") |
      es(t"Importar $label.*") | it(t"Importa $label.*") | pt(t"Importar $label.*") |
      pl(t"Importuj $label.*") | ru(t"Импортировать $label.*") | zh(t"导入 $label.*") |
      ja(t"$label.* をインポート")

  def removeImport(label: Text): Phrase =
    en(t"Remove the import of $label.*") | fr(t"Retirer l'import de $label.*") |
      de(t"Import von $label.* entfernen") | es(t"Quitar la importación de $label.*") |
      it(t"Rimuovi l'importazione di $label.*") | pt(t"Remover a importação de $label.*") |
      pl(t"Usuń import $label.*") | ru(t"Убрать импорт $label.*") |
      zh(t"移除对 $label.* 的导入") | ja(t"$label.* のインポートを削除")

  def addBookmark(label: Text): Phrase =
    en(t"Bookmark $label") | fr(t"Ajouter un signet pour $label") |
      de(t"Lesezeichen für $label setzen") | es(t"Añadir un marcador para $label") |
      it(t"Aggiungi un segnalibro per $label") | pt(t"Adicionar $label aos favoritos") |
      pl(t"Dodaj $label do zakładek") | ru(t"Добавить $label в закладки") |
      zh(t"为 $label 添加书签") | ja(t"$label をブックマーク")

  def removeBookmark(label: Text): Phrase =
    en(t"Remove the bookmark of $label") | fr(t"Retirer le signet de $label") |
      de(t"Lesezeichen für $label entfernen") | es(t"Quitar el marcador de $label") |
      it(t"Rimuovi il segnalibro di $label") | pt(t"Remover $label dos favoritos") |
      pl(t"Usuń $label z zakładek") | ru(t"Убрать $label из закладок") |
      zh(t"移除 $label 的书签") | ja(t"$label のブックマークを削除")

  def remove(label: Text): Phrase =
    en(t"Remove $label") | fr(t"Retirer $label") | de(t"$label entfernen") | es(t"Quitar $label") |
      it(t"Rimuovi $label") | pt(t"Remover $label") | pl(t"Usuń $label") | ru(t"Убрать $label") |
      zh(t"移除 $label") | ja(t"$label を削除")

  // A diagnostic's severity, and a trace's causes.
  val error: Phrase =
    en(t"error") | fr(t"erreur") | de(t"Fehler") | es(t"error") | it(t"errore") | pt(t"erro") |
      pl(t"błąd") | ru(t"ошибка") | zh(t"错误") | ja(t"エラー")

  val warning: Phrase =
    en(t"warning") | fr(t"avertissement") | de(t"Warnung") | es(t"advertencia") | it(t"avviso") |
      pt(t"aviso") | pl(t"ostrzeżenie") | ru(t"предупреждение") | zh(t"警告") | ja(t"警告")

  val ok: Phrase =
    en(t"ok") | fr(t"ok") | de(t"ok") | es(t"ok") | it(t"ok") | pt(t"ok") | pl(t"ok") |
      ru(t"ok") | zh(t"ok") | ja(t"ok")

  val info: Phrase =
    en(t"info") | fr(t"info") | de(t"Info") | es(t"info") | it(t"info") | pt(t"info") |
      pl(t"info") | ru(t"инфо") | zh(t"信息") | ja(t"情報")

  val note: Phrase =
    en(t"note") | fr(t"note") | de(t"Hinweis") | es(t"nota") | it(t"nota") | pt(t"nota") |
      pl(t"uwaga") | ru(t"примечание") | zh(t"备注") | ja(t"注記")

  val causedBy: Phrase =
    en(t"caused by:") | fr(t"causé par :") | de(t"verursacht durch:") | es(t"causado por:") |
      it(t"causato da:") | pt(t"causado por:") | pl(t"spowodowane przez:") | ru(t"причина:") |
      zh(t"原因：") | ja(t"原因：")

  // What stands for the name of a frame which was inlined, and has none of its own.
  val inlinedFrom: Phrase =
    en(t"inlined from") | fr(t"inliné depuis") | de(t"inline aus") | es(t"insertado desde") |
      it(t"espanso inline da") | pt(t"inserido a partir de") | pl(t"wstawione z") |
      ru(t"встроено из") | zh(t"内联自") | ja(t"インライン展開元")

  // The hub's landing page.
  def on(machine: Text): Phrase =
    en(t"Pyrocosm on $machine") | fr(t"Pyrocosm sur $machine") | de(t"Pyrocosm auf $machine") |
      es(t"Pyrocosm en $machine") | it(t"Pyrocosm su $machine") | pt(t"Pyrocosm em $machine") |
      pl(t"Pyrocosm na $machine") | ru(t"Pyrocosm на $machine") | zh(t"$machine 上的 Pyrocosm") |
      ja(t"$machine の Pyrocosm")

  val toolsHere: Phrase =
    en(t"Tools on this machine") | fr(t"Outils sur cette machine") |
      de(t"Werkzeuge auf diesem Rechner") | es(t"Herramientas en esta máquina") |
      it(t"Strumenti su questa macchina") | pt(t"Ferramentas nesta máquina") |
      pl(t"Narzędzia na tym komputerze") | ru(t"Инструменты на этом компьютере") |
      zh(t"本机上的工具") | ja(t"このマシンのツール")

  val toolsElsewhere: Phrase =
    en(t"Tools on other machines") | fr(t"Outils sur d'autres machines") |
      de(t"Werkzeuge auf anderen Rechnern") | es(t"Herramientas en otras máquinas") |
      it(t"Strumenti su altre macchine") | pt(t"Ferramentas em outras máquinas") |
      pl(t"Narzędzia na innych komputerach") | ru(t"Инструменты на других компьютерах") |
      zh(t"其他机器上的工具") | ja(t"他のマシンのツール")
