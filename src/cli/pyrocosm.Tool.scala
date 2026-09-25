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

import java.util.concurrent as juc
import scala.collection.concurrent.TrieMap

import soundness.*

import errorDiagnostics.stackTracesDiagnostics
import filesystemBackends.javaBaseFilesystem
import logging.silentLogging
import probates.cancelProbate
import systems.javaBaseSystem

// A command-line tool built on Pyrocosm, as its daemon knows it: the command's name, which is
// also the name of its configuration directories, the prose that opens its manpage, and the
// web front-end it can serve, if it has one. There is one `Tool` per application, and its
// `standard` method gives the application the subcommands every Pyrocosm tool shares —
// `about`, `install`, `quit` and `--version` — and the configuration files every tool reads,
// without touching how the application defines its own subcommands and flags.
//
// Configuration comes from two TEL files: the repository's `.pyrocosm/<name>/config.tel`,
// found by walking up from the invocation's working directory exactly as `.git` is found, so a
// tool can be invoked from anywhere inside a project; and the user's own
// `$XDG_CONFIG_HOME/<name>/config.tel` (`~/.config/<name>/config.tel` by default). The
// repository's file takes priority over the user's. Both feed the `Configurator` cascade an
// exoskeleton `Setting` reads, after its command-line flag, a `<NAME>_`-prefixed environment
// variable and a `<name>.`-prefixed system property.
//
// The SCHEMA of a `config.tel` is the settings themselves: each `Setting`'s camelCase name
// maps to a kebab-case TEL keyword (the same derivation as its `--flag`). Three value rules:
//
//   classpath out/tests.jar     # a keyword with an atom: the setting's value is that atom
//   fail-fast                   # a bare keyword (a TEL flag): reads as `true`
//   classpath out/util.jar      # a REPEATED keyword: the atoms join with ':', so a multi-entry
//                               # classpath is written one entry per line
//
// Two keywords are read by `Tool` itself, for a tool with a `web` front-end: `port` is the
// port it serves on, and a bare `serve` asks for the front-end to be launched when the daemon
// starts, so that it is simply there, for as long as the daemon lives, without a `serve`
// command ever being run.
case class Tool
  ( name: Text, prose: Text, web: Optional[Tool.Web] = Unset, services: List[Tool.Service] = Nil ):

  // Every daemon-lifetime service the tool offers: its web front-end, if any, and the rest.
  def allServices: List[Tool.Service] = web.lay(services) { web => web :: services }

object Tool:
  // A service the daemon runs for as long as it lives, once a configuration asks for it: a bare
  // `keyword` in a config file starts it when the daemon starts, `portKeyword` names the setting
  // giving its port (defaulting to `port`), and `serve` binds and returns only when `stop` is
  // called from another thread. `settings` reads any other setting the service needs, by its
  // camelCase name, through the same cascade a `Setting` uses. The web front-end is one such
  // service (`Web`); a listener for other machines (fume's remote worker) is another.
  trait Service:
    def keyword: Text
    def portKeyword: Text
    def port: Int
    def serve(port: Int, settings: Text => Optional[Text])(using Monitor, Probate): Unit
    def stop(): Unit

  // A web front-end the daemon can serve, started by `serve` on `port`. Pyrocosm's
  // `WebFrontend` is the usual implementation, but this module does not depend on it, so that
  // a tool with no web front-end depends on no web server either.
  trait Web extends Service:
    def keyword: Text = t"serve"
    def portKeyword: Text = t"port"
    def serve(port: Int)(using Monitor, Probate): Unit
    def serve(port: Int, settings: Text => Optional[Text])(using Monitor, Probate): Unit = serve(port)

  // A `Status` must be an `object`, not a `val` (soundness#1811), so that the precise union of
  // an `execute` block's result type documents it in the manpage's EXIT STATUS section.
  object InstallFailed extends Status(8, t"the tab-completions or manpage could not be installed")

  // The standard subcommands and flags, in an object so that `Install` does not shadow
  // `exoskeleton.Install`, the error type, for the rest of this one.
  object ui:
    val About = Subcommand("about", "show this tool's name, version and daemon")
    val Install = Subcommand("install", "install shell tab-completions and the manpage")
    val Quit = Subcommand("quit", "stop the background daemon, and any web front-end it serves")
    val Version = Flag[Unit]("version", false, List('v'), "show the version")
    val Force = Flag[Unit]("force", false, List('f'), "overwrite an installed manpage")

  private case class Cached(modified: Long, size: Long, config: Optional[Tel])

  // Because a tool runs as a daemon, parsed configurations are cached across invocations,
  // keyed by the file's absolute path, and invalidated whenever its modification time or size
  // changes — so an edit is honoured by the very next command with no daemon restart. A file
  // that fails to parse is treated (and cached) as absent rather than aborting the command; no
  // tool requires a configuration file to exist.
  private val cache: TrieMap[Text, Cached] = TrieMap()

  // The services launched in this daemon, by tool name and keyword: at most one of each per
  // tool, for as long as the daemon lives, or until `quit`. The instances are pure, so the map
  // holds them without laundering.
  private val serving: juc.ConcurrentHashMap[Text, Service] = juc.ConcurrentHashMap()

  // Reads and parses the file in two separately-scoped `safely` regions — one `Tactic` for the
  // filesystem read, another for the TEL parse — rather than one region with a union `Tactic`:
  // a single tactic would be captured by both the path-reader given and the TEL aggregator,
  // which separation checking rejects as overlapping hidden capabilities.
  private def parse(file: Path on Linux): Optional[Tel] =
    safely(file.read[Data]).let { data => safely(data.read[Tel]) }

  // The parsed document at `file`, freshly stat-checked on every call: one `stat` yields both
  // the modification time and the size, so the change check costs a single filesystem
  // operation per invocation. `Unset` if the file does not exist.
  private def document(file: Path on Linux): Optional[Tel] =
    safely(summon[FilesystemBackend on Linux].stat(file, true)).let: stat =>
      val key: Text = file.encode

      def reload(): Optional[Tel] =
        val parsed: Optional[Tel] = parse(file)
        cache(key) = Cached(stat.modified, stat.size, parsed)
        parsed

      val cached: Optional[Cached] = cache.getOrElse(key, Unset)

      cached match
        case cached: Cached if cached.modified == stat.modified && cached.size == stat.size =>
          cached.config

        case _ =>
          reload()

  // A configuration source for the `Setting` cascade over one document: keyed by the setting's
  // canonical camelCase name, translated to its kebab-case TEL keyword, and applying the three
  // value rules above. The document is already resolved, so the configurator captures nothing.
  private def configurator(document: Optional[Tel]): Configurator =
    name =>
      document.let: tel =>
        val matches: List[Tel] = tel.fields(name.uncamel.kebab).to[List]

        if matches.nil then Unset else
          val atoms = matches.map(_.primaryAtom).filter(_ != t"")
          if atoms.nil then t"true" else atoms.join(t":")

  // Reads (and so registers) a flag, and says whether it is present. NOT inline, and not
  // written at its use in `standard`: a `Flag`'s `apply()` is a transparent inline whose result
  // narrows to a `Prospective` only once it is expanded, and inside an inline method it is not
  // expanded when `.present` is resolved — which then finds `Optional`'s `present` on the
  // declared union, true for every handle, so every flag would read as present. Nor named
  // `present`, which an inline body resolves to that same extension applied as a function.
  private def flagPresent(flag: Flag of Unit)(using Cli, Interpreter): Boolean = flag().present

  private def uptime(startTime: Long): Text =
    val seconds: Long = (java.lang.System.currentTimeMillis() - startTime).max(0L)/1000
    val hours: Long = seconds/3600
    val minutes: Long = seconds%3600/60
    t"${hours.show}h ${minutes.show}m ${(seconds%60).show}s"

  extension (tool: Tool)
    // The version this build of the tool was published as, from the `META-INF/pyrocosm/<name>/
    // version` resource its build writes (see the tool's build.mill): a release's `X.Y.Z`, a
    // snapshot's `X.Y.Z-<hex>`, or a development build's tree hash. Read through the thread's
    // context classloader, which is the loader that sees the tool's own jar under Burdock.
    // `unknown` if no build wrote one.
    def version: Text =
      val resource: String = t"META-INF/pyrocosm/${tool.name}/version".s

      val loader: ClassLoader =
        Optional(Thread.currentThread.nn.getContextClassLoader).or(classOf[Tool].getClassLoader.nn)

      Optional(loader.getResourceAsStream(resource)).let: stream =>
        try String(stream.readAllBytes(), "UTF-8").trim.nn.tt finally stream.close()
      . or(t"unknown")

    // The nearest `.pyrocosm/<name>/config.tel` at or above `directory`, or `Unset` if no ancestor
    // has one. The FILE is what is sought: neither a `.pyrocosm` holding only other tools'
    // directories, nor a `.pyrocosm/<name>` without a `config.tel` (holding only state, say),
    // ends the search.
    def repoFile(directory: Text): Optional[Path on Linux] =
      safely:
        def recur(dir: Path on Linux): Optional[Path on Linux] =
          val candidate =
            dir / Name[Linux](t".pyrocosm") / Name[Linux](tool.name) / Name[Linux](t"config.tel")

          if candidate.existent() then candidate else dir.parent.let(recur(_))

        recur(directory.as[Path on Linux])

    // `$XDG_CONFIG_HOME/<name>/config.tel`, from the invocation's environment, whether or not it
    // exists.
    def userFile(using Environment): Optional[Path on Linux] =
      safely:
        Directories.configHome[Path on Linux] / Name[Linux](tool.name) / Name[Linux](t"config.tel")

    def repoConfig(directory: Text): Optional[Tel] = tool.repoFile(directory).let(Tool.document(_))
    def userConfig(using Environment): Optional[Tel] = tool.userFile.let(Tool.document(_))

    // The configuration files as one source for the `Setting` cascade, the repository's file
    // taking priority over the user's. The caller composes the properties and the environment
    // ahead of it (`standard` does so).
    def configurator(directory: Text)(using Environment): Configurator =
      Tool.configurator(tool.repoConfig(directory)) ++ Tool.configurator(tool.userConfig)

    // Wraps an application's dispatch with the standard subcommands. `dispatch` is the
    // application's own `arguments match`, run when none of the standard subcommands applies; it
    // receives the full `Configurator` cascade for its `Setting's: the flag, then the `<name>.*`
    // system property, the `<NAME>_*` environment variable, the repository's `config.tel` —
    // resolved from the INVOCATION's working directory (each daemon client has its own), never
    // the daemon process's — and the user's.
    //
    // Matching a `Subcommand` also SUGGESTS it, so `about`, `install` and `quit` are offered by
    // tab-completion alongside the application's own subcommands, and the help tree behind the
    // manpage descends into them. `--version` is offered for `<name> -<TAB>` without being
    // attached to every subcommand. An application with a flag-first arm of its own still sees
    // its invocation, since only a present `--version` is consumed here.
    // `inline`, so that `dispatch` is expanded in place: as a closure it would capture the
    // `Cli` and `DaemonService` that are also passed here, which separation checking (on in
    // the tools' builds) rejects. The flags are read through `present`, below, and never here.
    inline def standard(inline dispatch: Configurator ?=> Execution)
       (using cli: Cli, service: DaemonService[?], monitor: Monitor, environment: Environment)
       (using Interpreter)
    :   Execution =

      val directory: Text = cli.workingDirectory.directory()
      given prefix: Configurator.Prefix = Configurator.Prefix(tool.name)

      given configurator: Configurator =
        Configurator.properties ++ Configurator.environment ++ tool.configurator(directory)

      tool.launch()

      // `--version` is read only when the first argument is a flag, so that it is registered
      // where a flag can stand without being attached to every subcommand; it is read
      // unconditionally there, because reading is what registers it. The subcommands are matched
      // only when the head is NOT a flag: matching a `Subcommand` suggests it, and a suggestion at
      // the cursor takes precedence over the flag list, so trying them at `<name> --ver<TAB>`
      // would hide the flags. An empty head (the word being completed, or the help tree's probe
      // of the root) registers the flag too, for the manpage, but is never a present flag.
      arguments match
        case Argument(head) :: _ if head.starts(t"-") =>
          val version: Boolean = Tool.flagPresent(Tool.ui.Version)
          if version then execute(tool.showVersion()) else dispatch

        case Argument(head) :: rest =>
          if head == t"" then Tool.ui.Version()

          arguments match
            case Tool.ui.About() :: _ =>
              execute(tool.about(directory))

            case Tool.ui.Install() :: _ =>
              val force: Boolean = Tool.flagPresent(Tool.ui.Force)
              execute(tool.install(force))

            case Tool.ui.Quit() :: _ =>
              execute(tool.quit())

            case _ =>
              dispatch

        case _ =>
          dispatch

    // Launches every service the configuration asks for (`serve`, `listen`, …), once per
    // daemon. Runs on every invocation, since the daemon starts with the first of them, but
    // only for a real invocation: never for a tab-completion or the help tree's probe. The
    // task runs under the daemon's own monitor, which the `cli` block supplies, so it outlives
    // the client that happened to start it.
    private def launch()(using cli: Cli, monitor: Monitor, configurator: Configurator): Unit =
      cli match
        case _: Invocation =>
          tool.allServices.each: (service: Service) =>
            val wanted: Boolean = configurator.read(service.keyword).present
            val key: Text = Text(s"${tool.name.s}/${service.keyword.s}")

            if wanted && Tool.serving.putIfAbsent(key, service) == null
            then
              val port: Int =
                configurator.read(service.portKeyword).let { text => safely(text.as[Int]) }
                . or(service.port)

              val settings: Text => Optional[Text] = configurator.read(_)
              async(service.serve(port, settings))

        case _ =>
          ()

    // Runs `service` interactively from an invocation (`<tool> serve`, `<tool> listen`), unless
    // the daemon already runs it, in which case nothing more is needed; returns when it stops.
    def run(service: Service, port: Int)
       (using monitor: Monitor, probate: Probate, configurator: Configurator)
    :   Unit =

      val key: Text = Text(s"${tool.name.s}/${service.keyword.s}")
      val settings: Text => Optional[Text] = configurator.read(_)

      if Tool.serving.putIfAbsent(key, service) == null then
        try service.serve(port, settings) finally Tool.serving.remove(key)

    private def showVersion()(using invocation: Invocation): Exit =
      given Stdio = invocation.stdio
      Out.println(tool.version)
      Exit.Ok

    private def about(directory: Text)
       (using invocation: Invocation, service: DaemonService[?], environment: Environment)
    :   Exit =

      given Stdio = invocation.stdio

      val repo: Text = tool.repoFile(directory).let(_.encode).or(t"none at or above $directory")

      val user: Text = tool.userFile.let: file =>
        if file.existent() then file.encode else t"${file.encode} (absent)"
      . or(t"unknown")

      Out.println(t"${tool.name} ${tool.version}")
      Out.println(t"")
      Out.println(t"executable   ${service.executable.encode}")
      Out.println(t"daemon pid   ${java.lang.ProcessHandle.current.nn.pid.show}")
      Out.println(t"uptime       ${Tool.uptime(service.startTime)}")
      Out.println(t"repo config  $repo")
      Out.println(t"user config  $user")
      Exit.Ok

    // Installs the tool's shell tab-completions and its manpage. `Completions.ensure` writes the
    // zsh/bash/fish completion script (the same call the built-in `{admin} install` uses); it
    // needs an `Entrypoint`, which the ambient Ethereal `DaemonService` supplies (it extends
    // `Entrypoint`). The manpage's structure comes from `service.help()` — the same subcommand
    // and flag tree the completions register, discovered by re-running the dispatch in completion
    // mode — so `man <name>` can never disagree with the CLI, and the EXIT STATUS section is
    // populated from the `Status` unions of the `execute` blocks. `force = true` for the
    // completions installs even when the tool is not yet on the `PATH`, so a freshly-built binary
    // can set itself up before being installed as a command.
    private def install(force: Boolean)
       (using invocation: Invocation, service: DaemonService[?])
       (using erased Effectful)
    :   Tool.InstallFailed.type | Exit =

      given Stdio = invocation.stdio

      // The `DaemonService` extends `Entrypoint`, and `Completions.ensure` accepts a TRACKED
      // `Entrypoint^`, so the service is passed on with its capture intact, without laundering.
      given entrypoint: (Entrypoint^{service}) = service
      given manual: Manual = Manual(prose = tool.prose, version = safely(tool.version.as[Semver]))

      recover:
        // `exoskeleton.Install`, qualified: the bare name `Install` is the subcommand in `Tool.ui`.
        case error: exoskeleton.Install.Error =>
          Out.println(t"Could not install the tab-completions or manpage")
          Tool.InstallFailed

      . protect:
          Completions.ensure(force = true).each(Out.println(_))

          Manpages.install(service.help().roff, force) match
            case Manpages.InstallResult.Installed(path) =>
              Out.println(t"Installed the manpage to $path")

            case Manpages.InstallResult.AlreadyInstalled(path) =>
              Out.println(t"A manpage is already installed at $path; use --force to overwrite it")

            case Manpages.InstallResult.NoWritableLocation =>
              Out.println(t"No writable location was found for the manpage")

          Exit.Ok

    // Stops the front-end this daemon serves, if any, then the daemon itself. The shutdown is
    // deferred until this invocation's exit status has been delivered, so the client returns
    // cleanly.
    private def quit()(using invocation: Invocation, service: DaemonService[?]): Exit =
      given Stdio = invocation.stdio

      tool.allServices.each: (service: Service) =>
        val key: Text = Text(s"${tool.name.s}/${service.keyword.s}")
        Optional(Tool.serving.remove(key)).let(_.stop())

      Out.println(t"${tool.name}: stopping the daemon")
      service.shutdown()
      Exit.Ok
