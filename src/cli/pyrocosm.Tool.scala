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

import charsets.utf8Charset
import codepages.utf8Codepage
import errorDiagnostics.stackTracesDiagnostics
import filesystemBackends.javaBaseFilesystem
import httpBackends.javaNetHttp
import internetAccess.online
import logging.silentLogging
import probates.cancelProbate
import systems.javaBaseSystem
import textSanitizers.skipSanitizer

// A command-line tool built on Pyrocosm, as its daemon knows it: the command's name, which is
// also the name of its configuration directories, the prose that opens its manpage, and the
// web front-end it can serve, if it has one. There is one `Tool` per application, and its
// `standard` method gives the application the subcommands every Pyrocosm tool shares —
// `about`, `install`, `upgrade`, `quit` and `--version` — and the configuration files every tool
// reads, without touching how the application defines its own subcommands and flags.
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
// command ever being run. A third, `no-upgrade-check`, turns off the daily check for a newer
// release (below).
//
// UPGRADES. Once a day, in the background of the daemon, a released tool fetches the manifest of
// its newest release (`Release`). When that is newer than the running build, the `upgrade`
// subcommand is offered by tab-completion; it is accepted at any time, and fails with
// `NoUpgrade` when there is nothing newer. `upgrade` downloads the executable for this platform,
// checks its digest, and stages it for the launcher (`Upgrade.stage`), which verifies its
// signature against the key in the RUNNING executable and swaps it in at the start of the next
// invocation — so the daemon never replaces itself, and the launcher's verdict is read, reported
// once, and acknowledged by the next invocation (`report`). A development build, whose version
// is not a release's, never checks.
case class Tool
  ( name: Text, prose: Text, web: Optional[Tool.Web] = Unset, services: List[Tool.Service] = Nil ):

  // Every daemon-lifetime service the tool offers: its web front-end, if any, the rest, and the
  // swarm's own two, which every tool has.
  def allServices: List[Tool.Service] =
    web.lay(services) { web => web :: services } + Tool.swarmServices(this)

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
  object NoUpgrade extends Status(9, t"no newer release is available")
  object UpgradeFailed extends Status(10, t"the newer release could not be downloaded or staged")
  object UsageError extends Status(2, t"the command line was not understood")
  object NoMachine extends Status(11, t"no machine of that name is configured")
  object RemoteFailed extends Status(12, t"the connection to another machine could not be made")

  // The standard subcommands and flags, in an object so that `Install` does not shadow
  // `exoskeleton.Install`, the error type, for the rest of this one.
  object ui:
    val About = Subcommand("about", "show this tool's name, version and daemon")
    val Install = Subcommand("install", "install shell tab-completions and the manpage")
    val Quit = Subcommand("quit", "stop the background daemon, and any web front-end it serves")

    // `upgrade` is always accepted, but SUGGESTED only while a newer release is known: the
    // hidden twin is what `standard` matches against otherwise (`upgradeCommand`).
    val Upgrade = Subcommand("upgrade", "replace this executable with the newest release")
    val HiddenUpgrade =
      Subcommand("upgrade", "replace this executable with the newest release", hidden = true)
    val Version = Flag[Unit]("version", false, List('v'), "show the version")
    val Force = Flag[Unit]("force", false, List('f'), "overwrite an installed manpage")

    // The swarm every tool's daemon is a node of (`Swarm`), and the subcommands of `swarm`.
    val Swarm = Subcommand("swarm", "see the other Pyrocosm tools running, here and on other machines")
    val Invite = Subcommand("invite", "invite another machine to join this one's swarm, in one word")
    val Join = Subcommand("join", "accept another machine's invitation, and stay linked to it")
    val Listen = Subcommand("listen", "accept other machines, as this machine's gateway")
    val Connect = Subcommand("connect", "keep a link to a configured machine open")
    val Ping = Subcommand("ping", "send a message to a configured machine and await its answer")
    val Identity = Subcommand("identity", "show this machine's identity, for another to declare")
    val Peers = Subcommand("peers", "list the machines this one has admitted by invitation")
    val Revoke = Subcommand("revoke", "refuse a machine this one admitted by invitation")
    val Log = Subcommand("log", "show what this daemon's node has been doing")

    val Disconnect =
      Subcommand("disconnect", "stop keeping a link to a machine, or stop accepting other machines")

    // How long an invitation may be accepted for: `--expires 30m`, `2h`, `1d`; an hour unless
    // said otherwise.
    val Expires =
      Setting[Text](t"inviteExpiry", t"how long the invitation may be accepted for, as 30m, 2h or 1d")

    val Follow =
      Flag[Unit]("follow", false, proscenium.List('f'), "keep showing events until Ctrl+C")

    // The least a logged event must matter for `swarm log` to show it.
    val Threshold =
      Setting[Text](t"logLevel", t"show only events at this level or above: fine, info, warn or fail")

    // The port this machine accepts other machines on: `--port` or `-p` on the command line,
    // and `swarm-port` in a config file, since plain `port` is the web front-end's.
    val SwarmPort: Setting of Text =
      val description: Text = t"the port on which to accept other machines"

      new Setting(t"swarmPort", Flag[Text](t"port", false, List('p'), description), Unset):
        type Topic = Text

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

  // The newest release each tool in this daemon knows of, by tool name, from the last check
  // (`refresh`); and the tools whose daily check has been started in this daemon.
  private val releases: juc.ConcurrentHashMap[Text, Release] = juc.ConcurrentHashMap()
  private val checking: juc.ConcurrentHashMap[Text, Boolean] = juc.ConcurrentHashMap()

  // How old a cached manifest may be before it is fetched again: the check is daily.
  private val checkInterval: Long = 24*60*60*1000L

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

  // Reads (and so registers) a setting, for the same reason.
  private def settingValue(setting: Setting of Text)(using Cli, Interpreter, Configurator)
  :   Optional[Text] =
    setting()

  // The ports of the services launched in this daemon, by tool name and keyword, for the
  // swarm to advertise.
  private val ports: juc.ConcurrentHashMap[Text, Int] = juc.ConcurrentHashMap()

  // What this daemon serves, as the swarm advertises it.
  private def offered(tool: Tool): List[pyrocosm.Service] =
    val prefix: Text = t"${tool.name}/"
    val iterator = ports.entrySet.nn.iterator.nn
    var found: scala.List[pyrocosm.Service] = scala.Nil

    while iterator.hasNext do
      val entry = iterator.next.nn
      val key: Text = entry.getKey.nn
      if key.starts(prefix) then found = pyrocosm.Service(key.skip(prefix.length), entry.getValue.nn) :: found

    List(found.reverse*)

  // The two services every tool's daemon offers through its node: being the machine's gateway
  // (`swarm` in a config, on `swarm-port`, with `swarm-token` as the secret callers present),
  // and keeping links to the machines a `connect` line names.
  private def gatewayService(tool: Tool): Service = new Service:
    def keyword: Text = t"swarm"
    def portKeyword: Text = t"swarmPort"
    def port: Int = Wire.port

    @scala.caps.unsafe.untrackedCaptures
    
    private var stopped: Optional[Promise[Unit]] = Unset

    def serve(number: Int, settings: Text => Optional[Text])(using Monitor, Probate): Unit =
      val waiting: Promise[Unit] = Promise()
      stopped = waiting
      val token: Optional[Text] = settings(t"swarmToken").let(Machine.secret(_))
      Swarm.of(tool.name).let(_.wantGateway(number, token))
      safely(waiting.attend())

    def stop(): Unit =
      Swarm.of(tool.name).let(_.stopGateway())
      stopped.let(_.offer(()))

  private[pyrocosm] def swarmServices(tool: Tool): List[Service] =
    List(gatewayService(tool), connectService(tool))

  private def connectService(tool: Tool): Service = new Service:
    def keyword: Text = t"connect"
    def portKeyword: Text = t"connectPort"
    def port: Int = 0

    @scala.caps.unsafe.untrackedCaptures
    
    private var stopped: Optional[Promise[Unit]] = Unset

    // A service has no invocation, and so no project: the machines are those the user's own
    // configuration and the shared file declare.
    def serve(number: Int, settings: Text => Optional[Text])(using Monitor, Probate): Unit =
      import environments.javaBaseEnvironment
      val known: List[Machine] = Machine.resolve(List(tool.userConfig, Machine.shared))
      val names: List[Text] = settings(t"connect").lay(Nil: List[Text])(_.cut(t":"))
      val waiting: Promise[Unit] = Promise()
      stopped = waiting
      Swarm.of(tool.name).let: swarm =>
        known.filter { machine => names.has(machine.name) }.each: machine => swarm.connect(machine)
      safely(waiting.attend())

    def stop(): Unit =
      Swarm.of(tool.name).let(_.disconnect())
      stopped.let(_.offer(()))

  // Offers the configured machines' names at the argument after a swarm subcommand.
  private def suggestMachines(rest: List[Argument], known: List[Machine])(using Cli): Unit =
    rest.prim.let: argument =>
      val names: List[Suggestion] = known.map: machine => Suggestion(machine.name)
      summon[Cli].suggest(argument, names, t"", t"")

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

    // The version as a `Semver`, which only a release's (or a snapshot's) version is: `Unset`
    // for a development build, whose version is a tree hash.
    def releaseVersion: Optional[Semver] = safely(tool.version.as[Semver])

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

    // The credential called `name`, resolved through the cascade every Pyrocosm tool shares:
    // the repository's `config.tel`, then the user's `<name>/config.tel`, then the shared
    // `$XDG_CONFIG_HOME/pyrocosm/credentials.tel`, then a built-in default for a well-known
    // name (`anthropic` reads `ANTHROPIC_API_KEY`). The first declaration wins, and its sources
    // are tried in order (see `Credential`). `Unset` if no source yields a value.
    def credential(name: Text, directory: Text)(using Environment, WorkingDirectory)
    :   Optional[Text] =

      val declared: List[Credential] =
        Credential.resolve(List(tool.repoConfig(directory), tool.userConfig, Credential.shared))

      (declared + Credential.defaults).filter(_.name == name).prim.let(_.obtain())

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
    // `Cli` and `Resident` that are also passed here, which separation checking (on in
    // the tools' builds) rejects. The flags are read through `present`, below, and never here.
    inline def standard(inline dispatch: Configurator ?=> Execution)
       (using cli: Cli, service: Resident, monitor: Monitor, environment: Environment)
       (using Interpreter)
    :   Execution =

      val directory: Text = cli.workingDirectory.directory()
      given prefix: Configurator.Prefix = Configurator.Prefix(tool.name)

      given configurator: Configurator =
        Configurator.properties ++ Configurator.environment ++ tool.configurator(directory)

      tool.launch()
      tool.report()

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

            // The `swarm` subcommands: each arm reads what it needs through the non-inline
            // helpers and hands the work to `Swarming`, so that the inlined dispatch stays small.
            case Tool.ui.Swarm() :: Tool.ui.Invite() :: _ =>
              val port: Int = Tool.settingValue(Tool.ui.SwarmPort).let { text => safely(text.as[Int]) }.or(Wire.port)
              val token: Optional[Text] = configurator.read(t"swarmToken")
              val expires: Optional[Text] = Tool.settingValue(Tool.ui.Expires)
              execute(tool.swarmInvite(port, token, expires))

            case Tool.ui.Swarm() :: Tool.ui.Join() :: rest =>
              val words: List[Text] = rest.map { (argument: Argument) => argument() }
              execute(tool.swarmJoin(words))

            case Tool.ui.Swarm() :: Tool.ui.Listen() :: _ =>
              val port: Int = Tool.settingValue(Tool.ui.SwarmPort).let { text => safely(text.as[Int]) }.or(Wire.port)
              val token: Optional[Text] = configurator.read(t"swarmToken")
              execute(tool.swarmListen(port, token))

            case Tool.ui.Swarm() :: Tool.ui.Connect() :: rest =>
              val known: List[Machine] = Swarming.machines(tool, directory)
              Tool.suggestMachines(rest, known)
              val words: List[Text] = rest.map { (argument: Argument) => argument() }
              execute(tool.swarmConnect(words, known))

            case Tool.ui.Swarm() :: Tool.ui.Disconnect() :: rest =>
              val known: List[Machine] = Swarming.machines(tool, directory)
              Tool.suggestMachines(rest, known)
              val words: List[Text] = rest.map { (argument: Argument) => argument() }
              execute(tool.swarmDisconnect(words))

            case Tool.ui.Swarm() :: Tool.ui.Ping() :: rest =>
              val known: List[Machine] = Swarming.machines(tool, directory)
              Tool.suggestMachines(rest, known)
              val words: List[Text] = rest.map { (argument: Argument) => argument() }
              execute(tool.swarmPing(words, known))

            case Tool.ui.Swarm() :: Tool.ui.Identity() :: _ =>
              val port: Int = Tool.settingValue(Tool.ui.SwarmPort).let { text => safely(text.as[Int]) }.or(Wire.port)
              execute(tool.swarmIdentity(port))

            case Tool.ui.Swarm() :: Tool.ui.Peers() :: _ =>
              execute(tool.swarmPeers())

            case Tool.ui.Swarm() :: Tool.ui.Revoke() :: rest =>
              val words: List[Text] = rest.map { (argument: Argument) => argument() }
              execute(tool.swarmRevoke(words))

            case Tool.ui.Swarm() :: Tool.ui.Log() :: _ =>
              val follow: Boolean = Tool.flagPresent(Tool.ui.Follow)
              val level: Level = Swarming.level(Tool.settingValue(Tool.ui.Threshold))
              execute(tool.swarmLog(follow, level))

            case Tool.ui.Swarm() :: _ =>
              execute(tool.swarmStatus())

            case _ =>
              val upgrade: Subcommand = tool.upgradeCommand

              arguments match
                case upgrade() :: _ =>
                  // The manifest is fetched afresh for a real invocation only — never for a
                  // tab-completion — so that `upgrade` sees a release published since the daily
                  // check. Two `execute` blocks, not one returning a three-way union of
                  // statuses, which capture checking rejects (soundness#1811).
                  cli match
                    case _: Invocation => tool.refresh(force = true)
                    case _             => ()

                  tool.available match
                    case Unset             => execute(tool.noUpgrade())
                    case release: Release  => execute(tool.stage(release))

                case _ =>
                  dispatch

        case _ =>
          dispatch

    // Launches every service the configuration asks for (`serve`, `listen`, …), once per
    // daemon. Runs on every invocation, since the daemon starts with the first of them, but
    // only for a real invocation: never for a tab-completion or the help tree's probe. The
    // task runs under the daemon's own monitor, which the `cli` block supplies, so it outlives
    // the client that happened to start it.
    private def launch()
       (using cli: Cli, monitor: Monitor, configurator: Configurator, environment: Environment)
    :   Unit =

      cli match
        case _: Invocation =>
          tool.check()

          // The daemon's node on the swarm, started with the first invocation: it advertises
          // whatever services this daemon has launched.
          Swarm.start(tool.name, tool.version, () => Tool.offered(tool))

          tool.allServices.each: (service: Service) =>
            val wanted: Boolean = configurator.read(service.keyword).present
            val key: Text = Text(s"${tool.name.s}/${service.keyword.s}")

            if wanted && Tool.serving.putIfAbsent(key, service) == null
            then
              val port: Int =
                configurator.read(service.portKeyword).let { text => safely(text.as[Int]) }
                . or(service.port)

              val settings: Text => Optional[Text] = configurator.read(_)
              Tool.ports.put(key, port)
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
        Tool.ports.put(key, port)
        try service.serve(port, settings) finally
          Tool.serving.remove(key)
          Tool.ports.remove(key)

    // Starts the daily check for a newer release, once per daemon, for a released build whose
    // configuration does not say `no-upgrade-check`. The task runs under the daemon's monitor,
    // so it outlives the invocation that started it, and it never raises: a failed check leaves
    // the last known release in place.
    private def check()
       (using monitor: Monitor, configurator: Configurator, environment: Environment)
    :   Unit =

      val wanted: Boolean =
        tool.releaseVersion.present && !configurator.read(t"noUpgradeCheck").present

      if wanted && Tool.checking.putIfAbsent(tool.name, true) == null then
        async:
          while true do
            tool.refresh(force = false)
            snooze(24*Hour)

    // The cached manifest, `$XDG_CACHE_HOME/<name>/upgrade.tsv`, so that a daemon restarted
    // within a day of the last check does not fetch again.
    private def cacheDirectory(using Environment): Optional[Path on Linux] =
      safely(Xdg.cacheHome[Path on Linux] / Name[Linux](tool.name))

    private def manifestCache(using Environment): Optional[Path on Linux] =
      tool.cacheDirectory.let { directory => safely(directory / Name[Linux](t"upgrade.tsv")) }

    // The newest release, from the cache if it is fresh enough and `force` is not set, and
    // otherwise fetched (and cached). Whatever is learned is remembered for `available`; `Unset`
    // if nothing could be read.
    private def refresh(force: Boolean)(using Environment): Optional[Release] =
      val cache: Optional[Path on Linux] = tool.manifestCache

      val cached: Optional[Release] =
        if force then Unset else cache.let: file =>
          safely(summon[FilesystemBackend on Linux].stat(file, true)).let: stat =>
            val age: Long = java.lang.System.currentTimeMillis() - stat.modified
            if age < Tool.checkInterval then safely(file.read[Text]).let(Release.parse(_))
            else Unset

      val release: Optional[Release] = cached.or:
        safely(Release.manifestUrl(tool.name).as[HttpUrl].fetch().receive[Text]).let: text =>
          Release.parse(text).also:
            // The directory may not exist yet, and a write into a missing directory fails.
            tool.cacheDirectory.let: directory =>
              safely:
                if !directory.existent() then directory.create[Directory](CreateFlag.Parents)
                (directory / Name[Linux](t"upgrade.tsv")).write(text)

      release.let: release =>
        Tool.releases.put(tool.name, release)

      release

    // The newest release this daemon knows of, if it is newer than the running build: by build
    // id when the launcher gave the daemon one, and by version otherwise.
    private def available(using resident: Resident): Optional[Release] =
      Optional(Tool.releases.get(tool.name)).let: release =>
        val newer: Boolean =
          if resident.buildId > 0 then release.build > resident.buildId
          else
            tool.releaseVersion.let: current =>
              safely(release.version.as[Semver]).let(current < _)
            . or(false)

        if newer then release else Unset

    // The `upgrade` subcommand to match: suggested while a newer release is known, hidden
    // otherwise. Either way the word is accepted.
    private def upgradeCommand(using resident: Resident): Subcommand =
      if tool.available.present then Tool.ui.Upgrade else Tool.ui.HiddenUpgrade

    // Reports, once and on standard error, what the launcher did with a staged upgrade: this
    // invocation's launcher checked `.pending` before it connected, so its verdict is already on
    // disk. Nothing is printed for a tab-completion or the help tree's probe.
    private def report()(using cli: Cli, environment: Environment): Unit =
      cli match
        case invocation: Invocation =>
          given Stdio = invocation.stdio

          Upgrade.outcome.let: outcome =>
            outcome.result match
              case Upgrade.Outcome.Result.Applied =>
                Err.println(t"${tool.name}: upgraded to ${tool.version}")

              case result =>
                val reason: Message = result.communicate
                Err.println(t"${tool.name}: the staged upgrade was not applied: $reason")

            Upgrade.acknowledge()

        case _ =>
          ()

    // `upgrade` when nothing newer is known: the manifest has just been fetched afresh by
    // `standard`, so this is the newest release, or the check could not reach it.
    private def noUpgrade()(using invocation: Invocation): Tool.NoUpgrade.type =
      given Stdio = invocation.stdio
      Out.println(t"No release newer than ${tool.name} ${tool.version} is known")
      Tool.NoUpgrade

    // `upgrade` when `release` is newer: downloads its executable for this platform, checks the
    // digest, and stages it for the launcher to verify and apply. An executable built without a
    // key cannot be upgraded in place, and is told how to reinstall instead.
    private def stage(release: Release)(using invocation: Invocation, environment: Environment)
    :   Tool.UpgradeFailed.type | Exit =

      given Stdio = invocation.stdio

      if Upgrade.pending then
        Out.println(t"An upgrade is already staged, and takes effect when ${tool.name} next runs")
        Exit.Ok
      else if !Upgrade.enabled then
        val name: Text = tool.name
        Out.println(t"$name ${release.version} is available, but this executable has no key.")
        Out.println(t"Reinstall it with: curl -fsSL https://propensive.dev/${tool.name} | sh")
        Tool.UpgradeFailed
      else
        val platform: Optional[Text] = Release.platform
        val executable: Optional[Release.Executable] = platform.let(release.executable(_))

        executable match
          case Unset =>
            Out.println(t"${tool.name} ${release.version} has no executable for this platform")
            Tool.UpgradeFailed

          case executable: Release.Executable =>
            Out.println(t"Downloading ${tool.name} ${release.version} for ${platform.or(t"?")}...")

            Release.download(executable) match
              case Unset =>
                Out.println(t"The download failed, or did not have the digest the release promised")
                Tool.UpgradeFailed

              case data: Data =>
                recover:
                  case error: Upgrade.Error =>
                    Out.println(t"The upgrade could not be staged: ${error.message}")
                    Tool.UpgradeFailed

                . protect:
                    Upgrade.stage(data)
                    val name: Text = tool.name
                    val version: Text = release.version
                    Out.println(t"$name $version is staged, and takes effect when $name next runs")
                    Exit.Ok

    private def showVersion()(using invocation: Invocation): Exit =
      given Stdio = invocation.stdio
      Out.println(tool.version)
      Exit.Ok

    private def about(directory: Text)
       (using invocation: Invocation, service: Resident, environment: Environment)
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
    // needs an `Entrypoint`, which the ambient Ethereal `Resident` supplies (it extends
    // `Entrypoint`). The manpage's structure comes from `service.help()` — the same subcommand
    // and flag tree the completions register, discovered by re-running the dispatch in completion
    // mode — so `man <name>` can never disagree with the CLI, and the EXIT STATUS section is
    // populated from the `Status` unions of the `execute` blocks. `force = true` for the
    // completions installs even when the tool is not yet on the `PATH`, so a freshly-built binary
    // can set itself up before being installed as a command.
    private def install(force: Boolean)
       (using invocation: Invocation, service: Resident)
       (using erased Effectful)
    :   Tool.InstallFailed.type | Exit =

      given Stdio = invocation.stdio

      // The `Resident` extends `Entrypoint`, and `Completions.ensure` accepts a TRACKED
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

    // The `swarm` subcommands, each a thin wrapper around `Swarming` with the invocation's
    // standard streams in scope.
    private def swarmInvite(port: Int, token: Optional[Text], expires: Optional[Text])
       (using invocation: Invocation, monitor: Monitor, environment: Environment)
    :   Swarming.Swarmed =

      given Stdio = invocation.stdio
      val lifetime: Optional[Duration] = expires.let(Swarming.lifetime(_))

      if expires.present && lifetime.absent then
        Out.println(t"an expiry is a whole number and a unit: 30m, 2h, 1d")
        Tool.UsageError
      else Swarming.invite(tool, port, token, lifetime.or(3600.0*Second))

    private def swarmJoin(words: List[Text])
       (using invocation: Invocation, monitor: Monitor, environment: Environment)
    :   Swarming.Swarmed =

      given Stdio = invocation.stdio

      words match
        case word :: name :: _ => Swarming.join(tool, word, name)
        case word :: _         => Swarming.join(tool, word, Unset)
        case _                 =>
          Out.println(t"Usage: ${tool.name} swarm join <invitation> [name]")
          Tool.UsageError

    private def swarmListen(port: Int, token: Optional[Text])
       (using invocation: Invocation, monitor: Monitor, environment: Environment)
    :   Swarming.Swarmed =

      given Stdio = invocation.stdio
      Swarming.listen(tool, port, token)

    private def swarmConnect(words: List[Text], known: List[Machine])
       (using invocation: Invocation, monitor: Monitor)
    :   Swarming.Connected =

      given Stdio = invocation.stdio

      words match
        case name :: _ =>
          known.seek(_.name == name) match
            case machine: Machine => Swarming.connect(tool, machine)
            case _                =>
              Out.println(t"no machine named $name is configured")
              Tool.NoMachine

        case _ =>
          Out.println(t"Usage: ${tool.name} swarm connect <machine>")
          Tool.UsageError

    private def swarmDisconnect(words: List[Text])(using invocation: Invocation)
    :   Swarming.Connected =

      given Stdio = invocation.stdio

      words match
        case name :: _ => Swarming.disconnect(tool, name)
        case _         => Swarming.deafen(tool)

    private def swarmPing(words: List[Text], known: List[Machine])
       (using invocation: Invocation, monitor: Monitor)
    :   Exit | Tool.RemoteFailed.type | Tool.NoMachine.type | Tool.UsageError.type =

      given Stdio = invocation.stdio

      words match
        case name :: note =>
          known.seek(_.name == name) match
            case machine: Machine => Swarming.ping(tool, machine, note.join(t" "))
            case _                =>
              Out.println(t"no machine named $name is configured")
              Tool.NoMachine

        case _ =>
          Out.println(t"Usage: ${tool.name} swarm ping <machine> [note]")
          Tool.UsageError

    private def swarmIdentity(port: Int)(using invocation: Invocation, environment: Environment)
    :   Swarming.Pinged =

      given Stdio = invocation.stdio
      Swarming.identity(tool, port)

    private def swarmPeers()(using invocation: Invocation, environment: Environment)
    :   Swarming.Swarmed =

      given Stdio = invocation.stdio
      Swarming.peers(tool)

    private def swarmRevoke(words: List[Text])(using invocation: Invocation, environment: Environment)
    :   Swarming.Swarmed =

      given Stdio = invocation.stdio

      words match
        case name :: _ => Swarming.revoke(tool, name)
        case _         =>
          Out.println(t"Usage: ${tool.name} swarm revoke <name>")
          Tool.UsageError

    private def swarmLog(follow: Boolean, level: Level)
       (using invocation: Invocation, resident: Resident, monitor: Monitor)
    :   Exit =

      given Stdio = invocation.stdio
      Swarming.log(follow, level)

    private def swarmStatus()(using invocation: Invocation): Swarming.Connected =
      given Stdio = invocation.stdio
      Swarming.status(tool)

    // Stops the front-end this daemon serves, if any, then the daemon itself. The shutdown is
    // deferred until this invocation's exit status has been delivered, so the client returns
    // cleanly.
    private def quit()(using invocation: Invocation, service: Resident, monitor: Monitor): Exit =
      given Stdio = invocation.stdio

      tool.allServices.each: (service: Service) =>
        val key: Text = Text(s"${tool.name.s}/${service.keyword.s}")
        Optional(Tool.serving.remove(key)).let(_.stop())
        Tool.ports.remove(key)

      Swarm.stop(tool.name)
      Out.println(t"${tool.name}: stopping the daemon")
      service.shutdown()
      Exit.Ok
