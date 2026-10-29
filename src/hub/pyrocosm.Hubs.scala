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


import soundness.{Service as _, *}

import java.nio.file as jnf

import environments.javaBaseEnvironment
import probates.cancelProbate
import strategies.throwUnsafely
import threads.platformThreads
import webserverErrorPages.minimalErrorPage

// The `pyrocosm` process: one per machine, the hub of every Pyrocosm tool's node on it. A tool's
// daemon starts it (`ensure`) when the registry names no live hub, from the daemon's own
// classpath, as a JVM of its own; it runs `main`, which starts the `Hub` if it can take the
// hub's lock — two daemons which start one at once leave one — and ends once no leaf has been
// linked to it for a while, or when the JVM is told to stop. It serves the machine's one web
// origin, every tool's front-end under it (`Portal`), on `port` — 8090, which fume's dashboard
// had — or, while that is taken, on a port of its own, which it advertises either way. What it
// does is logged to a file beside the registry, since no invocation reaches its journal.
object Hubs:
  // How long the hub outlives its last leaf, which also covers its start, before any has linked.
  val grace: Duration = 30.0*Second

  // The port of the machine's web origin.
  val port: Int = 8090

  // How long a daemon waits for a hub it started to register itself.
  val patience: Duration = 10.0*Second

  // The variables the hub needs of the daemon's environment: where the registry, the identity
  // and the shared files are.
  private val inherited: List[Text] =
    List(t"HOME", t"USER", t"PATH", t"XDG_RUNTIME_DIR", t"XDG_STATE_HOME", t"XDG_CONFIG_HOME", t"XDG_CACHE_HOME", t"TMPDIR")

  private def registry(using Environment): Registry = Registry(Registry.directory)

  // The log the hub writes, beside its record.
  private def logFile(using Environment): Optional[jnf.Path] =
    Registry.directory.let { directory => jnf.Path.of(directory.encode.s).nn.resolve("hub.log").nn }

  // Whether a live hub is registered.
  def running(using Environment): Boolean = registry.hub().present

  // Starts a hub for this machine, if none is registered, and waits for it to register itself:
  // whether a hub is registered by the end. `tool` and `version` are the daemon's, which the
  // hub records as its own version, having no release of its own.
  def ensure(tool: Text, version: Text)(using environment: Environment): Boolean =
    if running then true else
      spawn(tool, version)
      val deadline: Long = java.lang.System.currentTimeMillis + (patience.value*1000.0).toLong

      while !running && java.lang.System.currentTimeMillis < deadline do
        Thread.sleep(100)

      running

  // A JVM of its own, from this JVM's classpath, with the daemon's environment, its output to
  // the log file; nothing is read from it, and it is not waited for.
  private def spawn(tool: Text, version: Text)(using environment: Environment): Unit =
    try
      val executable: String = java.lang.System.getProperty("java.home").nn + "/bin/java"
      val classpath: String = java.lang.System.getProperty("java.class.path").nn
      val command: java.util.List[String] =
        java.util.List.of(executable, "-cp", classpath, "pyrocosm.Hubs", tool.s, version.s).nn

      val builder: ProcessBuilder = ProcessBuilder(command)
      builder.redirectInput(ProcessBuilder.Redirect.from(java.io.File("/dev/null")))

      logFile match
        case log: jnf.Path =>
          jnf.Files.createDirectories(log.getParent.nn)
          builder.redirectErrorStream(true)
          builder.redirectOutput(ProcessBuilder.Redirect.appendTo(log.toFile.nn))

        case _ =>
          builder.redirectOutput(ProcessBuilder.Redirect.DISCARD)
          builder.redirectError(ProcessBuilder.Redirect.DISCARD)

      val variables: java.util.Map[String, String] = builder.environment().nn
      variables.clear()
      inherited.each { name => environment.variable(name).let { value => variables.put(name.s, value.s) } }
      builder.start()
      ()
    catch case _: Exception => ()

  // The hub process. Its arguments are the tool which started it and that tool's version.
  def main(arguments: scala.Array[String]): Unit =
    val words: scala.collection.immutable.ArraySeq[String] = scala.collection.immutable.ArraySeq.unsafeWrapArray(arguments)
    def argument(index: Int): Text = words.lift(index).map(_.tt).getOrElse(t"unknown")
    val tool: Text = argument(0)
    val version: Text = argument(1)

    supervise:
      Swarm.hub(version) match
        case hub: Hub =>
          async(transcribe())
          java.lang.System.err.nn.println(s"pyrocosm: hub started by ${tool.s} ${version.s}, pid ${hub.node.pid}")

          // The web origin: on its port, or on one of its own while that is taken.
          val proxy: Portal = Portal(hub, port)

          val origin: Optional[Int] = proxy.serve().or:
            java.lang.System.err.nn.println(s"pyrocosm: port $port is taken")
            Portal(hub, Port[Tcp]().number).serve()

          origin.let: port =>
            hub.web = port
            java.lang.System.err.nn.println(s"pyrocosm: serving http://localhost:$port/")

          linger(hub)
          proxy.stop()
          hub.stop()
          java.lang.System.err.nn.println("pyrocosm: hub stopped")

        case _ =>
          java.lang.System.err.nn.println("pyrocosm: another process is the hub")

    java.lang.System.exit(0)

  // Waits while any leaf is linked, and for `grace` after the last has gone.
  private def linger(hub: Hub)(using Monitor): Unit =
    var quiet: Long = java.lang.System.currentTimeMillis
    var lingering: Boolean = true

    while lingering do
      snooze(1.0*Second)
      val moment: Long = java.lang.System.currentTimeMillis
      if !hub.leaves.nil then quiet = moment
      else if moment - quiet > (grace.value*1000.0).toLong then lingering = false

  // Copies the journal to standard output, which is the log file, as it grows; the fine
  // detail — a beat each second each way — is left out, as `swarm log` leaves it out.
  private def transcribe()(using Monitor): Unit =
    var mark: Long = 0L

    while true do
      Journal.daemon.since(mark, Level.Info).each: entry =>
        mark = entry.sequence
        java.lang.System.out.nn.println(Journal.render(entry).s)

      snooze(1.0*Second)
