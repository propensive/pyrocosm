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

import java.nio.file as jnf

import soundness.*

import systems.javaBaseSystem

// How the tools' daemons on one machine find the machine's hub: the hub writes one small file,
// `hub.tel`, under `$XDG_RUNTIME_DIR/pyrocosm/swarm/` (or the state home, where there is no
// runtime directory, as ethereal keeps its sockets), saying which port its listener is on and
// whether it holds the machine's gateway port, and holds a lock beside it, `hub.lock`, for as
// long as it runs, so that two hubs started at once leave one. A leaf reads the file to find
// the hub, and deletes it if the process it names is gone. The file is written whole and moved
// into place, so a reader never sees half of one.
object Registry:
  // The hub's record.
  case class Record
    ( tool:     Text,
      version:  Text,
      pid:      Long,
      started:  Long,
      port:     Int,
      identity: Text,
      gateway:  Optional[Int] ):

    // Whether the process the record describes is still running: by its pid, and by its
    // start — a pid reused by a later process has a later start than the record says.
    def alive: Boolean = Registry.alive(pid, started)

  // Where the record is: under the runtime directory, or the state home.
  def directory(using Environment): Optional[Path on Linux] =
    safely:
      val base: Path on Linux =
        Directories.runtimeDir[Path on Linux].or(Directories.stateHome[Path on Linux])

      base / "pyrocosm" / "swarm"

  private val hubName: String = "hub.tel"
  private val lockName: String = "hub.lock"
  private val temporaryName: String = ".hub.tel.tmp"

  private def gatewayLine(port: Int): Text = t"\ngateway ${port.show}"

  private def line(record: Record): Text =
    val gateway: Text = record.gateway.lay(t"")(gatewayLine(_))

    t"""tool ${record.tool}
version ${record.version}
pid ${record.pid.show}
started ${record.started.show}
port ${record.port.show}
identity ${record.identity}$gateway
"""

  // Whether the process `pid` is running, and began no later than a minute after `started`.
  private def alive(pid: Long, started: Long): Boolean =
    val found: java.util.Optional[java.lang.ProcessHandle] = java.lang.ProcessHandle.of(pid).nn

    if !found.isPresent then false else
      val handle: java.lang.ProcessHandle = found.get.nn
      val begun: java.util.Optional[java.time.Instant] = handle.info.nn.startInstant.nn
      val early: Boolean = !begun.isPresent || begun.get.nn.toEpochMilli <= started + 60000L
      handle.isAlive && early

  // Named predicates rather than lambdas, which crash the 3.9.0 compiler inside implicit search
  // (`wildApprox`) when passed to `filter`.
  private def field(lines: List[Text], key: Text): Optional[Text] =
    val prefix: Text = t"$key "
    def keyed(line: Text): Boolean = line.starts(prefix)
    lines.filter(keyed(_)).head.let(_.skip(prefix.length).trim)

  private def parse(text: Text): Optional[Record] =
    val lines: List[Text] = text.cut(t"\n")

    def number(key: Text): Optional[Long] = field(lines, key).let: text => safely(text.as[Long])

    field(lines, t"tool").let: tool =>
      number(t"pid").let: pid =>
        number(t"started").let: started =>
          number(t"port").let: port =>
            Record
              ( tool, field(lines, t"version").or(t"unknown"), pid, started, port.toInt,
                field(lines, t"identity").or(t""), number(t"gateway").let(_.toInt) )

  private def path(directory: Path on Linux): jnf.Path = jnf.Path.of(directory.encode.s).nn

  private def write(root: jnf.Path, record: Record): Boolean =
    try
      jnf.Files.createDirectories(root)
      val temporary: jnf.Path = root.resolve(temporaryName).nn
      jnf.Files.writeString(temporary, line(record).s)

      jnf.Files.move
        ( temporary,
          root.resolve(hubName).nn,
          jnf.StandardCopyOption.ATOMIC_MOVE,
          jnf.StandardCopyOption.REPLACE_EXISTING )

      true
    catch case _: Exception => false

  private def delete(root: jnf.Path): Unit =
    try jnf.Files.deleteIfExists(root.resolve(hubName).nn) catch case _: Exception => ()

  // The record, if its process is still running; a dead one is deleted as it is found.
  private def read(root: jnf.Path): Optional[Record] =
    val path: jnf.Path = root.resolve(hubName).nn
    val text: Text | Null = try jnf.Files.readString(path).nn.tt catch case _: Exception => null

    if text == null then Unset else parse(text) match
      case record: Record =>
        if record.alive then record else
          try jnf.Files.deleteIfExists(path) catch case _: Exception => ()
          Unset

      case _ =>
        Unset

  // Takes the hub's lock, if no process holds it: the lock, which lasts until it is released or
  // the process ends. Absent if another process holds it, or it could not be made.
  private def lock(root: jnf.Path): Optional[java.nio.channels.FileLock] =
    try
      jnf.Files.createDirectories(root)

      val path: jnf.Path = root.resolve(lockName).nn

      val channel: java.nio.channels.FileChannel =
        java.nio.channels.FileChannel
        . open(path, jnf.StandardOpenOption.CREATE, jnf.StandardOpenOption.WRITE)
        . nn

      val held: java.nio.channels.FileLock | Null = channel.tryLock()

      if held == null then
        channel.close()
        Unset
      else
        held
    catch case _: Exception => Unset

// The registry under `directory`; the machine's own, by default.
class Registry(directory: Optional[Path on Linux]):
  import Registry.Record

  private def root: Optional[jnf.Path] = directory.let(Registry.path(_))

  // The machine's hub, if a live one is registered; the record of a dead one is deleted.
  def hub(): Optional[Record] = root.let(Registry.read(_)).or(Unset)

  // Writes `record` whole, then moves it into place.
  def registerHub(record: Record): Boolean = root.lay(false)(Registry.write(_, record))

  def unregisterHub(): Unit = root.let(Registry.delete(_))

  // The hub's lock, if this process can take it.
  def lockHub(): Optional[java.nio.channels.FileLock] = root.let(Registry.lock(_)).or(Unset)
