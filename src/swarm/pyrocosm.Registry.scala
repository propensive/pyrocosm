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

import charsets.utf8Charset
import filesystemBackends.javaBaseFilesystem
import systems.javaBaseSystem

// How the tools' daemons on one machine find each other: each node writes one small file,
// named by its process and tool, under `$XDG_RUNTIME_DIR/pyrocosm/swarm/nodes/` (or the state
// home, where there is no runtime directory, as ethereal keeps its sockets), saying which port
// its listener is on, and whether it holds the machine's gateway port. A node reads the
// directory to find the others, and deletes any file whose process is gone. Each file is
// written whole and moved into place, so a reader never sees half of one; the names are
// unique per process, so no lock is needed.
object Registry:
  // One node's record.
  case class Record
    ( tool:     Text,
      version:  Text,
      pid:      Long,
      started:  Long,
      port:     Int,
      identity: Text,
      gateway:  Optional[Int] ):

    def name: Text = t"${pid.show}-$tool.tel"

    // Whether the process the record describes is still running: by its pid, and by its
    // start — a pid reused by a later process has a later start than the record says.
    def alive: Boolean = Registry.alive(pid, started)

  // Where the records are: under the runtime directory, or the state home.
  def directory(using Environment): Optional[Path on Linux] =
    safely:
      val base: Path on Linux = Directories.runtimeDir[Path on Linux].or(Directories.stateHome[Path on Linux])
      base / "pyrocosm" / "swarm" / "nodes"

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
    lines.filter(keyed(_)).prim.let(_.skip(prefix.length).trim)

  private def parse(text: Text): Optional[Record] =
    val lines: List[Text] = text.cut(t"\n")

    def number(key: Text): Optional[Long] = field(lines, key).let { text => safely(text.as[Long]) }

    field(lines, t"tool").let: tool =>
      number(t"pid").let: pid =>
        number(t"started").let: started =>
          number(t"port").let: port =>
            Record
              ( tool, field(lines, t"version").or(t"unknown"), pid, started, port.toInt,
                field(lines, t"identity").or(t""), number(t"gateway").let(_.toInt) )

  private def path(directory: Path on Linux): jnf.Path = jnf.Path.of(directory.encode.s).nn
  private def temporaryName(record: Record): String = s".${record.name.s}.tmp"
  private def fileName(pid: Long, tool: Text): String = s"${pid}-${tool.s}.tel"

  private def write(root: jnf.Path, record: Record): Boolean =
    try
      jnf.Files.createDirectories(root)
      val temporary: jnf.Path = root.resolve(temporaryName(record)).nn
      jnf.Files.writeString(temporary, line(record).s)
      jnf.Files.move(temporary, root.resolve(record.name.s).nn, jnf.StandardCopyOption.ATOMIC_MOVE, jnf.StandardCopyOption.REPLACE_EXISTING)
      true
    catch case _: Exception => false

  private def delete(root: jnf.Path, name: String): Unit =
    try jnf.Files.deleteIfExists(root.resolve(name).nn) catch case _: Exception => ()

  // The live records under `root`, deleting the dead as they are found.
  private def scan(root: jnf.Path): List[Record] =
    if !jnf.Files.isDirectory(root) then Nil else
      val listing: java.util.stream.Stream[jnf.Path] | Null =
        try jnf.Files.list(root) catch case _: Exception => null

      if listing == null then Nil else
        try
          val iterator: java.util.Iterator[jnf.Path] = listing.iterator.nn
          var records: scala.List[Record] = scala.Nil

          while iterator.hasNext do
            val path: jnf.Path = iterator.next.nn
            val name: String = path.getFileName.nn.toString

            if name.endsWith(".tel") && !name.startsWith(".") then
              read(path) match
                case record: Record => records = record :: records
                case _              => ()

          List(records.reverse*)
        finally listing.close()

  private def read(path: jnf.Path): Optional[Record] =
    val text: Text | Null = try jnf.Files.readString(path).nn.tt catch case _: Exception => null

    if text == null then Unset else parse(text) match
      case record: Record =>
        if record.alive then record else
          try jnf.Files.deleteIfExists(path) catch case _: Exception => ()
          Unset

      case _ =>
        Unset

// The registry under `directory`; the machine's own, by default.
class Registry(directory: Optional[Path on Linux]):
  import Registry.Record

  private def root: Optional[jnf.Path] = directory.let(Registry.path(_))

  // Writes `record` whole, then moves it into place.
  def register(record: Record): Boolean =
    root.lay(false)(Registry.write(_, record))

  def unregister(pid: Long, tool: Text): Unit =
    root.let(Registry.delete(_, Registry.fileName(pid, tool)))

  // Every record whose process is still running; the rest are deleted as they are found.
  def scan(): List[Record] = root.lay(Nil: List[Record])(Registry.scan(_))

  // The record which holds the machine's gateway port, if a live one does.
  def gateway(): Optional[Record] =
    def holds(record: Record): Boolean = record.gateway.present
    scan().filter(holds(_)).prim
