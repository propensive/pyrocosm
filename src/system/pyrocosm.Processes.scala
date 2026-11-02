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

import java.io as ji
import java.lang as jl

import soundness.*

// A process started and left to run: its output discarded, or appended to `log`, its input
// `/dev/null`, so that it outlives the invocation that started it and holds no terminal.
// What every tool that spawns a server of its own — the machine's hub, a REPL's socket
// server — does, once. The child's environment is this process's with `environment` added,
// or `environment` alone if `inherit` is false; a log's parent directories are created.
object Processes:
  def detach
    ( command:     List[Text],
      environment: List[(Text, Text)] = Nil,
      log:         Optional[Text]     = Unset,
      inherit:     Boolean            = true )
  :   Boolean =

    try
      val builder = jl.ProcessBuilder(command.map(_.s).stdlib*)
      val variables = builder.environment.nn
      if !inherit then variables.clear()
      environment.each { (key, value) => variables.put(key.s, value.s) }
      builder.redirectInput(jl.ProcessBuilder.Redirect.from(ji.File("/dev/null")))

      log match
        case log: Text =>
          val file = ji.File(log.s)
          Optional(file.getParentFile).let(_.mkdirs())
          builder.redirectOutput(jl.ProcessBuilder.Redirect.appendTo(file))
          builder.redirectError(jl.ProcessBuilder.Redirect.appendTo(file))

        case _ =>
          builder.redirectOutput(jl.ProcessBuilder.Redirect.DISCARD)
          builder.redirectError(jl.ProcessBuilder.Redirect.DISCARD)

      builder.start()
      true
    catch case _: Exception => false
