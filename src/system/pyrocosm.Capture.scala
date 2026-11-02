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

// Capture of the JVM's `System.out` and `System.err` for one thread at a time. Each is a single
// global, so a daemon running two pieces of user code at once — two scripts, two REPL lines,
// two test suites — cannot redirect and restore them around each: one would clobber the
// other's. Instead ONE dispatcher is installed for each, lazily, once for the whole JVM,
// routing every write to the running thread's current capture, set by `threaded` around the
// code, and falling back to the real stream for a thread with none. Scala's own `println`
// is thread-local already (`Console.withOut`); this covers Java prints and anything else that
// writes to the `System` streams.
object Capture:
  private val currentOut: ThreadLocal[ji.OutputStream] = InheritableThreadLocal()
  private val currentErr: ThreadLocal[ji.OutputStream] = InheritableThreadLocal()

  private def dispatcher(current: ThreadLocal[ji.OutputStream], real: ji.OutputStream)
  :   ji.OutputStream =

    new ji.OutputStream:
      private def target: ji.OutputStream = Optional(current.get).or(real)
      def write(byte: Int): Unit = target.write(byte)

      override def write(bytes: scala.Array[Byte] | Null, offset: Int, length: Int): Unit =
        target.write(bytes, offset, length)

      override def flush(): Unit = target.flush()

  private lazy val installed: Unit =
    jl.System.setOut(ji.PrintStream(dispatcher(currentOut, jl.System.out.nn), true, "UTF-8"))
    jl.System.setErr(ji.PrintStream(dispatcher(currentErr, jl.System.err.nn), true, "UTF-8"))

  // Routes this thread's `System.out` and `System.err` to `out` and `err` for the duration of
  // `block`, then restores whatever they were before, so captures nest.
  def threaded[result](out: ji.OutputStream, err: ji.OutputStream)(block: => result): result =
    installed
    val previousOut: ji.OutputStream | Null = currentOut.get
    val previousErr: ji.OutputStream | Null = currentErr.get
    currentOut.set(out)
    currentErr.set(err)

    try block finally
      currentOut.set(previousOut)
      currentErr.set(previousErr)

  // The same, with the streams of a `Stdio`.
  def threaded[result](stdio: Stdio)(block: => result): result =
    threaded(stdio.out, stdio.err)(block)
