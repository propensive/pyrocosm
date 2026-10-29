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

import soundness.{Control as _, Origin as _, Service as _, *}

import codepages.utf8Codepage
import denominative.capped
import internetAccess.online
import socketBackends.javaBaseSockets
import strategies.throwUnsafely

// How the hub talks HTTP to a tool's server on this machine: one HTTP/1.1 exchange per request,
// over a connection of its own, closed once the answer is read. A client library's conveniences
// are not wanted here — no redirects followed, no connection kept — and one of them is harmful:
// Java's HTTP client asks every cleartext server to upgrade to `h2c`, which a server takes for
// a protocol upgrade and so hands its handler the connection's whole remainder as the body,
// which never ends. The same asking is why a handler here reads a body `bounded` by its
// `Content-Length` rather than to its end.
object Loopback:
  val loopback: Host = ip"127.0.0.1"

  // `request`, sent to the server on `port`, and its answer; nothing if the server could not be
  // reached or did not answer HTTP.
  def exchange(port: Int, request: Http.Request): Optional[Http.Response] =
    safely:
      val endpoint: Endpoint[Tcp.Port] = Endpoint(t"127.0.0.1", Port[Tcp](port))
      val duplex: Duplex = summon[Endpoint[Tcp.Port] is Connectable].connect(endpoint, Unset)

      try
        duplex.send(Http.Request.serialize(closing(request)))
        val bytes: Data = duplex.source.memoize
        Http.Response.parse(Chain(bytes))
      finally duplex.close()

  // The request as sent: for one answer, and the connection closed after it, so that the
  // answer's end is the connection's.
  private def closing(request: Http.Request): Http.Request =
    val headers: List[Http.Header] =
      Http.Header(t"connection", t"close") :: request.textHeaders.filter(_.key.lower != t"connection")

    Http.Request(request.method, 1.1, request.host, request.target, headers, request.body)

  // A request's body, read to its `Content-Length` and no further; empty without one.
  def bounded(request: Http.Request): Data =
    val length: Int =
      request.textHeaders.seek(_.key.lower == t"content-length").let { header => safely(header.value.trim.as[Int]) }
      . or(Unset).or(0)

    if length <= 0 then Data() else
      val input = request.body()
      var remaining: Int = length
      var result: Data = Data()
      var reading: Boolean = true

      while reading && remaining > 0 do input.refill(Credit(remaining.toLong)) match
        case count: Int if count > 0 =>
          val take: Int = count.min(remaining)
          val data: Data = input.lend { region => range => region.materialize(range.capped(take)) }
          input.skip(take)
          result = result + data
          remaining -= take

        case _ =>
          reading = false

      result

  // The whole of an answer's body.
  def text(response: Http.Response): Text = response.body.stream.memoize.utf8
