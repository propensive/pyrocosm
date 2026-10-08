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

import errorDiagnostics.emptyDiagnostics
import probates.cancelProbate

// The `swarm` subcommand every Pyrocosm tool has, as `Tool.standard` dispatches it: what this
// daemon's node knows of the swarm, and the commands by which a machine joins others — invite,
// join, listen, connect, disconnect, ping, identity, peers, revoke — and `log`, which shows
// what the node has been doing. Each is an ordinary method, called from the inline dispatch,
// so that the dispatch stays small.
object Swarming:
  // How a command which may fail to reach another machine can end.
  type Swarmed = Exit | Tool.RemoteFailed.type | Tool.UsageError.type

  // How a command which names a machine can end.
  type Connected = Exit | Tool.NoMachine.type | Tool.UsageError.type

  // The levels an event may be logged at, by the names `--log-level` takes.
  private val levels: Map[Text, Level] =
    Map(t"fine" -> Level.Fine, t"info" -> Level.Info, t"warn" -> Level.Warn, t"fail" -> Level.Fail)

  def level(name: Optional[Text]): Level = name.let(levels.at(_)).or(Level.Info)

  // A length of time as `--expires` takes it: a whole number and a unit, `s`, `m`, `h` or `d`.
  def lifetime(text: Text): Optional[Duration] =
    val units: Map[Text, Double] = Map(t"s" -> 1.0, t"m" -> 60.0, t"h" -> 3600.0, t"d" -> 86400.0)
    val unit: Text = text.skip(text.length - 1)
    val count: Optional[Int] = safely(text.keep(text.length - 1).as[Int])

    units.at(unit).let: seconds =>
      count.let: count => if count > 0 then (count*seconds)*Second else Unset

  // The machines declared to this invocation: the repository's configuration, then the user's,
  // then the shared `~/.config/pyrocosm/machines.tel`, a name's first declaration winning.
  def machines(tool: Tool, directory: Text)(using Environment): List[Machine] =
    Machine.resolve(List(tool.repoConfig(directory), tool.userConfig, Machine.shared))

  private def usage(form: Text)(using Stdio): Tool.UsageError.type =
    Out.println(t"Usage: $form")
    Tool.UsageError

  private def noNode()(using Stdio): Tool.RemoteFailed.type =
    Out.println(t"this daemon has no node on the swarm; is `keytool` there?")
    Tool.RemoteFailed

  // `<tool> swarm listen [--port]`: asks the node to be the machine's gateway, waits long enough
  // to see whether it could take the port, and says which. A gateway which cannot bind its port
  // gives up at once, saying why in the log, so one still listening after a moment has started.
  def listen(tool: Tool, port: Int, token: Optional[Text], quiet: Boolean = false)
    ( using Stdio, Monitor, Environment )
  :   Swarmed =

    Swarm.of(tool.name).lay[Swarmed](noNode()): swarm =>
      val mark: Long = Journal.daemon.latest

      def already(port: Int): Swarmed =
        if !quiet then Out.println(t"this machine is already accepting others on port ${port.show}")
        Exit.Ok

      def accepting(port: Int): Swarmed =
        if !quiet then
          Out.println(t"accepting other machines on port ${port.show}, in the background")
          Out.println(t"this machine's identity is ${swarm.node.identity}")
          Out.println(t"`${tool.name} swarm disconnect` stops it; `${tool.name} swarm log` shows what it does")

        Exit.Ok

      def failed(): Swarmed =
        Journal.daemon.since(mark, Level.Fail).each: entry => Err.println(entry.message.text)
        Tool.RemoteFailed

      swarm.listening.lay[Swarmed]:
        swarm.wantGateway(port, token)
        snooze(0.5*Second)
        swarm.listening.lay(failed())(accepting(_))
      . apply(already(_))

  // `<tool> swarm invite [--expires] [--port]`: starts the gateway, if this machine has none,
  // and prints the one word another machine joins with, and how to use it.
  def invite(tool: Tool, port: Int, token: Optional[Text], lifetime: Duration)
    ( using Stdio, Monitor, Environment )
  :   Swarmed =

    Swarm.of(tool.name).lay[Swarmed](noNode()): swarm =>
      def failed(error: Peer.Error): Swarmed =
        Out.println(error.message.text)
        Tool.RemoteFailed

      def issued(port: Int): Swarmed =
        recover:
          case error: Peer.Error => failed(error)

        . protect:
            val invitation: Invitation = swarm.invite(port, lifetime)
            val word: Text = Invitation.encode(invitation)
            Out.println(word)
            Err.println(t"")
            Err.println(t"On the other machine, run:")
            Err.println(t"")
            Err.println(t"  ${tool.name} swarm join $word")
            Err.println(t"")
            Err.println(t"It admits one machine, for every Pyrocosm tool, until ${Journal.time(invitation.expires)}.")
            Err.println(t"Until then, a machine on the same network finds this one by name.")
            Exit.Ok

      // Listening first, quietly, so that the invitation is all this prints to standard output.
      listen(tool, port, token, quiet = true) match
        case Tool.RemoteFailed => Tool.RemoteFailed
        case _                 => swarm.listening.lay[Swarmed](Tool.RemoteFailed)(issued(_))

  // `<tool> swarm join <invitation> [name]`: looks for the machine the invitation is to on the
  // local network, records it under `name` or the name it gives, and keeps a link to it.
  def join(tool: Tool, word: Text, name: Optional[Text])(using Stdio, Monitor, Environment)
  :   Swarmed =

    Swarm.of(tool.name).lay[Swarmed](noNode()): swarm =>
      def refused(error: Invitation.Error): Swarmed =
        Out.println(error.message.text)
        Tool.RemoteFailed

      def unreached(error: Peer.Error): Swarmed =
        Out.println(error.message.text)
        Tool.RemoteFailed

      def joined(machine: Machine): Swarmed =
        Out.println(t"joined ${machine.name}, at ${machine.hosts.join(t", ")}")
        connect(tool, machine)
        Exit.Ok

      recover:
        case error: Invitation.Error => refused(error)
        case error: Peer.Error       => unreached(error)

      . protect:
          val invitation: Invitation = Invitation.parse(word)
          val hosts: List[Text] = Swarm.locate(invitation)
          if !hosts.nil then Out.println(t"found ${invitation.name} nearby, at ${hosts.join(t", ")}")
          val machine: Machine = swarm.join(Swarm.nearer(invitation, hosts), name.or(invitation.name))
          joined(machine)

  // `<tool> swarm peers`: the machines this one has admitted by invitation.
  def peers(tool: Tool)(using Stdio, Environment): Swarmed =
    Swarm.of(tool.name).lay[Swarmed](noNode()): swarm =>
      val admitted: List[Text] = swarm.peers

      if admitted.nil then Out.println(t"no machine has joined this one by invitation")
      else admitted.each: name => Out.println(name)

      Exit.Ok

  // `<tool> swarm revoke <name>`.
  def revoke(tool: Tool, name: Text)(using Stdio, Environment): Swarmed =
    Swarm.of(tool.name).lay[Swarmed](noNode()): swarm =>
      val count: Int = swarm.revoke(name)

      if count == 0 then Out.println(t"$name was not admitted by invitation") else
        Out.println(t"$name is refused from now on")

      Exit.Ok

  // `<tool> swarm disconnect`, with no machine: stops being the gateway, and hangs up on every
  // caller from another machine.
  def deafen(tool: Tool)(using Stdio): Connected =
    Swarm.of(tool.name).lay[Connected](Exit.Ok): swarm =>
      val listening: Optional[Int] = swarm.listening
      swarm.stopGateway()

      Out.println:
        listening.lay(t"this machine is not accepting others"): port =>
          t"no longer accepting other machines on port ${port.show}"

      Exit.Ok

  // `<tool> swarm disconnect <machine>`.
  def disconnect(tool: Tool, name: Text)(using Stdio): Connected =
    Swarm.of(tool.name).lay[Connected](Exit.Ok): swarm =>
      if swarm.disconnect(name)
      then Out.println(t"no longer keeping a link to $name")
      else Out.println(t"this daemon is not keeping a link to $name")

      Exit.Ok

  // What is known of the other end of a link, in a few words: what it advertised itself to be,
  // how loaded it last said it was, and when it was last heard from.
  private def described(standing: Tether.Standing): Text =
    def machine(advert: Advert): Text =
      t"${advert.node.tool} ${advert.node.version}; ${advert.os} ${advert.arch}, ${advert.cores.show} cores; "

    def burden(load: Double): Text =
      val hundredths: Long = (load*100.0).toLong
      t"load ${(hundredths/100).show}.${(hundredths%100/10).show}${(hundredths%10).show}; "

    def heard(instant: Instant over Unix): Text = t"last heard from at ${Journal.time(instant)}"

    val advert: Text = standing.advert.let(machine(_)).or(t"")
    val load: Text = standing.load.let(burden(_)).or(t"")
    val last: Text = standing.heard.let(heard(_)).or(t"")
    t"$advert$load$last"

  private def located(url: Text): Text = t"  $url"
  private def host(address: Text): Text = t"    host      $address"

  // `<tool> swarm`: every node on the swarm, the links this node keeps to other machines, the
  // callers from other machines keeping one to it, and how each stands.
  def status(tool: Tool)(using Stdio): Connected =
    Swarm.of(tool.name).lay[Connected](Exit.Ok): swarm =>
      val all: List[Advert] = swarm.nodes()

      Out.println(t"nodes on the swarm:")

      def line(advert: Advert): Text =
        val node: Node = advert.node
        val here: Text = if node.key == swarm.node.key then t" (this daemon)" else t""
        val gateway: Text = if advert.gateway then t", gateway" else t""
        val url: Text = advert.url.lay(t"")(located(_))
        t"  ${node.tool} ${node.version} on ${node.machine}, pid ${node.pid.show}$gateway$here$url"

      all.each: advert => Out.println(line(advert))

      swarm.listening.let: port => Out.println(t"accepting other machines on port ${port.show}")

      val kept: List[Swarm.Connection] = swarm.connections
      val visitors: List[Swarm.Caller] = swarm.visitors

      def kept0(connection: Swarm.Connection): Text =
        val name: Text = connection.machine.name

        if connection.standing.heard.absent then t"  $name  not linked; trying again"
        else t"  $name  linked; ${described(connection.standing)}"

      def visitor(caller: Swarm.Caller): Text =
        t"  ${caller.peer.show}  ${described(caller.standing)}"

      if !kept.nil then
        Out.println(t"")
        Out.println(t"links to other machines:")
        kept.each: connection => Out.println(kept0(connection))

      if !visitors.nil then
        Out.println(t"")
        Out.println(t"linked from other machines:")
        visitors.each: caller => Out.println(visitor(caller))

      Exit.Ok

  // `<tool> swarm connect <machine>`: asks the node to keep the link, waits a moment to see
  // whether it could be made, and says which. Either way the node goes on trying.
  def connect(tool: Tool, machine: Machine)(using Stdio, Monitor): Connected =
    Swarm.of(tool.name).lay[Connected](Exit.Ok): swarm =>
      val name: Text = machine.name
      val mark: Long = Journal.daemon.latest

      def wait(attempts: Int): Unit =
        if attempts > 0 && !swarm.connected(name) then
          snooze(0.1*Second)
          wait(attempts - 1)

      if swarm.connected(name) then Out.println(t"this daemon is already linked to $name") else
        swarm.connect(machine)
        wait(20)

        if swarm.connected(name)
        then Out.println(t"linked to $name; `${tool.name} swarm disconnect $name` closes the link")
        else
          Journal.daemon.since(mark, Level.Warn).each: entry => Out.println(entry.message.text)
          Out.println(t"not linked to $name yet; this daemon will keep trying")

      Exit.Ok

  // How `<tool> swarm ping` can end, once the machine is known.
  type Pinged = Exit | Tool.RemoteFailed.type

  // `<tool> swarm ping`, once the machine is known: says what answered, or why nothing did.
  def ping(tool: Tool, machine: Machine, note: Text)(using Stdio, Monitor): Pinged =
    Swarm.of(tool.name).lay[Pinged](Tool.RemoteFailed): swarm =>
      def failed(error: Swarm.Error): Pinged =
        Out.println(error.message.text)
        Tool.RemoteFailed

      def answered(reply: Swarm.Reply): Pinged =
        val release: Text = reply.version.lay(t"an unknown version"): version => t"version $version"
        val elapsed: Int = (reply.elapsed.value*1000.0).toInt
        Out.println(t"pong from ${reply.node} ($release) in ${elapsed.show}ms")
        Exit.Ok

      recover:
        case error: Swarm.Error => failed(error)

      . protect:
          val reply: Swarm.Reply = swarm.ping(machine, note)
          answered(reply)

  // `<tool> swarm identity`: this machine's certificate fingerprint and addresses, for the
  // `machine` block another machine declares it with by hand.
  def identity(tool: Tool, port: Int)(using Stdio, Environment): Pinged =
    safely(Peer.identity) match
      case identity: Peer.Identity =>
        val hostname: Text = Machine.Identity.local.hostname
        Peer.token
        Out.println(t"identity  ${Peer.render(identity.fingerprint)}")
        Peer.tokenFile.let: file => Out.println(t"token     ${file.encode}")
        Out.println(t"")
        Out.println(t"`${tool.name} swarm invite` is simpler. By hand, declare this machine in")
        Out.println(t"another's ~/.config/pyrocosm/machines.tel as:")
        Out.println(t"")
        Out.println(t"  machine $hostname")
        Machine.addresses.each: address => Out.println(host(address))
        Out.println(t"    port      ${port.show}")
        Out.println(t"    identity  ${Peer.render(identity.fingerprint)}")
        Out.println(t"    token     <a file there, holding the token file's contents>")
        Exit.Ok

      case _ =>
        Out.println(t"this machine's identity could not be created; is `keytool` there?")
        Tool.RemoteFailed

  // `<tool> swarm log [--follow] [--log-level]`: what this node has been doing, oldest first:
  // everything logged at `info` or above, or at the given level or above. The beats of a link
  // and the events the bus carries are `fine`.
  def log(follow: Boolean, level: Level)
    ( using cli: Cli, resident: Resident, stdio: Stdio, monitor: Monitor )
  :   Exit =

    val seen: Atomic[Long] = Atomic(0L)

    def show(): Unit =
      Journal.daemon.since(seen(), level).each: entry =>
        Out.println(Journal.render(entry))
        seen() = entry.sequence

    if follow then until(false)(show()) else show()
    Exit.Ok

  // Runs `action` every quarter of a second until Ctrl+C — the signal, or, on a terminal, the
  // byte the launcher forwards for it (or Ctrl+D) — or until `finished` says so.
  private def until(finished: => Boolean)(action: => Unit)
    ( using cli: Cli, resident: Resident, stdio: Stdio, monitor: Monitor )
  :   Unit =

    val tty: Boolean = resident.cliInput == ethereal.Terminus.Terminal
    val aborted: Atomic[Boolean] = Atomic(false)

    trap:
      case profanity.Signal(Interrupt.Int, _, _, _) =>
        aborted() = true
        SignalResponse.Accept

    var running: Boolean = true

    while running do
      if tty then while stdio.in.available() > 0 do
        val byte: Int = stdio.in.read()
        if byte == 3 || byte == 4 then aborted() = true

      action

      if !aborted() && !finished then snooze(0.25*Second) else running = false
