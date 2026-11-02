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

// Who or what ran the command: an agent, when its environment says so — Codex sets
// `CODEX_SANDBOX=1` and Claude Code `CLAUDECODE=1` — a GitHub Actions workflow, which sets
// `GITHUB_ACTIONS=true`, or otherwise a human at a terminal. An agent and a workflow read a
// terse report, since neither has a screen to fill; a workflow also reads annotations.
enum Invoker:
  case Human, Claude, Codex, GithubActions

  def terse: Boolean = this != Human
  def annotates: Boolean = this == Invoker.GithubActions

object Invoker:
  // Codex's variable is checked first, so an environment carrying both reads as Codex; the
  // workflow last, since an agent may run inside one.
  def detect(using Environment): Invoker =
    if safely(Environment.codexSandbox[Text]) == t"1" then Codex
    else if safely(Environment.claudecode[Text]) == t"1" then Claude
    else if safely(Environment.githubActions[Text]) == t"true" then GithubActions
    else Human

// GitHub Actions workflow annotations: `::error` lines against a file and line, and log groups
// around each section, as the workflow's log folds them.
object GithubActions:
  def enabled(using Environment): Boolean = safely(Environment.githubActions[Text]) == t"true"

  private def workspaceRelative(path: Text)(using Environment): Text =
    safely(Environment.githubWorkspace[Text]).let: workspace =>
      if path.starts(workspace) && path.length > workspace.length && path(workspace.length.z) == '/'
      then path.skip(workspace.length + 1)
      else path

    . or(path)

  private def escape(text: Text): Text =
    text.sub(t"%", t"%25").sub(t"\r", t"%0D").sub(t"\n", t"%0A")

  private def escapeProperty(text: Text): Text =
    escape(text).sub(t",", t"%2C").sub(t":", t"%3A")

  def group(title: Text)(using Stdio): Unit = Out.println(t"::group::${escape(title)}")
  def endGroup()(using Stdio): Unit = Out.println(t"::endgroup::")

  def error
    ( message: Text,
      file:    Optional[Text] = Unset,
      line:    Optional[Int]  = Unset,
      title:   Optional[Text] = Unset )
    ( using Stdio, Environment )
  :   Unit =

    def one(value: Optional[Text]): List[Text] = value.lay(Nil: List[Text]): text => List(text)

    val props: List[Text] =
      one(file.let(workspaceRelative(_)).let { path => t"file=${escapeProperty(path)}" }) +
        one(line.let { value => t"line=${value.show}" }) +
        one(title.let { value => t"title=${escapeProperty(value)}" })

    val header = if props.nil then t"" else t" ${props.join(t",")}"
    Out.println(t"::error$header::${escape(message)}")
