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

// Excluded from the umbrella: `Filter` (panopticon), which would outrank this package's own
// definition, since a wildcard import beats a package member declared in another file.
import soundness.{Filter as _, *}

import logging.silentLogging
import charEncoders.utf8Encoder
import errorDiagnostics.emptyDiagnostics

// Metadata attached to source states through git notes, for every Pyrocosm tool. Two families
// of notes, all under `refs/notes/pyrocosm/`:
//
//  - `refs/notes/pyrocosm/commits`, on a *commit*: the index, one fingerprint per line in the
//    order they were bound, oldest first, without duplicates. The only commit-keyed note, and
//    append-only; binding twice is a no-op.
//  - `refs/notes/pyrocosm/<kind>` (`bench`, `coverage`, …), on a *filtered tree*: the TEL
//    document of a `Recordable` kind.
//
// A note on a tree does not need the tree to exist locally: `git notes` resolves a full hash
// without looking it up, so a clone that fetched the notes can read them without ever having
// computed the fingerprint. (`ci-attestation` is a separate scheme with its own ref, key and
// format, and is outside this layout.)
//
// Notes refs are synced explicitly, by `fetch` and `publish`; neither `git push` nor `git fetch`
// carries them by default.
//
// The class is a capability for one repository: `root` is any directory of a checkout, and `git`
// is found on the search path. Every subprocess is spawned from `run`, and nowhere else.
object Notes:
  val index: Text = t"refs/notes/pyrocosm/commits"
  val refspec: Text = t"refs/notes/pyrocosm/*:refs/notes/pyrocosm/*"
  def ref(kind: Text): Text = t"refs/notes/pyrocosm/$kind"

  object Error:
    enum Reason:
      case BadRef(refspec: Text)
      case GitFailed(exit: Int, stderr: Text)
      case Undecodable(kind: Text, fingerprint: Fingerprint)
      case Diverged(remote: Text)

    given communicable: Reason is Communicable =
      case Reason.BadRef(refspec)                => m"$refspec does not name a commit"
      case Reason.GitFailed(exit, stderr)        => m"git exited with status $exit: $stderr"
      case Reason.Undecodable(kind, fingerprint) => m"the $kind note on ${fingerprint.text} could not be decoded"
      case Reason.Diverged(remote)               => m"the notes on $remote have diverged from the local notes"

  case class Error(reason: Error.Reason)(using Diagnostics)
  extends fulminate.Error(m"the notes operation failed because $reason")

class Notes(root: Text):
  import Notes.Error.Reason.*

  private given WorkingDirectory = () => root

  // Runs a program in the repository and yields its exit status, standard output and standard
  // error; standard error is read after standard output, which suits git's short diagnostics. A
  // failure to spawn at all is a `GitFailed` with no status.
  private def run(program: Text, arguments: List[Text], input: Optional[Text])
  :   (Int, Text, Text) raises Notes.Error =

    mitigate:
      case Exec.Error(_, _, stderr) => Notes.Error(GitFailed(-1, stderr.utf8))
    . protect:
      val job = Command((program :: arguments)*).fork[Text]()
      // A truncated write is a closed pipe: git has exited, and its status says why.
      input.let { text => safely(job.stdin(Stream(text.bytestream))) }
      val output = job.await()
      val status = job.status()
      (status, output, job.errorText())

  private def succeed(result: (Int, Text, Text)): Text raises Notes.Error = result match
    case (0, output, _)     => output
    case (status, _, error) => abort(Notes.Error(GitFailed(status, error.trim)))

  // A `git` command that must succeed; its output.
  private def git(arguments: List[Text]): Text raises Notes.Error =
    succeed(run(t"git", arguments, Unset))

  // The same, with a body on standard input.
  private def feed(input: Text)(arguments: List[Text]): Text raises Notes.Error =
    succeed(run(t"git", arguments, input))

  // A `git` command run against a throwaway index file. `env` sets the variable, since
  // guillotine has no per-command environment.
  private def indexed(index: Text, input: Optional[Text])(arguments: List[Text]): Text raises Notes.Error =
    // Two conses, not one chain: a chained `::` resolves to the underlying list's own.
    val command: List[Text] = t"git" :: arguments
    succeed(run(t"env", t"GIT_INDEX_FILE=$index" :: command, input))

  // A `git` command whose failure means absence, as `git notes show` on an object without a note.
  private def attempt(arguments: List[Text]): Optional[Text] raises Notes.Error =
    run(t"git", arguments, Unset) match
      case (0, output, _) => output
      case _              => Unset

  // Git appends one newline to a shown note that is not part of the stored body.
  private def stripped(text: Text): Text = if text.ends(t"\n") then text.skip(1, Rtl) else text

  private def nul: Text = 0.toChar.toString.tt

  // The lines of some output, trimmed, without blanks.
  private def lines(output: Text): List[Text] = output.cut(t"\n").map(_.trim).filter(_ != t"")

  private def hashes[hash](parse: Text => Optional[hash])(lines: List[Text]): List[hash] =
    def recur(lines: List[Text], done: List[hash]): List[hash] = lines match
      case head :: tail => parse(head).lay(recur(tail, done)) { hash => recur(tail, hash :: done) }
      case _            => done.reverse

    recur(lines, Nil)

  def commit(refspec: Text): Commit raises Notes.Error =
    attempt(List(t"rev-parse", t"--verify", t"--quiet", t"$refspec^{commit}"))
    . let { hash => Commit.parse(hash.trim) }
    . or(abort(Notes.Error(BadRef(refspec))))

  // The fingerprint of a commit under a filter: the commit's tree read into a throwaway index,
  // every excluded path removed, and the tree written back. Nothing is excluded by an empty
  // filter, whose fingerprint is the commit's own tree.
  def fingerprint(commit: Commit, filter: Filter): Fingerprint raises Notes.Error =
    val listing = git(List(t"ls-tree", t"-r", t"-z", t"--name-only", commit.text))
    val excluded = listing.cut(nul).filter(_ != t"").filter(filter.excludes(_))

    if excluded.nil then Fingerprint.unsafe(git(List(t"rev-parse", t"${commit.text}^{tree}")).trim) else
      val directory = java.nio.file.Files.createTempDirectory("pyrocosm-notes").nn
      val index = directory.resolve("index").nn.toString.tt

      try
        indexed(index, Unset)(List(t"read-tree", commit.text))
        indexed(index, excluded.map(_ + nul).join)(List(t"update-index", t"--force-remove", t"-z", t"--stdin"))
        Fingerprint.unsafe(indexed(index, Unset)(List(t"write-tree")).trim)
      finally
        java.nio.file.Files.deleteIfExists(directory.resolve("index").nn)
        java.nio.file.Files.deleteIfExists(directory)

  // The fingerprints bound to a commit, newest first.
  def fingerprints(commit: Commit): List[Fingerprint] raises Notes.Error =
    attempt(List(t"notes", t"--ref=${Notes.index}", t"show", commit.text)).lay(Nil): body =>
      hashes(Fingerprint.parse)(lines(body)).reverse

  // Appends the fingerprint to the commit's index note, unless it is already there.
  def bind(commit: Commit, fingerprint: Fingerprint): Unit raises Notes.Error =
    val current = fingerprints(commit)

    if !current.has(fingerprint) then
      val body = (fingerprint :: current).reverse.map(_.text).join(t"\n")
      feed(body)(List(t"notes", t"--ref=${Notes.index}", t"add", t"-f", t"-F", t"-", commit.text))

  def read[value: Recordable as recordable](fingerprint: Fingerprint)
  :   Optional[value] raises Notes.Error =

    attempt(List(t"notes", t"--ref=${Notes.ref(recordable.kind)}", t"show", fingerprint.text)).let: body =>
      recordable.decode(stripped(body))
      . or(abort(Notes.Error(Undecodable(recordable.kind, fingerprint))))

  def write[value: Recordable as recordable](fingerprint: Fingerprint, value: value)
  :   Unit raises Notes.Error =

    feed(recordable.encode(value))
      (List(t"notes", t"--ref=${Notes.ref(recordable.kind)}", t"add", t"-f", t"-F", t"-", fingerprint.text))

  // The reverse index: every commit whose index note names the fingerprint. A scan of the index
  // ref, so linear in the number of annotated commits.
  def commits(fingerprint: Fingerprint): List[Commit] raises Notes.Error =
    attempt(List(t"notes", t"--ref=${Notes.index}", t"list")).lay(Nil): listing =>
      // Each line is `<note object> <annotated object>`.
      val targets = lines(listing).map: line =>
        line.cut(t" ") match
          case _ :: target :: _ => target
          case _                => t""
      hashes(Commit.parse)(targets).filter(fingerprints(_).has(fingerprint))

  // The commits reachable from a refspec, newest first, up to a limit: the axis of a trend.
  def history(refspec: Text, limit: Int): List[Commit] raises Notes.Error =
    hashes(Commit.parse)(lines(git(List(t"log", t"--format=%H", t"-n", limit.show, refspec))))

  // Brings the remote's notes refs to the local ones. A remote ref that cannot fast-forward the
  // local one is `Diverged`; an absent remote ref is nothing to fetch.
  def fetch(remote: Text = t"origin"): Unit raises Notes.Error =
    run(t"git", List(t"fetch", remote, Notes.refspec), Unset) match
      case (0, _, _)                                              => ()
      case (_, _, error) if error.contains(t"non-fast-forward")   => abort(Notes.Error(Diverged(remote)))
      case (_, _, error) if error.contains(t"rejected")           => abort(Notes.Error(Diverged(remote)))
      case (status, _, error)                                     => abort(Notes.Error(GitFailed(status, error.trim)))

  // Fetches, then pushes the notes refs; a push git rejects is `Diverged`, for the caller to
  // reconcile (`git notes merge`) before trying again.
  def publish(remote: Text = t"origin"): Unit raises Notes.Error =
    fetch(remote)

    run(t"git", List(t"push", remote, Notes.refspec), Unset) match
      case (0, _, _)                                    => ()
      case (_, _, error) if error.contains(t"rejected") => abort(Notes.Error(Diverged(remote)))
      case (status, _, error)                           => abort(Notes.Error(GitFailed(status, error.trim)))
