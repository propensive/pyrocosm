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

import soundness.*

// The shared services: what every tool now calls instead of its own copy, each pinned by a
// test so that a change to its behaviour changes every tool at once, knowingly.
object ServicesTests extends Suite(m"Pyrocosm services tests"):
  def run(): Unit =
    suite(m"Timing"):
      test(m"a bare number is seconds"):
        Timing.parse(t"90").let(Timing.millis(_))

      . assert(_ == 90000L)

      test(m"a unit scales the number"):
        List(t"10m", t"2h", t"1d", t"250ms", t"1.5m").map(Timing.parse(_).let(Timing.millis(_)))

      . assert(_ == List(600000L, 7200000L, 86400000L, 250L, 90000L))

      test(m"zero, negative and nonsense are Unset"):
        List(t"0", t"-5s", t"soon", t"").map(Timing.parse(_))

      . assert(_ == List(Unset, Unset, Unset, Unset))

      test(m"a duration shows its coarsest unit first, down to seconds"):
        List(5000L, 125000L, 3605000L, 90005000L, 420L).map(Timing.show(_))

      . assert(_ == List(t"5s", t"2m 5s", t"1h 0m 5s", t"1d 1h 0m 5s", t"420ms"))

      test(m"a moment ago is just now"):
        val now = aviation.now()
        Timing.ago(now, now)

      . assert(_ == t"just now")

      test(m"minutes ago are said in words"):
        val now = aviation.now()
        Timing.ago(Instant.of[Unix](now.long - 8*60*1000L), now)

      . assert(_.contains(t"8 minutes"))

    suite(m"Markdowns"):
      val text: Text =
        t"# Title\n\nHello `code` [x](y).\n\n<!-- hidden -->\n\n" +
          t"```scala\nval x = 1\n```\n\n# Title\n"

      val document: Markdown of Layout = Parser.parse(text)

      test(m"a document converts to blocks, without its comment"):
        Markdowns.convert(document).stdlib.length

      . assert(_ == 4)

      test(m"a fence loses its closing newline"):
        Markdowns.convert(document).stdlib.collectFirst:
          case Block.Code(_, lines, _, _, _, _, _, _) => lines.stdlib.length

      . assert(_ == Some(1))

      test(m"the hooks see every code span, link, fence and heading, with its line"):
        var seen: List[Text] = Nil

        val hooks: Markdowns.Hooks = new Markdowns.Hooks:
          override def code(code: Text, line: Int): Inline =
            seen = t"code:$code:${line.show}" :: seen
            super.code(code, line)

          override def link(destination: Text, content: List[Inline], line: Int): Inline =
            seen = t"link:$destination:${line.show}" :: seen
            super.link(destination, content, line)

          override def fence(info: List[Text], content: Text, line: Int): List[Block] =
            seen = t"fence:${info.join(t",")}:${line.show}" :: seen
            Nil

          override def heading(level: Int, content: List[Inline], line: Int): Block =
            seen = t"heading:${Markdowns.plain(content)}:${line.show}" :: seen
            Block.Heading(level, content)

        Markdowns.convert(document, hooks)
        seen.reverse

      . assert: seen =>
          val expected: List[Text] =
            List
              ( t"heading:Title:1",
                t"code:code:3",
                t"link:y:3",
                t"fence:scala:7",
                t"heading:Title:11" )

          seen == expected

      test(m"repeated headings are numbered as anchors"):
        val anchors = Markdowns.Anchors()
        val first: Text = anchors.anchor(t"title")
        val second: Text = anchors.anchor(t"title")
        val third: Text = anchors.anchor(t"title")
        List(first, second, third, anchors.anchor(t""))

      . assert(_ == List(t"title", t"title-1", t"title-2", t"section"))

      test(m"a slug is lower-case, with spaces hyphenated and punctuation dropped"):
        Markdowns.slug(t"Hello, World! (v2)")

      . assert(_ == t"hello-world-v2")

      test(m"the title is the first heading"):
        Markdowns.title(Markdowns.convert(document))

      . assert(_ == t"Title")

      test(m"rewrite replaces inside a quotation"):
        val blocks: List[Block] = List(Block.Quotation(List(Block.Rule())))

        Markdowns.rewrite(blocks):
          case Block.Rule(_) => List(Block.Paragraph(Inline.text(t"filled")))
          case _             => Unset

      . assert(_ == List(Block.Quotation(List(Block.Paragraph(Inline.text(t"filled"))))))

    suite(m"Progress"):
      test(m"the fraction is the weight done over the weight planned"):
        val progress = Progress[Int](weight = identity(_), caption = _.show)
        progress.plan(1, 10)
        progress.plan(3, 10)
        progress.step(1, 10)
        progress.fraction

      . assert(_ == 0.25)

      test(m"a finished phase counts whole whatever was planned"):
        val progress = Progress[Int](weight = _ => 1, caption = _.show)
        progress.plan(1, 10)
        progress.finish(1)
        progress.fraction

      . assert(_ == 1.0)

    suite(m"Statuses"):
      val standard: List[Int] =
        List
          ( Tool.UsageError, Tool.InstallFailed, Tool.NoUpgrade, Tool.UpgradeFailed, Tool.NoMachine,
            Tool.RemoteFailed, Tool.NoWeb, Tool.NotServing, Tool.Unimplemented, Tool.NoCredential )

        . map(_.code)

      test(m"the standard statuses are distinct"):
        standard.stdlib.toSet.size == standard.stdlib.length

      . assert(_ == true)

      test(m"the standard statuses are 2 or 8 to 31"):
        standard.stdlib.forall: code => code == 2 || (code >= 8 && code <= 31)

      . assert(_ == true)

    suite(m"Themes"):
      test(m"a colour subdued to nothing is the ground"):
        TerminalTheme.Solarized.subdue(Chroma(0xffffff), 0.0)

      . assert(_ == TerminalTheme.Solarized.background)

      test(m"a colour subdued wholly is itself"):
        TerminalTheme.Solarized.subdue(Chroma(0x123456), 1.0)

      . assert(_ == Chroma(0x123456))

      test(m"Solarized's ground is iridescence's base03"):
        TerminalTheme.Solarized.background

      . assert(_ == Chroma(0x002b36))

    suite(m"Frames"):
      test(m"frames split across chunk boundaries"):
        val one: Data = t"hello".sysData
        val two: Data = t"world!".sysData
        val whole: Data = Frames.join(List(Frames.frame(one), Frames.frame(two)))

        val chunks: List[Data] =
          List
            ( Frames.slice(whole, 0, 3),
              Frames.slice(whole, 3, 12),
              Frames.slice(whole, 12, whole.length) )

        Frames.split(chunks).map(_.utf8)

      . assert(_ == List(t"hello", t"world!"))

      test(m"an empty frame survives"):
        Frames.split(Frames.frame(Frames.join(Nil))).stdlib.length

      . assert(_ == 1)

    suite(m"ObjectStore"):
      test(m"a blob is stored under its digest, verified, and pruned"):
        Filing.temporary(t"pyrocosm-test"): directory =>
          val store: ObjectStore = ObjectStore(directory, t".blob")
          val data: Data = t"content".sysData
          val digest: Text = store.put(data)
          val verified: Boolean = store.put(t"bad", data)
          val present: Boolean = store.has(digest) && store.get(digest).let(_.utf8) == t"content"
          val missing: List[Text] = store.missing(List(digest, t"nothere"))
          val pruned: Int = store.prune(Set())
          (digest == Digests.of(data), verified, present, missing, pruned, store.has(digest))

      . assert(_ == (true, false, true, List(t"nothere"), 1, false))

    suite(m"Filing"):
      test(m"a file is replaced whole, with its parents made"):
        Filing.temporary(t"pyrocosm-test"): directory =>
          val file: Path on Linux = unsafely(directory / "deep" / "file.txt")
          val first: Boolean = Filing.replace(file, t"one")
          val second: Boolean = Filing.replace(file, t"two")
          (first, second, Filing.text(file))

      . assert(_ == (true, true, t"two"))

      test(m"a tree digest is stable and names its files"):
        Filing.temporary(t"pyrocosm-test"): directory =>
          Filing.replace(unsafely(directory / "a.txt"), t"a")
          Filing.replace(unsafely(directory / "b.txt"), t"b")
          val one: Text = Digests.tree(directory)
          Filing.replace(unsafely(directory / "b.txt"), t"c")
          (one == Digests.tree(directory), one.length)

      . assert(_ == (false, 64))

    suite(m"Project"):
      test(m"an entry resolves under the root unless absolute"):
        val project = Tool.Project(unsafely(t"/tmp/root".as[Path on Linux]), Unset, Unset)
        List(project.resolve(t"src").encode, project.resolve(t"/abs").encode)

      . assert(_ == List(t"/tmp/root/src", t"/abs"))

      test(m"a path relative to the root loses the root"):
        val project = Tool.Project(unsafely(t"/tmp/root".as[Path on Linux]), Unset, Unset)
        project.relative(t"/tmp/root/src/x.scala")

      . assert(_ == t"src/x.scala")

      test(m"entries split a setting on colons"):
        val project = Tool.Project(unsafely(t"/tmp/root".as[Path on Linux]), Unset, Unset)
        project.entries(t"a::b").map(_.encode)

      . assert(_ == List(t"/tmp/root/a", t"/tmp/root/b"))

    suite(m"Diversions"):
      test(m"two threads' output stays apart"):
        val left = ji.ByteArrayOutputStream()
        val right = ji.ByteArrayOutputStream()
        val sink = ji.ByteArrayOutputStream()

        def print(text: String): Unit = java.lang.System.out.nn.print(text)

        val thread1 = Thread(new Runnable:
          def run(): Unit = Diversions.threaded(left, sink)(print("left")))

        val thread2 = Thread(new Runnable:
          def run(): Unit = Diversions.threaded(right, sink)(print("right")))

        thread1.start(); thread2.start(); thread1.join(); thread2.join()
        (left.toString("UTF-8").nn.tt, right.toString("UTF-8").nn.tt)

      . assert(_ == (t"left", t"right"))
