// Pyrocosm's code blocks in the browser: the playing of an animated block, the page's clock
// for the blocks which keep time, and the tabs of a block with alternatives. It needs nothing
// but the HTML `CodeRenderer` writes and the rules of `CodeStyles`, and speaks to no server,
// so a page which is not a Pyrocosm application's can serve it alone: it then wires the
// document itself once that is parsed. Pyrocosm's own script (`pyrocosm.js`, which is served
// after this one, as one file) declares the page `managed` and wires each part of it as it
// arrives, through `window.PyrocosmCode`.
(function () {
  "use strict";

  // The page's one clock, which the animated blocks with a frame at a time follow: at a time
  // t, such a block shows the last of its frames whose time is not after t (a frame without
  // a time follows its predecessor by that frame's hold). The bar — play, the clock and a
  // scrubber — is made when a timed block is first wired, and hidden when none remains;
  // space on the page toggles it, and stepping a block by hand pauses it. A block still
  // mid-step when its frame falls due is caught up on a later tick.
  function after(ms) { return new Promise(function (resolve) { setTimeout(resolve, ms); }); }

  var timeline = (function () {
    var players = [];
    var bar = null, playButton = null, clock = null, scrub = null;
    var elapsed = 0, origin = 0, raf = null, end = 0;
    var reduced = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

    function timed() {
      players = players.filter(function (player) { return document.contains(player.block); });
      return players.filter(function (player) { return player.timed; });
    }

    function format(seconds) { return seconds.toFixed(1) + " s"; }

    function update() {
      clock.textContent = format(elapsed);
      scrub.value = elapsed;
    }

    function targetOf(player, time) {
      var target = 0;
      player.times.forEach(function (at, index) { if (at !== null && at <= time) target = index; });
      return target;
    }

    function apply(animated) {
      timed().forEach(function (player) {
        var target = targetOf(player, elapsed);
        if (target === player.frame() || player.busy()) return;
        player.show(target, animated);
        if (animated) player.block.scrollIntoView({ block: "nearest", behavior: reduced ? "auto" : "smooth" });
      });
    }

    function tick(now) {
      elapsed = Math.min(end, (now - origin) / 1000);
      update();
      apply(true);
      if (elapsed >= end) { pause(); return; }
      raf = requestAnimationFrame(tick);
    }

    function play() {
      if (raf) return;
      if (elapsed >= end) elapsed = 0;
      timed().forEach(function (player) { player.stop(); });
      origin = performance.now() - elapsed * 1000;
      raf = requestAnimationFrame(tick);
      playButton.textContent = "⏸";
    }

    function pause() {
      if (raf) cancelAnimationFrame(raf);
      raf = null;
      if (playButton) playButton.textContent = "▶";
    }

    function toggle() { if (raf) pause(); else play(); }

    function create() {
      bar = document.createElement("div");
      bar.className = "pyro-timeline";
      playButton = document.createElement("button");
      playButton.type = "button";
      playButton.className = "pyro-timeline-play";
      playButton.textContent = "▶";
      playButton.addEventListener("click", toggle);
      clock = document.createElement("span");
      clock.className = "pyro-timeline-clock";
      scrub = document.createElement("input");
      scrub.type = "range";
      scrub.className = "pyro-timeline-scrub";
      scrub.min = "0";
      scrub.step = "0.1";
      scrub.addEventListener("input", function () {
        pause();
        elapsed = parseFloat(scrub.value) || 0;
        update();
        apply(false);
      });
      bar.appendChild(playButton);
      bar.appendChild(clock);
      bar.appendChild(scrub);
      document.body.appendChild(bar);
    }

    // The bar for the timed blocks now on the page: shown, with the clock's end the last
    // frame's time plus its hold, or hidden when there are none.
    function refresh() {
      var active = timed();
      if (!active.length) { if (bar) bar.style.display = "none"; return; }
      if (!bar) create();
      bar.style.display = "";
      end = 0;
      active.forEach(function (player) {
        var last = player.times.length - 1;
        var at = player.times[last];
        if (at !== null) end = Math.max(end, at + (player.data.slides[last].hold || 1.5));
      });
      scrub.max = end.toFixed(1);
      update();
    }

    return {
      register: function (player) { players.push(player); },
      refresh: refresh,
      pause: pause,
      toggle: toggle,
      shown: function () { return !!bar && bar.style.display !== "none"; }
    };
  })();

  // An animated code block: the server rendered its first slide, with each token's atom id,
  // and the animation as JSON — slides, and the transition into each. A step plays a
  // transition: a morph rebuilds the lines of the target slide from the same nodes, keyed by
  // atom, and animates each atom from where it was to where it is (FLIP): a measured move,
  // so an inserted line, a reflowed line or a move across lines all follow; an arriving atom
  // fades in, a departing one leaves as a ghost over its old place. A fade lets the whole
  // slide go before the next appears; an edit types the change, a keypress at a time, behind a
  // cursor, and the slide's own tokens take its place once the typing is done. The
  // slide's annotations fade out before the step and the target's fade in after it, in its
  // language. Beneath the block, its steps: each slide's number and label, the current one
  // marked; a click on another animates the block straight to it. The result is the block's
  // player, which the timeline drives.
  function animate(block) {
    var data;
    try { data = JSON.parse(block.querySelector(".pyro-animation-data").textContent); } catch (error) { console.error(error); return; }
    var code = block.querySelector("pre.pyro-codeblock > code");
    var steps = Array.prototype.slice.call(block.querySelectorAll(".pyro-animation-steps > .pyro-animation-step"));
    var pre = code.parentElement;
    var atoms = {};
    data.atoms.forEach(function (atom) { atoms[atom.id] = atom; });
    var nodes = {};
    code.querySelectorAll("[data-atom]").forEach(function (span) { nodes[span.dataset.atom] = span; });
    var frame = 0;
    var timer = null;
    var reduced = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

    // The step of the slide shown, or being moved to, marked as current; the others not.
    function mark(index) {
      steps.forEach(function (step) {
        var current = parseInt(step.dataset.step, 10) === index;
        step.classList.toggle("pyro-current", current);
        var number = step.querySelector(".pyro-animation-number");
        if (!number) return;
        if (current) number.setAttribute("aria-current", "step"); else number.removeAttribute("aria-current");
      });
    }

    // Where a token is within the block: positions measured so are unaffected by the block
    // itself moving, as it does when its height changes in a container which centres it.
    function place(span) {
      var outer = pre.getBoundingClientRect(), inner = span.getBoundingClientRect();
      return { left: inner.left - outer.left, top: inner.top - outer.top };
    }

    function spanFor(id) {
      if (nodes[id]) return nodes[id];
      var atom = atoms[id];
      var span = document.createElement("span");
      span.className = "pyro-atom pyro-accent-" + atom.accent + (atom.binding ? " pyro-binding" : "");
      span.dataset.atom = id;
      span.textContent = atom.text;
      nodes[id] = span;
      return span;
    }

    function linesOf(index) {
      var lines = [];
      data.placements.forEach(function (placement) {
        if (placement[0] !== index) return;
        (lines[placement[1]] = lines[placement[1]] || []).push(placement[2]);
      });
      for (var i = 0; i < lines.length; i++) lines[i] = lines[i] || [];
      return lines;
    }

    // The line (by its identity across slides) each atom of a slide is on.
    function lineOfAtoms(index) {
      var ids = (data.slides[index] && data.slides[index].lines) || [];
      var result = {};
      linesOf(index).forEach(function (atoms, line) {
        atoms.forEach(function (id) { result[id] = ids[line] || ("#" + line); });
      });
      return result;
    }

    // The transition from one slide to another: the server's, between neighbours (the one
    // back is the one forward reversed), else a morph derived here from the placements, for
    // a jump the clock or the scrubber makes.
    function transitionFor(from, to) {
      var found = null;
      (data.transitions || []).forEach(function (t) {
        if (t.from === from && t.to === to) found = t;
        else if (t.from === to && t.to === from && !found)
          found = { from: from, to: to, kind: t.kind, duration: t.duration, departing: t.arriving, arriving: t.departing,
                    moved: t.moved.map(function (m) { return [m[0], m[2], m[1]]; }), removed: t.inserted, inserted: t.removed };
      });
      if (found) return found;
      var before = lineOfAtoms(from), after = lineOfAtoms(to);
      var moved = [];
      Object.keys(before).forEach(function (id) { if (after[id] && after[id] !== before[id]) moved.push([id, before[id], after[id]]); });
      return {
        from: from, to: to, kind: "morph", duration: null,
        departing: Object.keys(before).filter(function (id) { return !after[id]; }),
        arriving: Object.keys(after).filter(function (id) { return !before[id]; }),
        moved: moved, removed: [], inserted: []
      };
    }

    // The time of each frame on the page's clock: its own, or its predecessor's plus that
    // frame's hold when it has none; null before any frame with a time. A block is timed when
    // any frame of it is.
    var times = [];
    data.slides.forEach(function (f, i) {
      if (typeof f.at === "number") times[i] = f.at;
      else if (i > 0 && times[i - 1] !== null) times[i] = times[i - 1] + (data.slides[i - 1].hold || 1.5);
      else times[i] = null;
    });
    var timed = times.some(function (at) { return at !== null; });

    // A frame's notes laid over its atoms: the note, if any, covering some part of each atom
    // of each line, keyed by atom id — an atom is marked whole, never split.
    function coverage(index) {
      var result = {};
      var notes = (data.slides[index] && data.slides[index].notes) || [];
      linesOf(index).forEach(function (ids, line) {
        var offset = 0;
        ids.forEach(function (id) {
          var end = offset + atoms[id].text.length;
          notes.forEach(function (note) {
            if (result[id]) return;
            var range = rangeOf(note, line, end);
            if (range && range[0] < range[1] && range[0] < end && range[1] > offset) result[id] = note;
          });
          offset = end;
        });
      });
      return result;
    }

    // The columns of line `line` (of `length` characters) which a note marks, as the server
    // computes them: from its start on its first line to its end on its last, whole lines
    // between, clamped to the line; null on a line it does not touch.
    function rangeOf(note, line, length) {
      var last = note.last === null ? note.line : note.last;
      if (line < note.line || line > last) return null;
      var from = line === note.line ? note.start : 0;
      var to = line === last ? note.end : length;
      from = Math.min(from, length);
      return [from, Math.max(Math.min(to, length), from)];
    }

    // The rows beneath line `line` of a frame for its caret and captioned notes, as the
    // server renders them: a diagnostic's caret beneath its position, and its caption on a row
    // of its own at the line's indentation; any other note's caret, or a mark under each
    // column, then its caption.
    function markersOf(index, line, length, indent) {
      var notes = (data.slides[index] && data.slides[index].notes) || [];
      var rows = [];
      function row(note, text) {
        var element = document.createElement("span");
        element.className = "pyro-marker pyro-marker-" + note.style;
        element.appendChild(document.createTextNode(text));
        rows.push(element);
        return element;
      }
      function marks(text) {
        var span = document.createElement("span");
        span.className = "pyro-marks";
        span.textContent = text;
        return span;
      }
      notes.forEach(function (note) {
        var last = note.last === null ? note.line : note.last;
        var caret = note.last === null && note.start === note.end;
        if (last !== line || !(caret || note.caption)) return;
        var range = rangeOf(note, line, length) || [0, 0];
        var pad = caret ? Math.max(range[0] - 1, 0) : range[0];
        var mark = caret ? "╱╲" : "‾".repeat(Math.max(range[1] - range[0], 1));
        var caption = document.createElement("span");
        if (note.caption) caption.innerHTML = note.caption;
        if (note.style === "erroneous" || note.style === "caution") {
          if (caret) row(note, " ".repeat(pad)).appendChild(marks("╱╲"));
          if (note.caption) {
            caption.className = "pyro-marker-caption";
            row(note, indent).appendChild(caption);
          }
        } else {
          var element = row(note, " ".repeat(pad));
          element.appendChild(marks(mark));
          if (note.caption) {
            element.appendChild(document.createTextNode(" "));
            element.appendChild(caption);
          }
        }
      });
      return rows;
    }

    // The target frame's marks and language, applied: classes on the atoms its notes cover,
    // the marker rows shown, the block's language class its own.
    function dress(index) {
      var covered = coverage(index);
      Object.keys(covered).forEach(function (id) {
        nodes[id].classList.add("pyro-marked", "pyro-marked-" + covered[id].style);
      });
      // A frame with a focus mark fades everything but what is marked.
      var focusing = Object.keys(covered).some(function (id) { return covered[id].style === "focus"; });
      pre.classList.toggle("pyro-focusing", focusing);
      code.querySelectorAll(".pyro-marker").forEach(function (row) { row.style.transition = ""; row.style.opacity = ""; });
      var language = (data.slides[index] && data.slides[index].language) || data.language;
      pre.className = pre.className.replace(/\bpyro-language-\S+/, "pyro-language-" + language);
      return Object.keys(covered).length > 0 || code.querySelector(".pyro-marker") !== null;
    }

    // A slide's lines, block-level, each atom's span made by `make`, each line followed by
    // its marker rows, unseen when `hidden`.
    function layout(index, make, hidden) {
      var fragment = document.createDocumentFragment();
      linesOf(index).forEach(function (ids, line) {
        var element = document.createElement("span");
        element.className = "pyro-line";
        var length = 0;
        var text = "";
        ids.forEach(function (id) { element.appendChild(make(id)); length += atoms[id].text.length; text += atoms[id].text; });
        fragment.appendChild(element);
        markersOf(index, line, length, text.match(/^\s*/)[0]).forEach(function (row) {
          if (hidden) { row.style.transition = "none"; row.style.opacity = "0"; }
          fragment.appendChild(row);
        });
      });
      return fragment;
    }

    // Where each atom of a slide would be, and how tall the block would be, were it shown:
    // laid out in an unseen copy of the block, as wide as the block is, and measured.
    function measure(index) {
      var copy = pre.cloneNode(false);
      var inner = code.cloneNode(false);
      var fresh = {};
      function make(id) {
        var atom = atoms[id];
        var span = document.createElement("span");
        span.className = "pyro-atom pyro-accent-" + atom.accent + (atom.binding ? " pyro-binding" : "");
        span.textContent = atom.text;
        fresh[id] = span;
        return span;
      }
      inner.appendChild(layout(index, make, false));
      copy.appendChild(inner);
      copy.style.cssText = "position:absolute;left:0;top:0;visibility:hidden;pointer-events:none;height:auto;" +
        "overflow:visible;transition:none;box-sizing:border-box;width:" + pre.getBoundingClientRect().width + "px";
      pre.parentElement.insertBefore(copy, pre);
      var outer = copy.getBoundingClientRect();
      var last = {};
      Object.keys(fresh).forEach(function (id) {
        var box = fresh[id].getBoundingClientRect();
        last[id] = { left: box.left - outer.left, top: box.top - outer.top };
      });
      var height = outer.height;
      copy.remove();
      return { last: last, height: height };
    }

    // The move of a step from slide `from` to slide `to`, given each surviving atom's
    // displacement and whether the block changes height: which part each movement belongs
    // to, when each part starts, and how long the whole takes. Which line each surviving
    // atom leaves and reaches, by line identity; a line is occupied when some atom stays on
    // it from one slide to the next. A token joining an occupied line slides along its old
    // row to its column first (H1) and then drops into its slot (V); one reaching a new or
    // emptied line drops first (V) and slides along the empty row after (H2), with the
    // tokens shifting within their own lines. A token shifting within its own line moves
    // with the tokens that line exchanges: in H1 when the line receives joiners, so that it
    // makes room before they drop in, and otherwise in H2, closing the gap once the leavers
    // have gone. `dy` is the token's displacement back to where it was: positive means it
    // moves up. The contracting vertical part starts STAGGER after H1 when both exist; the
    // expanding vertical part halfway through the contracting one (or after H1, where
    // there is no contraction); H2 STAGGER after that. The move ends with its last part.
    function planMoves(moves, resized, before, after0, from, to, MOVE) {
      var wasOn = lineOfAtoms(from), nowOn = lineOfAtoms(to);
      var occupied = {};
      Object.keys(nowOn).forEach(function (id) { if (wasOn[id] === nowOn[id]) occupied[nowOn[id]] = true; });
      var receiving = {};
      moves.forEach(function (move) {
        if (wasOn[move.id] !== nowOn[move.id] && occupied[nowOn[move.id]]) receiving[nowOn[move.id]] = true;
      });
      var h1 = [], v1 = [], v2 = [], h2 = [];
      moves.forEach(function (move) {
        var changesLine = wasOn[move.id] !== nowOn[move.id];
        var joining = changesLine && occupied[nowOn[move.id]];
        var early = changesLine ? joining : receiving[nowOn[move.id]];
        if (Math.abs(move.dx) > 0.5) (early ? h1 : h2).push(move);
        if (Math.abs(move.dy) > 0.5) {
          var contracting = changesLine ? joining : move.dy > 0;
          (contracting ? v1 : v2).push(move);
        }
      });
      var shrinking = resized && after0 < before, growing = resized && after0 > before;
      var firstV = v1.length > 0 || shrinking, secondV = v2.length > 0 || growing;
      var s1 = (h1.length && firstV) ? STAGGER : 0;
      var v2Start = firstV ? s1 + MOVE / 2 : (h1.length ? MOVE : 0);
      var s2 = (secondV && h2.length) ? STAGGER : 0;
      var h2Start = v2Start + s2;
      var T = 0;
      if (h1.length) T = Math.max(T, MOVE);
      if (firstV) T = Math.max(T, s1 + MOVE);
      if (secondV) T = Math.max(T, v2Start + MOVE);
      if (h2.length) T = Math.max(T, h2Start + MOVE);
      return { h1: h1, v1: v1, v2: v2, h2: h2, s1: s1, v2Start: v2Start, h2Start: h2Start, T: T,
               shrinking: shrinking, growing: growing };
    }

    // What a step from the slide shown to `target` departs and arrives: what the transition
    // says, and whatever the block holds beyond its say (after a jump); a fade takes
    // everything out and brings everything in.
    function exchange(target, current) {
      var present = {};
      linesOf(target).forEach(function (ids) { ids.forEach(function (id) { present[id] = true; }); });
      var transition = transitionFor(frame, target);
      var fading = transition.kind === "fade";
      var departing = transition.departing.filter(function (id) { return current[id]; });
      var arriving = transition.arriving.filter(function (id) { return present[id] && !current[id]; });
      Object.keys(current).forEach(function (id) { if (!present[id] && departing.indexOf(id) < 0) departing.push(id); });
      Object.keys(present).forEach(function (id) { if (!current[id] && arriving.indexOf(id) < 0) arriving.push(id); });
      if (fading) { departing = Object.keys(current); arriving = Object.keys(present); }
      return { present: present, transition: transition, fading: fading, departing: departing, arriving: arriving };
    }

    // ---- An edit: the change from one slide to another, typed. ------------------------------

    // Timings of an edit, in milliseconds: a keypress which types, one which erases, the
    // cursor's jump to another place, its poise before the first keypress, and its rest after
    // the last. A keypress varies about its time, so that the typing is fast but not even.
    var TYPE = 58, ERASE = 26, JUMP = 280, POISE = 350, REST = 450;

    function classOf(atom) { return "pyro-atom pyro-accent-" + atom.accent + (atom.binding ? " pyro-binding" : ""); }

    // A slide as characters, each with the class of its token and the token's id; a line's end
    // is a character too.
    function charsOf(index) {
      var out = [];
      linesOf(index).forEach(function (ids, line) {
        if (line > 0) out.push({ ch: "\n", cls: "", atom: null });
        ids.forEach(function (id) {
          var cls = classOf(atoms[id]);
          Array.from(atoms[id].text).forEach(function (ch) { out.push({ ch: ch, cls: cls, atom: id }); });
        });
      });
      return out;
    }

    function linesIn(chars) {
      var lines = [[]];
      chars.forEach(function (item) { if (item.ch === "\n") lines.push([]); else lines[lines.length - 1].push(item); });
      return lines;
    }

    function textIn(items) { return items.map(function (item) { return item.ch; }).join(""); }

    // The edits which turn the characters `a` into `b`, as an editor's user would make them,
    // in order: each a place in `a`, a number of characters to erase there, and the
    // characters to type. Lines which are in both stay; a line which changes keeps what it
    // begins and ends with, and has what is between replaced; a line which goes is erased
    // with its end, and one which comes is typed with its own.
    function hunks(a, b) {
      var la = linesIn(a), lb = linesIn(b);
      var ta = la.map(textIn), tb = lb.map(textIn);
      var starts = [], offset = 0;
      la.forEach(function (line) { starts.push(offset); offset += line.length + 1; });
      var n = la.length, m = lb.length, i, j;
      var table = [];
      for (i = 0; i <= n; i++) { table.push(new Array(m + 1).fill(0)); }
      for (i = n - 1; i >= 0; i--) for (j = m - 1; j >= 0; j--)
        table[i][j] = ta[i] === tb[j] ? table[i + 1][j + 1] + 1 : Math.max(table[i + 1][j], table[i][j + 1]);
      var result = [];
      var newline = { ch: "\n", cls: "", atom: null };
      function change(ai, bj) {
        var x = la[ai], y = lb[bj], head = 0, tail = 0;
        while (head < x.length && head < y.length && x[head].ch === y[head].ch) head++;
        while (tail < x.length - head && tail < y.length - head && x[x.length - 1 - tail].ch === y[y.length - 1 - tail].ch) tail++;
        result.push({ at: starts[ai] + head, remove: x.length - head - tail, insert: y.slice(head, y.length - tail) });
      }
      function between(a0, a1, b0, b1) {
        var paired = Math.min(a1 - a0, b1 - b0), k;
        for (k = 0; k < paired; k++) change(a0 + k, b0 + k);
        for (k = a0 + paired; k < a1; k++) {
          if (k + 1 < n) result.push({ at: starts[k], remove: la[k].length + 1, insert: [] });
          else result.push({ at: Math.max(starts[k] - 1, 0), remove: la[k].length + (k > 0 ? 1 : 0), insert: [] });
        }
        for (k = b0 + paired; k < b1; k++) {
          if (a1 < n) result.push({ at: starts[a1], remove: 0, insert: lb[k].concat([newline]) });
          else result.push({ at: a.length, remove: 0, insert: [newline].concat(lb[k]) });
        }
      }
      i = 0; j = 0;
      var a0 = 0, b0 = 0;
      while (i < n && j < m) {
        if (ta[i] === tb[j]) { between(a0, i, b0, j); i++; j++; a0 = i; b0 = j; }
        else if (table[i + 1][j] >= table[i][j + 1]) i++;
        else j++;
      }
      between(a0, n, b0, m);
      return result.filter(function (hunk) { return hunk.remove > 0 || hunk.insert.length > 0; });
    }

    // A source of numbers between 0 and 1 which is the same each time it is made from the
    // same seed, so that an edit takes as long when it is played as when it was measured.
    function seeded(seed) {
      var state = seed >>> 0;
      return function () {
        state = (state + 0x6D2B79F5) >>> 0;
        var t = state;
        t = Math.imul(t ^ (t >>> 15), t | 1);
        t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
        return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
      };
    }

    // The typing of an edit from slide `from` to slide `to`: the characters it begins with,
    // its keypresses — the cursor's moves, the erasures and the characters typed, each after
    // its wait — and how long the whole takes. A character typed into a token which keeps
    // characters of another colour is typed in theirs: the token takes its own colour, whole,
    // when the slide's tokens take the typing's place.
    function typing(from, to, duration) {
      var a = charsOf(from), b = charsOf(to);
      var edits = hunks(a, b);
      var random = seeded(from * 7919 + to * 104729 + a.length * 31 + b.length);

      // The colour of what is kept, by the token of the target it ends in.
      var kept = {}, position = 0, at = 0;
      edits.forEach(function (edit) {
        for (; at < edit.at; at++, position++) if (b[position] && b[position].atom && !kept[b[position].atom]) kept[b[position].atom] = a[at].cls;
        at += edit.remove;
        position += edit.insert.length;
      });
      for (; at < a.length; at++, position++) if (b[position] && b[position].atom && !kept[b[position].atom]) kept[b[position].atom] = a[at].cls;

      var keys = [], delta = 0, total = 0;
      function key(kind, wait, value) { keys.push({ kind: kind, wait: wait, value: value }); total += wait; }
      edits.forEach(function (edit, index) {
        key("move", index === 0 ? 0 : JUMP / 2, edit.at + delta + edit.remove);
        var pause = index === 0 ? POISE : JUMP / 2;
        for (var k = 0; k < edit.remove; k++) { key("erase", pause + ERASE * (0.75 + 0.5 * random())); pause = 0; }
        edit.insert.forEach(function (item) {
          var wait = TYPE * (0.5 + random());
          if (item.ch === " " || item.ch === "\n") wait += TYPE * 0.8 * random();
          if (random() < 0.06) wait += TYPE * 2 * random();
          var typed = { ch: item.ch, cls: (item.atom && kept[item.atom]) || item.cls, atom: item.atom };
          key("type", pause + wait, typed);
          pause = 0;
        });
        delta += edit.insert.length - edit.remove;
      });
      var rest = REST;
      if (duration && total + rest > 0) {
        var scale = duration * 1000 / (total + rest);
        keys.forEach(function (k) { k.wait *= scale; });
        total *= scale; rest *= scale;
      }
      return { start: a, keys: keys, rest: rest, total: keys.length ? total + rest : 0 };
    }

    // The characters of an edit under way, drawn as lines, the cursor before the character at
    // `caret`: runs of one class are one span, and the cursor blinks from when it is drawn, so
    // that it is steady while keys are pressed and blinks when they are not.
    function drawTyping(buffer, caret) {
      var fragment = document.createDocumentFragment();
      var line = null, run = null;
      function open() { line = document.createElement("span"); line.className = "pyro-line"; fragment.appendChild(line); run = null; }
      function cursor() {
        var element = document.createElement("span");
        element.className = "pyro-typing-caret";
        line.appendChild(element);
        run = null;
        if (element.animate)
          element.animate([{ opacity: 0.75, offset: 0 }, { opacity: 0.75, offset: 0.5 }, { opacity: 0, offset: 0.5 }, { opacity: 0, offset: 1 }],
                          { duration: 1000, iterations: Infinity });
      }
      open();
      buffer.forEach(function (item, index) {
        if (index === caret) cursor();
        if (item.ch === "\n") { open(); return; }
        if (!run || run.className !== item.cls) { run = document.createElement("span"); run.className = item.cls; line.appendChild(run); }
        run.textContent += item.ch;
      });
      if (caret >= buffer.length) cursor();
      code.replaceChildren(fragment);
    }

    // An edit's typing, played: each keypress after its wait, and the cursor's rest after the
    // last.
    function type(plan) {
      var buffer = plan.start.slice(), caret = 0, index = 0;
      return new Promise(function (resolve) {
        function next() {
          if (index >= plan.keys.length) { setTimeout(resolve, plan.rest); return; }
          var key = plan.keys[index++];
          setTimeout(function () {
            if (key.kind === "move") caret = key.value;
            else if (key.kind === "erase") { buffer.splice(caret - 1, 1); caret--; }
            else { buffer.splice(caret, 0, key.value); caret++; }
            drawTyping(buffer, caret);
            next();
          }, key.wait);
        }
        next();
      });
    }

    // How long a step to `target` takes until the target's code has fully arrived — its
    // marks' fade out, its departures' fade out, the move and its arrivals' fade in; not the
    // target's marks' fade in, which follows — found before the step starts, by laying the
    // target out unseen and measuring where each surviving token would be, as the step
    // itself does when it reaches its move. Nothing when motion is reduced, since the step
    // then takes no time.
    function lead(target) {
      if (reduced || target === frame) return 0;
      var current = {};
      code.querySelectorAll("[data-atom]").forEach(function (span) { current[span.dataset.atom] = span; });
      var step = exchange(target, current);
      var MOVE = step.transition.duration ? step.transition.duration * 1000 : MOVE_DEFAULT;
      var total = 0;
      if (code.querySelector(".pyro-marked, .pyro-marker") || pre.classList.contains("pyro-focusing")) total += FADE;
      // An edit takes as long as its typing does.
      if (step.transition.kind === "edit") return total + typing(frame, target, step.transition.duration).total;
      if (step.departing.length) total += FADE;
      var measured = measure(target);
      var before = pre.getBoundingClientRect().height;
      var resized = Math.abs(measured.height - before) > 0.5;
      if (step.fading) { if (resized) total += MOVE / 2; }
      else {
        var moves = [];
        Object.keys(step.present).forEach(function (id) {
          if (!current[id] || !measured.last[id]) return;
          var first = place(current[id]), last = measured.last[id];
          var dx = first.left - last.left, dy = first.top - last.top;
          if (Math.abs(dx) > 0.5 || Math.abs(dy) > 0.5) moves.push({ id: id, dx: dx, dy: dy });
        });
        total += planMoves(moves, resized, before, measured.height, frame, target, MOVE).T;
      }
      if (step.arriving.length) total += FADE;
      return total;
    }

    // Timings, in milliseconds; the stylesheet's fades match. A step is a fade out, then a
    // move, then a fade in. The move is two halves. In the first, tokens joining lines that
    // remain slide along their old rows (H1), and STAGGER later the contracting vertical
    // movement runs: those tokens dropping into their lines, and lines shifting up as lines
    // above them go. In the second, the expanding vertical movement runs — tokens reaching
    // new lines, and lines shifting down as lines are inserted above — and STAGGER later the
    // tokens slide along their new rows (H2). Every movement takes MOVE, eased; a half with
    // nothing in it takes no time, and one with no horizontal part has no stagger.
    var FADE = 750, MOVE_DEFAULT = 1000, STAGGER = 300;
    var busy = false;
    var pending = null;   // the step chosen while the block moved, gone to once it settles

    function nextFrame() { return new Promise(function (resolve) { requestAnimationFrame(function () { requestAnimationFrame(resolve); }); }); }

    // One step, in three phases, each awaited before the next: the atoms the target frame
    // lacks fade out and are removed; the target's lines are then rebuilt from the surviving
    // nodes, with the arriving atoms in place but invisible, so that the survivors move
    // (FLIP, from measured geometry) into a layout that already leaves their gaps; and last
    // the arrivals fade in. A phase with nothing to do takes no time.
    function show(target, animated) {
      var moving = animated && !reduced;
      var current = {};
      code.querySelectorAll("[data-atom]").forEach(function (span) { current[span.dataset.atom] = span; });

      // What departs and arrives.
      var step = exchange(target, current);
      var present = step.present, transition = step.transition, fading = step.fading;
      var departing = step.departing, arriving = step.arriving;
      var MOVE = transition.duration ? transition.duration * 1000 : MOVE_DEFAULT;

      // The target's lines, block-level, from the surviving nodes and the arriving atoms,
      // each followed by its marker rows — unseen when the step is animated, so that the
      // layout the survivors move into already leaves their room.
      function rebuild() { code.replaceChildren(layout(target, spanFor, moving)); }

      // Settled on the target: a step chosen while the block moved is gone to now, the
      // latest chosen, so that clicks made mid-move are neither queued nor lost.
      function settle() {
        frame = target;
        busy = false;
        var chosen = pending;
        pending = null;
        if (chosen !== null && chosen !== frame) show(chosen, true); else mark(frame);
      }

      mark(target);

      if (!moving) {
        Object.keys(current).forEach(function (id) { current[id].className = current[id].className.replace(/ ?pyro-marked\S*/g, ""); });
        rebuild();
        Object.keys(present).forEach(function (id) { nodes[id].style.opacity = ""; nodes[id].style.transform = ""; });
        dress(target);
        settle();
        return Promise.resolve();
      }

      busy = true;

      // Phase 0: the frame's marks and marker rows fade out. The rows stay in place,
      // unseen, until the rebuild, so that their going is part of the measured move.
      var marked = code.querySelectorAll(".pyro-marked");
      var rows = code.querySelectorAll(".pyro-marker");
      var focusing = pre.classList.contains("pyro-focusing");
      var undress = Promise.resolve();
      if (marked.length || rows.length || focusing) {
        marked.forEach(function (span) { span.className = span.className.replace(/ ?pyro-marked\S*/g, ""); });
        rows.forEach(function (row) { row.style.opacity = "0"; });
        pre.classList.remove("pyro-focusing");
        undress = after(FADE);
      }

      // An edit has no fades and no move: once the marks have gone, the change is typed, and
      // the target's own tokens then take the typing's place, its marker rows unseen until
      // its marks fade in.
      if (transition.kind === "edit") {
        var plan = typing(frame, target, transition.duration);
        return undress.then(function () {
          return plan.keys.length ? type(plan) : Promise.resolve();
        }).then(function () {
          departing.forEach(function (id) { delete nodes[id]; });
          rebuild();
          Object.keys(present).forEach(function (id) { nodes[id].style.opacity = ""; nodes[id].style.transform = ""; });
          return nextFrame().then(function () { return dress(target) ? after(FADE) : Promise.resolve(); });
        }).then(settle);
      }

      // Phase 1: departures fade out.
      var fadeOut = undress.then(function () {
        if (!departing.length) return Promise.resolve();
        departing.forEach(function (id) { current[id].style.opacity = "0"; });
        return after(FADE);
      });

      return fadeOut.then(function () {
        // Phase 2: survivors move. Their places are measured before the departures are
        // removed (removal reflows what follows them, and that reflow is part of the move)
        // and after the rebuild; arrivals are in the new layout from the start, invisible,
        // so the gaps are real. The block itself goes from its old height to its new over
        // the same time, so lines appearing or vanishing open and close it smoothly.
        var first = {};
        if (!fading) Object.keys(present).forEach(function (id) { if (current[id]) first[id] = place(current[id]); });
        var before = pre.getBoundingClientRect().height;
        departing.forEach(function (id) { current[id].remove(); delete nodes[id]; });
        rebuild();
        arriving.forEach(function (id) { nodes[id].style.transition = "none"; nodes[id].style.opacity = "0"; });
        var after0 = pre.getBoundingClientRect().height;
        var resized = Math.abs(after0 - before) > 0.5;

        // A fade has no move: the block goes to its new height, and the arrivals follow.
        if (fading) {
          if (!resized) return Promise.resolve();
          pre.style.transition = "none";
          pre.style.height = before + "px";
          pre.style.overflow = "hidden";
          void pre.offsetHeight;
          pre.style.transition = "height " + (MOVE / 2) + "ms ease-in-out";
          pre.style.height = after0 + "px";
          return after(MOVE / 2).then(function () { pre.style.transition = ""; pre.style.height = ""; pre.style.overflow = ""; });
        }
        var moves = [];
        Object.keys(first).forEach(function (id) {
          var span = nodes[id];
          var last = place(span);
          var dx = first[id].left - last.left, dy = first[id].top - last.top;
          if (Math.abs(dx) > 0.5 || Math.abs(dy) > 0.5) moves.push({ span: span, id: id, dx: dx, dy: dy });
        });
        if (!moves.length && !resized) return Promise.resolve();

        var plan = planMoves(moves, resized, before, after0, frame, target, MOVE);
        var h1 = plan.h1, v1 = plan.v1, v2 = plan.v2, h2 = plan.h2;
        var s1 = plan.s1, v2Start = plan.v2Start, h2Start = plan.h2Start, T = plan.T;
        var shrinking = plan.shrinking;
        if (T === 0) return Promise.resolve();

        // One animation per axis per token, over the whole move, adding: held at its
        // displacement until its segment, eased to nothing across it, held after.
        function track(span, axis, d, start) {
          var tr = function (value) { return (axis === "x" ? "translateX(" : "translateY(") + value + "px)"; };
          var frames = [{ transform: tr(d), offset: 0, easing: "linear" }];
          if (start > 0) frames.push({ transform: tr(d), offset: start / T, easing: "ease-in-out" });
          else frames[0].easing = "ease-in-out";
          var end = Math.min(1, (start + MOVE) / T);
          frames.push({ transform: tr(0), offset: end, easing: "linear" });
          if (end < 1) frames.push({ transform: tr(0), offset: 1 });
          span.animate(frames, { duration: T, fill: "none", composite: "add" });
        }

        moves.forEach(function (move) {
          if (h1.indexOf(move) >= 0) track(move.span, "x", move.dx, 0);
          else if (h2.indexOf(move) >= 0) track(move.span, "x", move.dx, h2Start);
          if (v1.indexOf(move) >= 0) track(move.span, "y", move.dy, s1);
          else if (v2.indexOf(move) >= 0) track(move.span, "y", move.dy, v2Start);
        });

        // The block's height contracts in the first half and expands in the second.
        if (resized) {
          pre.style.transition = "none";
          pre.style.height = before + "px";
          pre.style.overflow = "hidden";
          void pre.offsetHeight;
          setTimeout(function () {
            pre.style.transition = "height " + MOVE + "ms ease-in-out";
            pre.style.height = after0 + "px";
          }, shrinking ? s1 : v2Start);
        }

        return after(T).then(function () {
          if (resized) { pre.style.transition = ""; pre.style.height = ""; pre.style.overflow = ""; }
        });
      }).then(function () {
        // Phase 3: arrivals fade in.
        if (!arriving.length) return Promise.resolve();
        return nextFrame().then(function () {
          arriving.forEach(function (id) { nodes[id].style.transition = ""; nodes[id].style.opacity = ""; });
          return after(FADE);
        });
      }).then(function () {
        // Phase 4: the target's marks and marker rows fade in.
        return nextFrame().then(function () { return dress(target) ? after(FADE) : Promise.resolve(); });
      }).then(settle);
    }

    function step(delta) {
      if (busy) return false;
      var target = frame + delta;
      if (target < 0 || target >= data.slides.length) {
        if (data.loop && delta > 0) target = 0; else return false;
      }
      show(target, true);
      return true;
    }

    function stop() { if (timer) { clearTimeout(timer); timer = null; } }

    function playing() {
      var hold = (data.slides[frame] && data.slides[frame].hold) || 1.5;
      timer = setTimeout(function () {
        if (!step(1)) { stop(); return; }
        var wait = function () { if (busy) setTimeout(wait, 50); else if (timer) playing(); };
        wait();
      }, hold * 1000);
    }

    function toggle() { if (timer) stop(); else { if (frame === data.slides.length - 1 && !data.loop) show(0, true); playing(); } }

    function manual(delta) { timeline.pause(); stop(); step(delta); }

    // A step chosen: the block animates straight to its slide, or, if it is moving, goes there
    // once it settles. A click on a link in a step's label follows the link instead.
    function choose(index) {
      timeline.pause();
      stop();
      var deck = block.closest(".pyro-deck");
      if (deck && deck.pyroDeck) deck.pyroDeck.halt();
      if (busy) { pending = index; mark(index); return; }
      if (index !== frame) show(index, true);
    }

    steps.forEach(function (step) {
      step.addEventListener("click", function (event) {
        if (event.target.closest && event.target.closest("a")) return;
        choose(parseInt(step.dataset.step, 10));
      });
    });
    block.addEventListener("keydown", function (event) {
      if (block.closest(".pyro-deck")) return;   // a deck steps its blocks itself
      if (event.key === "ArrowRight") { manual(1); event.preventDefault(); }
      else if (event.key === "ArrowLeft") { manual(-1); event.preventDefault(); }
      else if (event.key === " ") { timeline.pause(); toggle(); event.preventDefault(); event.stopPropagation(); }
    });
    show(0, false);
    if (data.autoplay) playing();

    return {
      block: block,
      data: data,
      times: times,
      timed: timed,
      frame: function () { return frame; },
      busy: function () { return busy; },
      lead: lead,
      show: show,
      stop: stop
    };
  }

  // A code block with alternatives shows one at a time: a tab chosen shows its alternative.
  function wireCodeGroup(group) {
    var tabs = Array.prototype.slice.call(group.querySelectorAll(":scope > .pyro-code-header .pyro-code-tab"));
    var alternatives = Array.prototype.slice.call(group.querySelectorAll(":scope > .pyro-alternative"));
    tabs.forEach(function (tab) {
      tab.addEventListener("click", function () {
        var chosen = tab.dataset.alternative;
        tabs.forEach(function (other) { other.classList.toggle("pyro-current", other === tab); });
        alternatives.forEach(function (alternative) {
          alternative.classList.toggle("pyro-current", alternative.dataset.alternative === chosen);
        });
      });
    });
  }

  function wireGroups(root) {
    root.querySelectorAll(".pyro-code-group").forEach(function (group) {
      if (group.dataset.pyroWired) return;
      group.dataset.pyroWired = "1";
      wireCodeGroup(group);
    });
  }

  function wireAnimated(root) {
    root.querySelectorAll(".pyro-animated").forEach(function (block) {
      if (block.dataset.pyroWired) return;
      block.dataset.pyroWired = "1";
      var player = animate(block);
      if (player) { block.pyroPlayer = player; timeline.register(player); }
    });
    timeline.refresh();
  }

  function wire(root) { wireGroups(root); wireAnimated(root); }

  // Space on the page plays or pauses the clock, when it is shown.
  function spaceKey(event) {
    if (event.key !== " " || !timeline.shown()) return;
    if (event.target && event.target.closest && event.target.closest(".pyro-animated, .pyro-editor, input, textarea, select, button, [contenteditable]")) return;
    timeline.toggle();
    event.preventDefault();
  }

  var api = window.PyrocosmCode = {
    wire: wire,
    wireGroups: wireGroups,
    wireAnimated: wireAnimated,
    spaceKey: spaceKey,
    timeline: timeline,
    managed: false
  };

  // Alone on a page, the script wires the document when it has been parsed. A script which
  // manages the page says so as it starts, which is before this runs.
  function start() {
    if (api.managed) return;
    document.addEventListener("keydown", spaceKey);
    wire(document);
  }

  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", start);
  else Promise.resolve().then(start);
})();
