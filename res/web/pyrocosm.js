// Pyrocosm's browser side: the only script any Pyrocosm application serves. It knows nothing
// about the application. It connects a WebSocket (to the page's own session, when the page names
// one), applies patches the server sends (replace an element's content, set a field's value,
// enable or check a control), and reports what the user does by the id of the element they did
// it to (or the action an activity's card names, whose id is its own). Everything else is server-rendered HTML, including a code field's decoration: its
// tokens, note and completions arrive as HTML, and the script paints the editor from them.
(function () {
  "use strict";

  var socket = null;
  var indicator = null;

  function status(online) {
    indicator = indicator || document.getElementById("pyro-connection");
    if (!indicator) return;
    indicator.textContent = online ? "connected" : "reconnecting";
    indicator.classList.toggle("pyro-online", online);
    indicator.classList.toggle("pyro-offline", !online);
  }

  function send(message) {
    if (socket && socket.readyState === WebSocket.OPEN) socket.send(JSON.stringify(message));
  }

  function sessionQuery() {
    var meta = document.querySelector('meta[name="pyro-session"]');
    return meta ? "?session=" + encodeURIComponent(meta.getAttribute("content")) : "";
  }

  // A local destination: a page of this application. Going to one tells the session where the
  // tab now is and lets it change the panels, so the page is not reloaded; the address bar
  // follows (keeping the query, which names the session), and the fragment is scrolled to once
  // the patch which contains it has landed. Back and forward replay the same.
  var pendingFragment = "";

  function navigate(href, push) {
    var url;
    try { url = new URL(href, location.href); } catch (error) { return false; }
    if (url.origin !== location.origin) return false;
    var moved = url.pathname !== location.pathname;
    if (push) history.pushState({}, "", url.pathname + location.search + url.hash);
    if (moved) {
      pendingFragment = url.hash ? url.hash.substring(1) : "";
      send({ kind: "locate", id: "", text: url.pathname, caret: 0, index: 0 });
    } else if (url.hash) {
      var target = document.getElementById(url.hash.substring(1));
      if (target) target.scrollIntoView();
    }
    return true;
  }

  function settle() {
    if (!pendingFragment) return;
    var target = document.getElementById(pendingFragment);
    if (target) { target.scrollIntoView(); pendingFragment = ""; }
  }

  window.addEventListener("popstate", function () { navigate(location.href, false); });

  // The page's mode — light, dark or mixed — kept by the browser and cycled by the masthead's
  // button; the head applied the kept one before the first paint.
  var modes = ["light", "dark", "mixed"];

  function applyMode(mode) {
    document.documentElement.dataset.mode = mode;
    var button = document.getElementById("pyro-mode");
    if (button) button.textContent = mode;
  }

  function wireMode() {
    var button = document.getElementById("pyro-mode");
    if (!button || button.dataset.pyroWired) return;
    button.dataset.pyroWired = "1";
    applyMode(document.documentElement.dataset.mode || "mixed");
    button.addEventListener("click", function () {
      var current = document.documentElement.dataset.mode || "mixed";
      var next = modes[(modes.indexOf(current) + 1) % modes.length];
      try { localStorage.setItem("pyro-mode", next); } catch (error) {}
      applyMode(next);
    });
  }

  // A panel's drawings outlive its repaints: a figure keeps its holder current by its own
  // patches, so the holder the page already has is carried into the panel's new content in
  // place of the (identical or older) copy rendered there, and a transition under way is not
  // cut short.
  // A holder's scroll position is kept too: a detached element forgets where it was scrolled
  // to, so a wide chart being read at its right end would jump back to its left on every
  // repaint of its panel.
  function keepDrawings(root) {
    var kept = {};
    root.querySelectorAll(".pyro-drawing-holder").forEach(function (holder) {
      kept[holder.id] = { node: holder, left: holder.scrollLeft, top: holder.scrollTop };
    });
    return kept;
  }

  function restoreDrawings(root, kept) {
    root.querySelectorAll(".pyro-drawing-holder").forEach(function (holder) {
      var old = kept[holder.id];
      if (!old) return;
      holder.replaceWith(old.node);
      old.node.scrollLeft = old.left;
      old.node.scrollTop = old.top;
    });
  }

  // A panel's collapsible tree nodes keep the open state the reader gave them across its
  // repaints: each is named by its tree's ordinal within the panel and its path within the
  // tree (both stable, since a transcript only grows), and a node the page already showed
  // takes its recorded state in place of the server's default.
  function keepNodes(root) {
    var kept = {};
    root.querySelectorAll(".pyro-tree").forEach(function (tree, ordinal) {
      tree.querySelectorAll("details.pyro-node").forEach(function (node) {
        kept[ordinal + ":" + node.getAttribute("data-node")] = node.open;
      });
    });
    return kept;
  }

  function restoreNodes(root, kept) {
    root.querySelectorAll(".pyro-tree").forEach(function (tree, ordinal) {
      tree.querySelectorAll("details.pyro-node").forEach(function (node) {
        var key = ordinal + ":" + node.getAttribute("data-node");
        if (key in kept) node.open = kept[key];
      });
    });
  }

  function parseSvg(html) {
    var scratch = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    scratch.innerHTML = html;
    return scratch.firstElementChild;
  }

  function sameShape(a, b) {
    if (a.nodeType !== b.nodeType) return false;
    if (a.nodeType !== 1) return true;
    if (a.tagName !== b.tagName || a.childNodes.length !== b.childNodes.length) return false;
    for (var i = 0; i < a.childNodes.length; i++) if (!sameShape(a.childNodes[i], b.childNodes[i])) return false;
    return true;
  }

  function copyAttributes(target, source) {
    var names = {};
    for (var i = 0; i < source.attributes.length; i++) {
      var attribute = source.attributes[i];
      names[attribute.name] = true;
      if (target.getAttribute(attribute.name) !== attribute.value) target.setAttribute(attribute.name, attribute.value);
    }
    for (var j = target.attributes.length - 1; j >= 0; j--) {
      var name = target.attributes[j].name;
      if (!names[name]) target.removeAttribute(name);
    }
  }

  function copyInto(target, source) {
    if (target.nodeType !== 1) { if (target.nodeValue !== source.nodeValue) target.nodeValue = source.nodeValue; return; }
    copyAttributes(target, source);
    for (var k = 0; k < target.childNodes.length; k++) copyInto(target.childNodes[k], source.childNodes[k]);
  }

  function revisePart(element, html) {
    var fresh = parseSvg(html);
    if (fresh && sameShape(element, fresh)) copyInto(element, fresh);
    else element.outerHTML = html;
  }

  // A whole drawing redrawn, part by part: each identified part of the new drawing whose
  // shape the old one shares is kept and updated in place, so its geometry transitions even
  // when the axes around it are replaced — a chart whose bars are all in place from the start
  // grows each bar as its value arrives, whatever the scale does.
  function reviseDrawing(holder, html) {
    var fresh = parseSvg(html);
    var old = holder.firstElementChild;
    if (!fresh || !old || old.tagName !== fresh.tagName) { holder.innerHTML = html; return; }
    copyAttributes(old, fresh);
    var byId = {};
    Array.prototype.forEach.call(old.children, function (child) { if (child.id) byId[child.id] = child; });
    var next = [];
    Array.prototype.slice.call(fresh.childNodes).forEach(function (child) {
      var previous = child.nodeType === 1 && child.id ? byId[child.id] : null;
      if (previous && sameShape(previous, child)) { copyInto(previous, child); next.push(previous); }
      else next.push(child);
    });
    old.replaceChildren.apply(old, next);
  }

  function apply(patch) {
    if (patch.kind === "pong") return;
    var element = document.getElementById(patch.id);
    if (!element) return;
    switch (patch.kind) {
      case "replace":
        if (element.classList.contains("pyro-drawing-holder")) { reviseDrawing(element, patch.html); break; }
        var follow = element.scrollHeight - element.scrollTop - element.clientHeight < 8;
        var drawings = keepDrawings(element);
        var nodes = keepNodes(element);
        element.innerHTML = patch.html;
        restoreDrawings(element, drawings);
        restoreNodes(element, nodes);
        if (follow) element.scrollTop = element.scrollHeight;
        wire(element);
        settle();
        if (/-decoration$/.test(patch.id)) {
          var editor = document.getElementById(patch.id.replace(/-decoration$/, ""));
          if (editor && editor.classList.contains("pyro-editor")) decorate(editor, element);
        }
        break;
      // Another tab's text, or the application's: adopted whole, with its version.
      case "value":
        if (element.classList.contains("pyro-editor")) {
          if (editorText(element) !== patch.html) { clearSuggestion(element); element.textContent = patch.html; trailingBreak(element); placeCaret(element, patch.html.length); emptiness(element); }
          adopt(element, patch.html, patch.version || 0);
        } else if (element.value !== patch.html) element.value = patch.html;
        break;
      // The application's proposed replacement of the text, taken only if the text is still
      // at the version it was based on; a keystroke since has overtaken it.
      case "revise":
        if (element.classList.contains("pyro-editor")) {
          try { var proposal = JSON.parse(patch.html); } catch (error) { console.error(error); break; }
          if (proposal.base === versionOf(element)) setText(element, proposal.text, proposal.caret);
        }
        break;
      // The server's answer to an Enter it was asked about.
      case "verdict":
        if (element.classList.contains("pyro-editor") && pendingEnter[element.id]) {
          try { var verdict = JSON.parse(patch.html); } catch (error) { console.error(error); break; }
          var trusted = verdict.version === versionOf(element) || verdict.version === 0;
          resolveEnter(element, trusted ? verdict.incomplete : looksIncomplete(editorText(element)));
        }
        break;
      // One part of a figure's drawing. When the new markup has the same shape as the old (the
      // same elements in the same order), the attributes are updated in place, so the elements
      // survive and their geometry transitions — a bar grows to its new height. Otherwise it is
      // replaced; an SVG element takes its namespace from its parent, so the markup is parsed
      // as SVG wherever the part sits in the drawing.
      case "part": revisePart(element, patch.html); break;
      case "class": element.classList.add(patch.html); break;
      case "unclass": element.classList.remove(patch.html); break;
      case "enable": element.disabled = false; break;
      case "disable": element.disabled = true; break;
      case "check": element.checked = true; break;
      case "uncheck": element.checked = false; break;
      case "select": element.value = patch.html; break;
      case "history":
        try { histories[patch.id] = JSON.parse(patch.html); recalls[patch.id] = histories[patch.id].length; } catch (error) { console.error(error); }
        break;
    }
  }

  // A socket refused before it ever opened is a session the server no longer knows (it has
  // restarted); after a few such refusals the page reloads to get a fresh one.
  var refusals = 0;

  function connect() {
    var protocol = location.protocol === "https:" ? "wss://" : "ws://";
    var opened = false;
    socket = new WebSocket(protocol + location.host + "/socket" + sessionQuery());
    socket.onopen = function () { opened = true; refusals = 0; status(true); };
    socket.onclose = function () {
      status(false);
      if (!opened && sessionQuery() !== "" && ++refusals >= 5) { location.reload(); return; }
      setTimeout(connect, 1000);
    };
    socket.onmessage = function (event) {
      try { apply(JSON.parse(event.data)); } catch (error) { console.error(error); }
    };
  }

  // ── The versioned text ───────────────────────────────────────────────────────────────────
  // The editor owns its text and numbers its versions: every local edit — a keystroke, a paste,
  // a recall, a completion taken, the clearing on submit — advances the version, which goes to
  // the server with the edit. What comes back (tokens, the Enter verdict, completions) says
  // which version it describes. A decoration of the current version is painted as it is; one
  // of an older version has the edits made since replayed over its tokens (the rule the model
  // calls `Token.rebase`), so the line is never shown plain while highlighting is awaited, nor
  // with another text's colours; and a verdict is trusted only for the version it was given for.
  var versions = {}, lastTexts = {}, logs = {}, verdicts = {}, composing = {};
  var LOGGED = 64;

  function versionOf(editor) {
    if (!(editor.id in versions)) { versions[editor.id] = 0; lastTexts[editor.id] = editorText(editor); logs[editor.id] = []; }
    return versions[editor.id];
  }

  // The one edit turning `before` into `after`: the common prefix and suffix are kept, and
  // what lies between them is the splice.
  function spliceOf(before, after) {
    var shortest = Math.min(before.length, after.length), prefix = 0, suffix = 0;
    while (prefix < shortest && before.charAt(prefix) === after.charAt(prefix)) prefix++;
    while (suffix < shortest - prefix && before.charAt(before.length - 1 - suffix) === after.charAt(after.length - 1 - suffix)) suffix++;
    return { start: prefix, removed: before.length - prefix - suffix, inserted: after.slice(prefix, after.length - suffix) };
  }

  // The text has changed here: a new version, with the edit logged under it.
  function record(editor) {
    versionOf(editor);
    var id = editor.id, text = editorText(editor);
    var splice = spliceOf(lastTexts[id], text);
    versions[id]++;
    logs[id].push({ version: versions[id], splice: splice });
    if (logs[id].length > LOGGED) logs[id].shift();
    lastTexts[id] = text;
    return versions[id];
  }

  // A text adopted whole from elsewhere, with its version when it has one, so every tab
  // numbers the text alike.
  function adopt(editor, text, version) {
    versionOf(editor);
    lastTexts[editor.id] = text;
    if (version > 0) { versions[editor.id] = version; logs[editor.id] = []; }
  }

  function isWord(text) { return /^[A-Za-z0-9_]+$/.test(text); }

  // A token as the script holds it: its text, and the element it was rendered in, which
  // carries its accent class (none for plain text, such as the newline between lines).
  function accentOf(item) {
    var match = item.template && /pyro-accent-([a-z]+)/.exec(item.template.className);
    return match ? match[1] : "unparsed";
  }

  // Which tokens grow when identifier characters are typed inside or at the end of them, and
  // which take anything typed inside them at all: the inside of a string, and unparsed text.
  function extendable(accent) { return accent === "term" || accent === "typal" || accent === "number" || accent === "keyword" || accent === "unparsed"; }
  function absorbent(accent) { return accent === "string" || accent === "unparsed" || accent === "error"; }

  function itemsOf(tokens) {
    var items = [], walker = document.createTreeWalker(tokens, NodeFilter.SHOW_TEXT, null), node;
    while ((node = walker.nextNode())) {
      var parent = node.parentNode;
      var accented = parent && parent.classList && /pyro-accent-/.test(parent.className);
      items.push({ text: node.textContent, template: accented ? parent : null });
    }
    return items;
  }

  // The tokens as they stand after one edit: the removed range cut from the tokens it covers,
  // and the inserted text joining the token it lands in or ends, or standing as plain text of
  // its own until the highlighting of the edited text arrives.
  function rebaseItems(items, splice) {
    var start = splice.start, end = start + splice.removed, offset = 0, cut = [];
    items.forEach(function (item) {
      var a = offset, b = a + item.text.length;
      offset = b;
      if (b <= start || a >= end) { cut.push(item); return; }
      var kept = item.text.slice(0, Math.max(0, start - a)) + item.text.slice(Math.min(item.text.length, end - a));
      if (kept !== "") cut.push({ text: kept, template: item.template });
    });
    var inserted = splice.inserted;
    if (inserted === "") return cut;
    var word = isWord(inserted);
    function grows(item) { return word && extendable(accentOf(item)) && item.text !== "" && isWord(item.text.charAt(item.text.length - 1)); }
    function precedes(item) { return word && extendable(accentOf(item)) && item.text !== "" && isWord(item.text.charAt(0)); }
    function within(item) { return absorbent(accentOf(item)) || (word && extendable(accentOf(item))); }
    var plain = { text: inserted, template: null };
    var result = [], placed = false;
    offset = 0;
    for (var i = 0; i < cut.length; i++) {
      var item = cut[i], a = offset, b = a + item.text.length;
      offset = b;
      if (placed) { result.push(item); continue; }
      if (start === a) {
        var last = result.length ? result[result.length - 1] : null;
        if (last && grows(last)) { result[result.length - 1] = { text: last.text + inserted, template: last.template }; result.push(item); }
        else if (precedes(item)) result.push({ text: inserted + item.text, template: item.template });
        else { result.push(plain); result.push(item); }
        placed = true;
      } else if (start < b) {
        var head = item.text.slice(0, start - a), tail = item.text.slice(start - a);
        if (within(item)) result.push({ text: head + inserted + tail, template: item.template });
        else { result.push({ text: head, template: item.template }); result.push(plain); result.push({ text: tail, template: item.template }); }
        placed = true;
      } else result.push(item);
    }
    if (!placed) {
      var final = result.length ? result[result.length - 1] : null;
      if (final && grows(final)) result[result.length - 1] = { text: final.text + inserted, template: final.template };
      else result.push(plain);
    }
    return result;
  }

  // The items painted into the editor, as lines, in the markup the server's tokens use.
  function paintItems(editor, items) {
    editor.innerHTML = "";
    var line = document.createElement("span");
    line.className = "pyro-line";
    items.forEach(function (item) {
      item.text.split("\n").forEach(function (part, index) {
        if (index > 0) {
          line.appendChild(document.createTextNode("\n"));
          editor.appendChild(line);
          line = document.createElement("span");
          line.className = "pyro-line";
        }
        if (part === "") return;
        if (item.template) { var span = item.template.cloneNode(false); span.textContent = part; line.appendChild(span); }
        else line.appendChild(document.createTextNode(part));
      });
    });
    editor.appendChild(line);
  }

  // ── The code editor ──────────────────────────────────────────────────────────────────────
  // A contenteditable element painted with the server's tokens. The caret is kept as a text
  // offset across repaints; the suggestion, the remainder of the selected completion, is a
  // non-editable span at the end, never part of the text; the completions list beneath is
  // navigated with the arrows.

  function suggestionOf(editor) { return editor.querySelector(".pyro-suggestion"); }

  function editorText(editor) {
    var suggestion = suggestionOf(editor);
    return suggestion ? editor.textContent.slice(0, editor.textContent.length - suggestion.textContent.length) : editor.textContent;
  }

  function clearSuggestion(editor) { var suggestion = suggestionOf(editor); if (suggestion) suggestion.remove(); }

  function showSuggestion(editor, text) {
    clearSuggestion(editor);
    if (!text) return;
    var span = document.createElement("span");
    span.className = "pyro-suggestion";
    span.contentEditable = "false";
    span.textContent = text;
    editor.appendChild(span);
  }

  function hasSuggestion(editor) { return !!suggestionOf(editor); }

  function caretOf(editor) {
    var selection = window.getSelection();
    if (!selection.rangeCount) return 0;
    var range = selection.getRangeAt(0);
    if (!editor.contains(range.endContainer)) return editorText(editor).length;
    var before = range.cloneRange();
    before.selectNodeContents(editor);
    before.setEnd(range.endContainer, range.endOffset);
    return before.toString().length;
  }

  function placeCaret(editor, offset) {
    var walker = document.createTreeWalker(editor, NodeFilter.SHOW_TEXT, null);
    var node, remaining = offset, target = null, targetOffset = 0;
    while ((node = walker.nextNode())) {
      if (node.parentNode && node.parentNode.classList && node.parentNode.classList.contains("pyro-suggestion")) continue;
      var length = node.textContent.length;
      if (remaining <= length) { target = node; targetOffset = remaining; break; }
      remaining -= length;
    }
    var range = document.createRange();
    var trailing = editor.querySelector("br.pyro-break");
    // At the very end of a text that ends with a newline, the caret goes before the trailing
    // break, which has the line box the position after the newline lacks.
    if (trailing && offset >= editorText(editor).length) range.setStartBefore(trailing);
    else if (target) range.setStart(target, targetOffset);
    else { range.selectNodeContents(editor); range.collapse(false); }
    range.collapse(true);
    var selection = window.getSelection();
    selection.removeAllRanges();
    selection.addRange(range);
  }

  // A trailing line break, so that an empty last line — or one of indentation alone — has a
  // line box of its own: without one, the caret after a final newline is drawn, and text typed
  // there lands, at the end of the line before. It is never part of the text.
  function trailingBreak(editor) {
    var text = editorText(editor), last = text.lastIndexOf("\n");
    var wanted = last >= 0 && /^[ \t]*$/.test(text.slice(last + 1));
    var existing = editor.querySelector("br.pyro-break");
    if (wanted && !existing) { var br = document.createElement("br"); br.className = "pyro-break"; editor.appendChild(br); }
    else if (!wanted && existing) existing.remove();
  }

  function emptiness(editor) {
    var holder = editor.parentNode;
    if (holder && holder.classList.contains("pyro-field-group"))
      holder.classList.toggle("pyro-empty", editorText(editor) === "");
  }

  function isIdentifier(character) { return /[A-Za-z0-9_]/.test(character); }

  function stemOf(editor) {
    var text = editorText(editor), caret = caretOf(editor), start = caret;
    while (start > 0 && isIdentifier(text.charAt(start - 1))) start--;
    return text.slice(start, caret);
  }

  function completionsOf(editor) {
    var decoration = document.getElementById(editor.id + "-decoration");
    if (!decoration || dismissed[editor.id]) return [];
    return Array.prototype.slice.call(decoration.querySelectorAll(".pyro-completions li"));
  }

  // The candidates are shown as they arrive, but none is taken until the user picks one with
  // Tab or Down: only then does Enter accept it, and the suggestion preview it. Escape dismisses
  // them until the next edit.
  var dismissed = {};

  function selectedCompletion(editor) {
    var items = completionsOf(editor);
    for (var i = 0; i < items.length; i++) if (items[i].classList.contains("pyro-selected")) return items[i];
    return null;
  }

  function moveCompletion(editor, delta) {
    var items = completionsOf(editor);
    if (!items.length) return false;
    var index = -1;
    for (var i = 0; i < items.length; i++) if (items[i].classList.contains("pyro-selected")) index = i;
    if (index >= 0) items[index].classList.remove("pyro-selected");
    if (index < 0) index = delta > 0 ? 0 : -1;
    else index = Math.min(items.length - 1, index + delta);
    if (index >= 0) items[index].classList.add("pyro-selected");
    suggest(editor);
    return true;
  }

  function dismiss(editor) {
    dismissed[editor.id] = true;
    var decoration = document.getElementById(editor.id + "-decoration");
    var list = decoration && decoration.querySelector(".pyro-completions");
    if (list) list.hidden = true;
    clearSuggestion(editor);
  }

  function nameOf(item) { var code = item.querySelector("code"); return code ? code.textContent : item.textContent; }

  // What a candidate covers: the whole text when it says so, else the identifier before the caret.
  function coveredBy(editor, item) { return item.classList.contains("pyro-replacement") ? editorText(editor) : stemOf(editor); }

  // The suggestion: what the selected completion adds to the identifier before the caret, when
  // the caret ends the text.
  function suggest(editor) {
    var item = selectedCompletion(editor);
    var text = editorText(editor);
    if (!item || caretOf(editor) !== text.length) { clearSuggestion(editor); return; }
    var stem = coveredBy(editor, item), name = nameOf(item);
    if (name.length > stem.length && name.indexOf(stem) === 0) showSuggestion(editor, name.slice(stem.length));
    else clearSuggestion(editor);
  }

  // Paint the editor with the decoration's tokens: as they are when they are of the text as it
  // stands, rebased through the edits since when they are of an older version, and only if
  // they then spell out the text exactly. Its state gives the Enter verdict for that version,
  // which sets the field's class when the version is the current one. Nothing is repainted
  // in the middle of an IME composition, which a repaint would break off.
  function decorate(editor, decoration) {
    var state = decoration.querySelector(".pyro-state");
    var given = state ? parseInt(state.getAttribute("data-version"), 10) || 0 : 0;
    var current = versionOf(editor);
    var tokens = decoration.querySelector(".pyro-tokens");
    if (tokens && !composing[editor.id]) {
      var items = itemsOf(tokens);
      if (given > 0 && given < current) {
        var since = logs[editor.id].filter(function (entry) { return entry.version > given; });
        if (since.length !== current - given) items = null;
        else since.forEach(function (entry) { items = rebaseItems(items, entry.splice); });
      }
      if (items && items.map(function (item) { return item.text; }).join("") === editorText(editor)) {
        var caret = caretOf(editor);
        clearSuggestion(editor);
        paintItems(editor, items);
        trailingBreak(editor);
        placeCaret(editor, caret);
      }
    }
    if (state) {
      var incomplete = state.getAttribute("data-incomplete") === "true";
      verdicts[editor.id] = { version: given, incomplete: incomplete };
      if (given === 0 || given === current) editor.classList.toggle("pyro-incomplete", incomplete);
    }
    var items = completionsOf(editor);
    items.forEach(function (item) {
      item.addEventListener("mousedown", function (event) { event.preventDefault(); accept(editor, item); });
    });
    suggest(editor);
    emptiness(editor);
  }

  function edited(editor) {
    dismissed[editor.id] = false;
    clearSuggestion(editor);
    trailingBreak(editor);
    emptiness(editor);
    var version = record(editor);
    send({ kind: "edit", id: editor.id, text: editorText(editor), caret: caretOf(editor), index: 0, version: version });
  }

  function setText(editor, text, caret) {
    clearSuggestion(editor);
    editor.textContent = text;
    trailingBreak(editor);
    placeCaret(editor, caret);
    edited(editor);
  }

  // Replace what the candidate covers (the whole text, or the identifier before the caret).
  function accept(editor, item, name) {
    name = name || nameOf(item);
    var text = editorText(editor), caret = caretOf(editor);
    if (item && item.classList.contains("pyro-replacement")) { setText(editor, name, name.length); return; }
    var start = caret;
    while (start > 0 && isIdentifier(text.charAt(start - 1))) start--;
    setText(editor, text.slice(0, start) + name + text.slice(caret), start + name.length);
  }

  function commonPrefix(names) {
    if (!names.length) return "";
    var prefix = names[0];
    for (var i = 1; i < names.length; i++) {
      var j = 0;
      while (j < prefix.length && j < names[i].length && prefix.charAt(j) === names[i].charAt(j)) j++;
      prefix = prefix.slice(0, j);
    }
    return prefix;
  }

  // Tab: a lone candidate is accepted; several first extend the text to their longest common
  // prefix, when longer than what they cover, and otherwise cycle the selection.
  function complete(editor) {
    var items = completionsOf(editor);
    // No candidates yet: the application hears the Tab and may supply some.
    if (!items.length) { send({ kind: "key", id: "", text: "[⇥]", caret: 0, index: 0 }); return; }
    if (items.length === 1) { accept(editor, items[0]); return; }
    var prefix = commonPrefix(items.map(nameOf)), covered = coveredBy(editor, items[0]);
    if (prefix.length > covered.length && prefix.indexOf(covered) === 0) accept(editor, items[0], prefix);
    else if (!selectedCompletion(editor)) moveCompletion(editor, 1);
    else {
      // Cycle: the last selected wraps to the first.
      var index = items.indexOf(selectedCompletion(editor));
      items[index].classList.remove("pyro-selected");
      items[(index + 1) % items.length].classList.add("pyro-selected");
      suggest(editor);
    }
  }

  // Enter: continue the line or submit it. The verdict is the server's, for the text as it
  // stands; when the latest edit's verdict has yet to arrive (a key pressed within a round
  // trip of the last), the server is asked, and keys pressed meanwhile wait their turn, so the
  // line is continued or submitted once, and never corrected afterwards.
  var pendingEnter = {}, queued = {};

  // For a verdict that never comes: open brackets, or a continued line whose last line is
  // not blank, read as unfinished.
  function looksIncomplete(text) {
    var depth = 0;
    for (var i = 0; i < text.length; i++) {
      var character = text.charAt(i);
      if ("([{".indexOf(character) >= 0) depth++; else if (")]}".indexOf(character) >= 0) depth--;
    }
    if (depth > 0) return true;
    var last = text.lastIndexOf("\n");
    return last >= 0 && text.slice(last + 1).trim() !== "";
  }

  function settle(editor, incomplete) { if (incomplete) newline(editor); else submit(editor); }

  function enter(editor) {
    var current = versionOf(editor), verdict = verdicts[editor.id];
    if (verdict && verdict.version === current) { settle(editor, verdict.incomplete); return; }
    queued[editor.id] = [];
    pendingEnter[editor.id] = setTimeout(function () { resolveEnter(editor, looksIncomplete(editorText(editor))); }, 500);
    send({ kind: "enter", id: editor.id, text: editorText(editor), caret: caretOf(editor), index: 0, version: current });
  }

  function resolveEnter(editor, incomplete) {
    if (!pendingEnter[editor.id]) return;
    clearTimeout(pendingEnter[editor.id]);
    delete pendingEnter[editor.id];
    settle(editor, incomplete);
    var replay = queued[editor.id] || [];
    queued[editor.id] = [];
    replay.forEach(function (key) {
      var text = editorText(editor), caret = caretOf(editor);
      if (key.key.length === 1) setText(editor, text.slice(0, caret) + key.key + text.slice(caret), caret + 1);
      else if (key.key === "Backspace") { if (caret > 0) setText(editor, text.slice(0, caret - 1) + text.slice(caret), caret - 1); }
      else editorKey(editor, { key: key.key, shiftKey: key.shiftKey, ctrlKey: false, altKey: false, metaKey: false, preventDefault: function () {} });
    });
  }

  // A newline, indented as the line being left when the caret ends it.
  function newline(editor) {
    var text = editorText(editor), caret = caretOf(editor);
    var rest = text.slice(caret), indent = "";
    if (rest === "" || rest.charAt(0) === "\n") {
      var line = text.slice(0, caret); line = line.slice(line.lastIndexOf("\n") + 1);
      indent = (line.match(/^[ \t]*/) || [""])[0];
    }
    setText(editor, text.slice(0, caret) + "\n" + indent + rest, caret + 1 + indent.length);
  }

  // The field's history is the application's: seeded in the page, replaced by `history` patches
  // when the application appends to it. Recall past the end is the draft.
  var histories = {};
  var recalls = {};

  function historyOf(editor) {
    if (!histories[editor.id]) {
      var seed = editor.dataset.history;
      try { histories[editor.id] = seed ? JSON.parse(seed) : []; } catch (error) { histories[editor.id] = []; }
      recalls[editor.id] = histories[editor.id].length;
    }
    return histories[editor.id];
  }

  // The clearing is an edit, versioned like any other, so the submission says which version
  // the empty field is at, and anything still in flight for the text is overtaken.
  function submit(editor) {
    var text = editorText(editor);
    recalls[editor.id] = historyOf(editor).length + (text === "" ? 0 : 1);
    clearSuggestion(editor);
    editor.textContent = "";
    emptiness(editor);
    var version = record(editor);
    send({ kind: "submit", id: editor.id, text: text, caret: 0, index: 0, version: version });
  }

  function editorKey(editor, event) {
    var items = completionsOf(editor);
    var multiline = editorText(editor).indexOf("\n") >= 0;

    // An Enter awaits its verdict: what is typed meanwhile is replayed after it.
    if (pendingEnter[editor.id]) {
      event.preventDefault();
      var plain = event.key.length === 1 && !event.ctrlKey && !event.altKey && !event.metaKey;
      if (plain || event.key === "Enter" || event.key === "Backspace" || event.key === "Tab") queued[editor.id].push({ key: event.key, shiftKey: event.shiftKey });
      return;
    }

    if (event.key === "Tab") {
      event.preventDefault();
      complete(editor);
    } else if (event.key === "Enter" && event.shiftKey) {
      event.preventDefault();
      newline(editor);
    } else if (event.key === "Enter") {
      event.preventDefault();
      if (selectedCompletion(editor) && hasSuggestion(editor)) accept(editor, selectedCompletion(editor));
      else enter(editor);
    } else if (event.key === "Escape" && items.length) {
      event.preventDefault(); dismiss(editor);
    } else if (event.key === "ArrowDown" && items.length) {
      event.preventDefault(); moveCompletion(editor, 1);
    } else if (event.key === "ArrowUp" && items.length && selectedCompletion(editor)) {
      event.preventDefault(); moveCompletion(editor, -1);
    } else if (event.key === "ArrowUp" && !multiline && recalls[editor.id] > 0) {
      event.preventDefault(); var history = historyOf(editor); recalls[editor.id]--;
      setText(editor, history[recalls[editor.id]], history[recalls[editor.id]].length);
    } else if (event.key === "ArrowDown" && !multiline && recalls[editor.id] < historyOf(editor).length) {
      event.preventDefault(); var history = historyOf(editor); recalls[editor.id]++;
      var recalled = recalls[editor.id] === history.length ? "" : history[recalls[editor.id]];
      setText(editor, recalled, recalled.length);
    } else if (event.key === "ArrowRight" && hasSuggestion(editor) && caretOf(editor) === editorText(editor).length) {
      event.preventDefault(); accept(editor, selectedCompletion(editor));
    }
  }

  // Actionable elements (rows, items, links, buttons) press their id, or the action they name;
  // fields edit and submit. A link to a tool of the server's machine names `localhost`, which
  // is right only when the page is viewed there: elsewhere it takes the page's own host.
  // The page's one clock, which the animated blocks with a frame at a time follow: at a time
  // t, such a block shows the last of its frames whose time is not after t (a frame without
  // a time follows its predecessor by that frame's hold). The bar — play, the clock and a
  // scrubber — is made when a timed block is first wired, and hidden when none remains;
  // space on the page toggles it, and stepping a block by hand pauses it. A block still
  // mid-step when its frame falls due is caught up on a later tick.
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
  // slide go before the next appears; an edit is not yet played, and is shown as a morph. The
  // slide's annotations fade out before the step and the target's fade in after it, with its
  // label and in its language. The result is the block's player, which the timeline drives.
  function animate(block) {
    var data;
    try { data = JSON.parse(block.querySelector(".pyro-animation-data").textContent); } catch (error) { console.error(error); return; }
    var code = block.querySelector("pre.pyro-codeblock > code");
    var counter = block.querySelector(".pyro-animation-counter");
    var prev = block.querySelector(".pyro-animation-prev");
    var next = block.querySelector(".pyro-animation-next");
    var play = block.querySelector(".pyro-animation-play");
    var label = block.querySelector(".pyro-animation-label");
    var pre = code.parentElement;
    var atoms = {};
    data.atoms.forEach(function (atom) { atoms[atom.id] = atom; });
    var nodes = {};
    code.querySelectorAll("[data-atom]").forEach(function (span) { nodes[span.dataset.atom] = span; });
    var frame = 0;
    var timer = null;
    var reduced = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

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
    // server renders them: the caret beneath its position, or a mark under each column, then
    // the caption.
    function markersOf(index, line, length) {
      var notes = (data.slides[index] && data.slides[index].notes) || [];
      var rows = [];
      notes.forEach(function (note) {
        var last = note.last === null ? note.line : note.last;
        var caret = note.last === null && note.start === note.end;
        if (last !== line || !(caret || note.caption)) return;
        var range = rangeOf(note, line, length) || [0, 0];
        var pad = caret ? Math.max(range[0] - 1, 0) : range[0];
        var marks = caret ? "╱╲" : "‾".repeat(Math.max(range[1] - range[0], 1));
        var row = document.createElement("span");
        row.className = "pyro-marker pyro-marker-" + note.style;
        row.appendChild(document.createTextNode(" ".repeat(pad)));
        var span = document.createElement("span");
        span.className = "pyro-marks";
        span.textContent = marks;
        row.appendChild(span);
        if (note.caption) {
          row.appendChild(document.createTextNode(" "));
          var caption = document.createElement("span");
          caption.innerHTML = note.caption;
          row.appendChild(caption);
        }
        rows.push(row);
      });
      return rows;
    }

    // The target frame's marks, label and language, applied: classes on the atoms its notes
    // cover, the marker rows and the label shown, the block's language class its own.
    function dress(index) {
      var covered = coverage(index);
      Object.keys(covered).forEach(function (id) {
        nodes[id].classList.add("pyro-marked", "pyro-marked-" + covered[id].style);
      });
      code.querySelectorAll(".pyro-marker").forEach(function (row) { row.style.transition = ""; row.style.opacity = ""; });
      if (label) { label.innerHTML = (data.slides[index] && data.slides[index].label) || ""; label.style.opacity = ""; }
      var language = (data.slides[index] && data.slides[index].language) || data.language;
      pre.className = pre.className.replace(/\bpyro-language-\S+/, "pyro-language-" + language);
      return Object.keys(covered).length > 0 || code.querySelector(".pyro-marker") !== null || (label && label.innerHTML !== "");
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

    function after(ms) { return new Promise(function (resolve) { setTimeout(resolve, ms); }); }
    function nextFrame() { return new Promise(function (resolve) { requestAnimationFrame(function () { requestAnimationFrame(resolve); }); }); }

    // One step, in three phases, each awaited before the next: the atoms the target frame
    // lacks fade out and are removed; the target's lines are then rebuilt from the surviving
    // nodes, with the arriving atoms in place but invisible, so that the survivors move
    // (FLIP, from measured geometry) into a layout that already leaves their gaps; and last
    // the arrivals fade in. A phase with nothing to do takes no time.
    function show(target, animated) {
      var moving = animated && !reduced;
      var lines = linesOf(target);
      var present = {};
      lines.forEach(function (ids) { ids.forEach(function (id) { present[id] = true; }); });
      var current = {};
      code.querySelectorAll("[data-atom]").forEach(function (span) { current[span.dataset.atom] = span; });

      // The transition says what departs and arrives; whatever the block holds beyond its
      // say (after a jump) goes or comes too. A fade takes everything out and brings
      // everything in; an edit is played as a morph for now.
      var transition = transitionFor(frame, target);
      var fading = transition.kind === "fade";
      var MOVE = transition.duration ? transition.duration * 1000 : MOVE_DEFAULT;
      var departing = transition.departing.filter(function (id) { return current[id]; });
      var arriving = transition.arriving.filter(function (id) { return present[id] && !current[id]; });
      Object.keys(current).forEach(function (id) { if (!present[id] && departing.indexOf(id) < 0) departing.push(id); });
      Object.keys(present).forEach(function (id) { if (!current[id] && arriving.indexOf(id) < 0) arriving.push(id); });
      if (fading) { departing = Object.keys(current); arriving = Object.keys(present); }

      // The target's lines, block-level, from the surviving nodes and the arriving atoms,
      // each followed by its marker rows — unseen when the step is animated, so that the
      // layout the survivors move into already leaves their room.
      function rebuild() {
        var fragment = document.createDocumentFragment();
        lines.forEach(function (ids, index) {
          var line = document.createElement("span");
          line.className = "pyro-line";
          var length = 0;
          ids.forEach(function (id) { line.appendChild(spanFor(id)); length += atoms[id].text.length; });
          fragment.appendChild(line);
          markersOf(target, index, length).forEach(function (row) {
            if (moving) { row.style.transition = "none"; row.style.opacity = "0"; }
            fragment.appendChild(row);
          });
        });
        code.replaceChildren(fragment);
      }

      function settle() {
        frame = target;
        busy = false;
        if (counter) counter.textContent = (frame + 1) + " / " + data.slides.length;
        if (prev) prev.disabled = frame === 0;
        if (next) next.disabled = frame === data.slides.length - 1;
      }

      if (!moving) {
        Object.keys(current).forEach(function (id) { current[id].className = current[id].className.replace(/ ?pyro-marked\S*/g, ""); });
        rebuild();
        Object.keys(present).forEach(function (id) { nodes[id].style.opacity = ""; nodes[id].style.transform = ""; });
        dress(target);
        settle();
        return Promise.resolve();
      }

      busy = true;

      // Phase 0: the frame's marks, marker rows and label fade out. The rows stay in place,
      // unseen, until the rebuild, so that their going is part of the measured move.
      var marked = code.querySelectorAll(".pyro-marked");
      var rows = code.querySelectorAll(".pyro-marker");
      var labelled = label && label.innerHTML !== "";
      var undress = Promise.resolve();
      if (marked.length || rows.length || labelled) {
        marked.forEach(function (span) { span.className = span.className.replace(/ ?pyro-marked\S*/g, ""); });
        rows.forEach(function (row) { row.style.opacity = "0"; });
        if (labelled) label.style.opacity = "0";
        undress = after(FADE);
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
        if (!fading) Object.keys(present).forEach(function (id) { if (current[id]) first[id] = current[id].getBoundingClientRect(); });
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
          var last = span.getBoundingClientRect();
          var dx = first[id].left - last.left, dy = first[id].top - last.top;
          if (Math.abs(dx) > 0.5 || Math.abs(dy) > 0.5) moves.push({ span: span, id: id, dx: dx, dy: dy });
        });
        if (!moves.length && !resized) return Promise.resolve();

        // Which line each surviving atom leaves and reaches, by line identity; a line is busy
        // when some atom stays on it from one frame to the next. A token joining a busy line
        // slides along its old row to its column first (H1) and then drops into its slot (V);
        // one reaching a new or emptied line drops first (V) and slides along the empty row
        // after (H2), with the tokens shifting within their own lines.
        var wasOn = lineOfAtoms(frame), nowOn = lineOfAtoms(target);
        var busy = {};
        Object.keys(nowOn).forEach(function (id) { if (wasOn[id] === nowOn[id]) busy[nowOn[id]] = true; });

        // A token shifting within its own line moves with the tokens that line exchanges: in
        // H1 when the line receives joiners, so that it makes room before they drop in, and
        // otherwise in H2, closing the gap once the leavers have gone.
        var receiving = {};
        moves.forEach(function (move) {
          if (wasOn[move.id] !== nowOn[move.id] && busy[nowOn[move.id]]) receiving[nowOn[move.id]] = true;
        });

        // `dy` is the token's displacement back to where it was: positive means it moves up.
        var h1 = [], v1 = [], v2 = [], h2 = [];
        moves.forEach(function (move) {
          var changesLine = wasOn[move.id] !== nowOn[move.id];
          var joining = changesLine && busy[nowOn[move.id]];
          var early = changesLine ? joining : receiving[nowOn[move.id]];
          if (Math.abs(move.dx) > 0.5) (early ? h1 : h2).push(move);
          if (Math.abs(move.dy) > 0.5) {
            var contracting = changesLine ? joining : move.dy > 0;
            (contracting ? v1 : v2).push(move);
          }
        });
        var shrinking = resized && after0 < before, growing = resized && after0 > before;

        // The schedule, as each part's start: the contracting vertical part STAGGER after H1
        // when both exist; the expanding vertical part halfway through the contracting one
        // (or after H1, where there is no contraction); H2 STAGGER after that. The move ends
        // with its last part.
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
        // Phase 4: the target's marks, marker rows and label fade in.
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

    function stop() { if (timer) { clearTimeout(timer); timer = null; } if (play) play.textContent = "▶"; }

    function playing() {
      var hold = (data.slides[frame] && data.slides[frame].hold) || 1.5;
      timer = setTimeout(function () {
        if (!step(1)) { stop(); return; }
        var wait = function () { if (busy) setTimeout(wait, 50); else if (timer) playing(); };
        wait();
      }, hold * 1000);
      if (play) play.textContent = "⏸";
    }

    function toggle() { if (timer) stop(); else { if (frame === data.slides.length - 1 && !data.loop) show(0, true); playing(); } }

    function manual(delta) { timeline.pause(); stop(); step(delta); }

    if (prev) prev.addEventListener("click", function () { manual(-1); });
    if (next) next.addEventListener("click", function () { manual(1); });
    if (play) play.addEventListener("click", function () { timeline.pause(); toggle(); });
    block.addEventListener("keydown", function (event) {
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
      show: show,
      stop: stop
    };
  }

  function wire(root) {
    root.querySelectorAll(".pyro-animated").forEach(function (block) {
      if (block.dataset.pyroWired) return;
      block.dataset.pyroWired = "1";
      var player = animate(block);
      if (player) timeline.register(player);
    });
    timeline.refresh();

    root.querySelectorAll("a[data-local='true']").forEach(function (link) {
      if (link.dataset.pyroWired) return;
      try {
        var url = new URL(link.getAttribute("href"), location.href);
        if (url.hostname === "localhost" && location.hostname !== "localhost") { url.hostname = location.hostname; link.setAttribute("href", url.toString()); }
      } catch (error) { console.error(error); }
    });

    // A plain left click goes through the session; any other (a middle click, a modifier) is
    // the browser's, from the anchor's real href.
    root.querySelectorAll("a.pyro-locate").forEach(function (link) {
      if (link.dataset.pyroWired) return;
      link.dataset.pyroWired = "1";
      link.addEventListener("click", function (event) {
        if (event.button !== 0 || event.metaKey || event.ctrlKey || event.shiftKey || event.altKey) return;
        if (navigate(link.getAttribute("href"), true)) event.preventDefault();
      });
    });

    root.querySelectorAll(".pyro-action, button.pyro-button").forEach(function (element) {
      if (element.dataset.pyroWired) return;
      element.dataset.pyroWired = "1";
      element.addEventListener("click", function (event) {
        event.preventDefault();
        send({ kind: "press", id: element.dataset.action || element.id, text: "", caret: 0, index: 0 });
      });
    });

    root.querySelectorAll(".pyro-editor").forEach(function (editor) {
      if (editor.dataset.pyroWired) return;
      editor.dataset.pyroWired = "1";
      emptiness(editor);
      verdicts[editor.id] = { version: 0, incomplete: editor.classList.contains("pyro-incomplete") };
      editor.addEventListener("input", function (event) { if (!event.isComposing) edited(editor); });
      editor.addEventListener("compositionstart", function () { composing[editor.id] = true; });
      editor.addEventListener("compositionend", function () { composing[editor.id] = false; edited(editor); });
      editor.addEventListener("keydown", function (event) { editorKey(editor, event); });
      editor.addEventListener("keyup", function (event) {
        if (event.key === "ArrowLeft" || event.key === "Home" || event.key === "End") suggest(editor);
      });
      var decoration = document.getElementById(editor.id + "-decoration");
      if (decoration) decorate(editor, decoration);
    });

    root.querySelectorAll("textarea.pyro-field").forEach(function (field) {
      if (field.dataset.pyroWired) return;
      field.dataset.pyroWired = "1";
      field.addEventListener("input", function () {
        send({ kind: "edit", id: field.id, text: field.value, caret: field.selectionStart || 0, index: 0 });
      });
      field.addEventListener("keydown", function (event) {
        if (event.key === "Enter" && !event.shiftKey) {
          if (!field.classList.contains("pyro-incomplete")) {
            event.preventDefault();
            send({ kind: "submit", id: field.id, text: field.value, caret: 0, index: 0 });
            field.value = "";
          }
        }
      });
    });

    root.querySelectorAll(".pyro-toggle").forEach(function (toggle) {
      if (toggle.dataset.pyroWired) return;
      toggle.dataset.pyroWired = "1";
      toggle.addEventListener("change", function () { send({ kind: "toggle", id: toggle.id, text: "", caret: 0, index: 0 }); });
    });

    root.querySelectorAll(".pyro-choice").forEach(function (choice) {
      if (choice.dataset.pyroWired) return;
      choice.dataset.pyroWired = "1";
      choice.addEventListener("change", function () {
        send({ kind: "choose", id: choice.id, text: "", caret: 0, index: parseInt(choice.value, 10) || 0 });
      });
    });
  }

  // Declared shortcuts arrive as keypresses in clavichord's rendering, e.g. "[⌃]+[L]"; only
  // chords with a modifier are reported, so ordinary typing is never intercepted.
  document.addEventListener("keydown", function (event) {
    if (event.key !== " " || !timeline.shown()) return;
    if (event.target && event.target.closest && event.target.closest(".pyro-animated, .pyro-editor, input, textarea, select, button, [contenteditable]")) return;
    timeline.toggle();
    event.preventDefault();
  });

  document.addEventListener("keydown", function (event) {
    if (!(event.ctrlKey || event.altKey || event.metaKey)) return;
    if (event.target && (event.target.tagName === "TEXTAREA" || event.target.tagName === "INPUT")) return;
    var pieces = [];
    if (event.metaKey) pieces.push("[⌘]");
    if (event.ctrlKey) pieces.push("[⌃]");
    if (event.altKey) pieces.push("[⌥]");
    if (event.shiftKey) pieces.push("[⇧]");
    var key = event.key.length === 1 ? "[" + event.key.toUpperCase() + "]" : null;
    if (!key) return;
    pieces.push(key);
    send({ kind: "key", id: "", text: pieces.join("+"), caret: 0, index: 0 });
  });

  wire(document);
  wireMode();
  var first = document.querySelector(".pyro-editor");
  if (first) first.focus({ preventScroll: true });
  connect();
  setInterval(function () { send({ kind: "ping", id: "", text: "", caret: 0, index: 0 }); }, 20000);
})();
