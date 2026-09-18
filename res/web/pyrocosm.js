// Pyrocosm's browser side: the only script any Pyrocosm application serves. It knows nothing
// about the application. It connects a WebSocket (to the page's own session, when the page names
// one), applies patches the server sends (replace an element's content, set a field's value,
// enable or check a control), and reports what the user does by the id of the element they did
// it to. Everything else is server-rendered HTML, including a code field's decoration: its
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
        element.innerHTML = patch.html;
        restoreDrawings(element, drawings);
        if (follow) element.scrollTop = element.scrollHeight;
        wire(element);
        if (/-decoration$/.test(patch.id)) {
          var editor = document.getElementById(patch.id.replace(/-decoration$/, ""));
          if (editor && editor.classList.contains("pyro-editor")) decorate(editor, element);
        }
        break;
      case "value":
        if (element.classList.contains("pyro-editor")) {
          if (editorText(element) !== patch.html) { clearSuggestion(element); element.textContent = patch.html; placeCaret(element, patch.html.length); emptiness(element); }
        } else if (element.value !== patch.html) element.value = patch.html;
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
    if (target) range.setStart(target, targetOffset);
    else { range.selectNodeContents(editor); range.collapse(false); }
    range.collapse(true);
    var selection = window.getSelection();
    selection.removeAllRanges();
    selection.addRange(range);
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

  // Paint the editor with the decoration's tokens, if they describe the text as it stands
  // (a newer keystroke has a fresh decoration in flight; stale tokens are dropped).
  function decorate(editor, decoration) {
    var tokens = decoration.querySelector(".pyro-tokens");
    if (tokens && tokens.textContent === editorText(editor)) {
      var caret = caretOf(editor);
      clearSuggestion(editor);
      editor.innerHTML = "";
      Array.prototype.forEach.call(tokens.childNodes, function (node) { editor.appendChild(node.cloneNode(true)); });
      placeCaret(editor, caret);
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
    emptiness(editor);
    send({ kind: "edit", id: editor.id, text: editorText(editor), caret: caretOf(editor), index: 0 });
  }

  function setText(editor, text, caret) {
    clearSuggestion(editor);
    editor.textContent = text;
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

  function incomplete(editor) {
    return editor.classList.contains("pyro-incomplete");
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

  function submit(editor) {
    var text = editorText(editor);
    recalls[editor.id] = historyOf(editor).length + (text === "" ? 0 : 1);
    send({ kind: "submit", id: editor.id, text: text, caret: 0, index: 0 });
    clearSuggestion(editor);
    editor.textContent = "";
    emptiness(editor);
  }

  function editorKey(editor, event) {
    var items = completionsOf(editor);
    var multiline = editorText(editor).indexOf("\n") >= 0;

    if (event.key === "Tab") {
      event.preventDefault();
      complete(editor);
    } else if (event.key === "Enter" && event.shiftKey) {
      event.preventDefault();
      newline(editor);
    } else if (event.key === "Enter") {
      event.preventDefault();
      if (selectedCompletion(editor) && hasSuggestion(editor)) accept(editor, selectedCompletion(editor));
      else if (incomplete(editor)) newline(editor);
      else submit(editor);
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

  // Actionable elements (rows, items, links, buttons) press their id; fields edit and submit.
  function wire(root) {
    root.querySelectorAll(".pyro-action, button.pyro-button").forEach(function (element) {
      if (element.dataset.pyroWired) return;
      element.dataset.pyroWired = "1";
      element.addEventListener("click", function (event) {
        event.preventDefault();
        send({ kind: "press", id: element.id, text: "", caret: 0, index: 0 });
      });
    });

    root.querySelectorAll(".pyro-editor").forEach(function (editor) {
      if (editor.dataset.pyroWired) return;
      editor.dataset.pyroWired = "1";
      emptiness(editor);
      editor.addEventListener("input", function () { edited(editor); });
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
  var first = document.querySelector(".pyro-editor");
  if (first) first.focus({ preventScroll: true });
  connect();
  setInterval(function () { send({ kind: "ping", id: "", text: "", caret: 0, index: 0 }); }, 20000);
})();
