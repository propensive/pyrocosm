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
    indicator.className = "pyro-connection " + (online ? "pyro-online" : "pyro-offline");
  }

  function send(message) {
    if (socket && socket.readyState === WebSocket.OPEN) socket.send(JSON.stringify(message));
  }

  function sessionQuery() {
    var meta = document.querySelector('meta[name="pyro-session"]');
    return meta ? "?session=" + encodeURIComponent(meta.getAttribute("content")) : "";
  }

  function apply(patch) {
    var element = document.getElementById(patch.id);
    if (!element) return;
    switch (patch.kind) {
      case "replace":
        var follow = element.scrollHeight - element.scrollTop - element.clientHeight < 8;
        element.innerHTML = patch.html;
        if (follow) element.scrollTop = element.scrollHeight;
        wire(element);
        if (/-decoration$/.test(patch.id)) {
          var editor = document.getElementById(patch.id.replace(/-decoration$/, ""));
          if (editor && editor.classList.contains("pyro-editor")) decorate(editor, element);
        }
        break;
      case "value":
        if (element.classList.contains("pyro-editor")) {
          if (editorText(element) !== patch.html) { clearGhost(element); element.textContent = patch.html; placeCaret(element, patch.html.length); emptiness(element); }
        } else if (element.value !== patch.html) element.value = patch.html;
        break;
      case "enable": element.disabled = false; break;
      case "disable": element.disabled = true; break;
      case "check": element.checked = true; break;
      case "uncheck": element.checked = false; break;
      case "select": element.value = patch.html; break;
    }
  }

  function connect() {
    var protocol = location.protocol === "https:" ? "wss://" : "ws://";
    socket = new WebSocket(protocol + location.host + "/socket" + sessionQuery());
    socket.onopen = function () { status(true); };
    socket.onclose = function () { status(false); setTimeout(connect, 1000); };
    socket.onmessage = function (event) {
      try { apply(JSON.parse(event.data)); } catch (error) { console.error(error); }
    };
  }

  // ── The code editor ──────────────────────────────────────────────────────────────────────
  // A contenteditable element painted with the server's tokens. The caret is kept as a text
  // offset across repaints; ghost text, the remainder of the selected completion, is a
  // non-editable span at the end, never part of the text; the completions list beneath is
  // navigated with the arrows.

  function ghostOf(editor) { return editor.querySelector(".pyro-ghost"); }

  function editorText(editor) {
    var ghost = ghostOf(editor);
    return ghost ? editor.textContent.slice(0, editor.textContent.length - ghost.textContent.length) : editor.textContent;
  }

  function clearGhost(editor) { var ghost = ghostOf(editor); if (ghost) ghost.remove(); }

  function showGhost(editor, text) {
    clearGhost(editor);
    if (!text) return;
    var span = document.createElement("span");
    span.className = "pyro-ghost";
    span.contentEditable = "false";
    span.textContent = text;
    editor.appendChild(span);
  }

  function hasGhost(editor) { return !!ghostOf(editor); }

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
      if (node.parentNode && node.parentNode.classList && node.parentNode.classList.contains("pyro-ghost")) continue;
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
    if (holder && holder.classList.contains("pyro-field-holder"))
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
    return decoration ? Array.prototype.slice.call(decoration.querySelectorAll(".pyro-completions li")) : [];
  }

  function selectedCompletion(editor) {
    var items = completionsOf(editor);
    for (var i = 0; i < items.length; i++) if (items[i].classList.contains("pyro-selected")) return items[i];
    return items[0] || null;
  }

  function moveCompletion(editor, delta) {
    var items = completionsOf(editor);
    if (!items.length) return false;
    var index = 0;
    for (var i = 0; i < items.length; i++) if (items[i].classList.contains("pyro-selected")) index = i;
    items[index].classList.remove("pyro-selected");
    index = (index + delta + items.length) % items.length;
    items[index].classList.add("pyro-selected");
    ghost(editor);
    return true;
  }

  function nameOf(item) { var code = item.querySelector("code"); return code ? code.textContent : item.textContent; }

  // The ghost: what the selected completion adds to the identifier before the caret, when
  // the caret ends the text.
  function ghost(editor) {
    var item = selectedCompletion(editor);
    var text = editorText(editor);
    if (!item || caretOf(editor) !== text.length) { clearGhost(editor); return; }
    var stem = stemOf(editor), name = nameOf(item);
    if (name.length > stem.length && name.indexOf(stem) === 0) showGhost(editor, name.slice(stem.length));
    else clearGhost(editor);
  }

  // Paint the editor with the decoration's tokens, if they describe the text as it stands
  // (a newer keystroke has a fresh decoration in flight; stale tokens are dropped).
  function decorate(editor, decoration) {
    var tokens = decoration.querySelector(".pyro-tokens");
    if (tokens && tokens.textContent === editorText(editor)) {
      var caret = caretOf(editor);
      clearGhost(editor);
      editor.innerHTML = "";
      Array.prototype.forEach.call(tokens.childNodes, function (node) { editor.appendChild(node.cloneNode(true)); });
      placeCaret(editor, caret);
    }
    var items = completionsOf(editor);
    if (items.length) items[0].classList.add("pyro-selected");
    items.forEach(function (item) {
      item.addEventListener("mousedown", function (event) { event.preventDefault(); accept(editor, nameOf(item)); });
    });
    ghost(editor);
    emptiness(editor);
  }

  function edited(editor) {
    clearGhost(editor);
    emptiness(editor);
    send({ kind: "edit", id: editor.id, text: editorText(editor), caret: caretOf(editor), index: 0 });
  }

  function setText(editor, text, caret) {
    clearGhost(editor);
    editor.textContent = text;
    placeCaret(editor, caret);
    edited(editor);
  }

  // Replace the identifier before the caret with a completion.
  function accept(editor, name) {
    var text = editorText(editor), caret = caretOf(editor), start = caret;
    while (start > 0 && isIdentifier(text.charAt(start - 1))) start--;
    setText(editor, text.slice(0, start) + name + text.slice(caret), start + name.length);
  }

  function incomplete(editor) {
    var decoration = document.getElementById(editor.id + "-decoration");
    return !!(decoration && decoration.querySelector(".pyro-incomplete"));
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

  var history = [];
  var recall = 0;

  function submit(editor) {
    var text = editorText(editor);
    if (text !== "") { history.push(text); }
    recall = history.length;
    send({ kind: "submit", id: editor.id, text: text, caret: 0, index: 0 });
    clearGhost(editor);
    editor.textContent = "";
    emptiness(editor);
  }

  function editorKey(editor, event) {
    var items = completionsOf(editor);
    var multiline = editorText(editor).indexOf("\n") >= 0;

    if (event.key === "Tab") {
      event.preventDefault();
      var item = selectedCompletion(editor);
      if (item) accept(editor, nameOf(item));
    } else if (event.key === "Enter" && event.shiftKey) {
      event.preventDefault();
      newline(editor);
    } else if (event.key === "Enter") {
      event.preventDefault();
      if (hasGhost(editor)) accept(editor, nameOf(selectedCompletion(editor)));
      else if (incomplete(editor)) newline(editor);
      else submit(editor);
    } else if (event.key === "ArrowDown" && items.length) {
      event.preventDefault(); moveCompletion(editor, 1);
    } else if (event.key === "ArrowUp" && items.length) {
      event.preventDefault(); moveCompletion(editor, -1);
    } else if (event.key === "ArrowUp" && !multiline && recall > 0) {
      event.preventDefault(); recall--; setText(editor, history[recall], history[recall].length);
    } else if (event.key === "ArrowDown" && !multiline && recall < history.length) {
      event.preventDefault(); recall++; setText(editor, recall === history.length ? "" : history[recall], recall === history.length ? 0 : history[recall].length);
    } else if (event.key === "ArrowRight" && hasGhost(editor) && caretOf(editor) === editorText(editor).length) {
      event.preventDefault(); accept(editor, nameOf(selectedCompletion(editor)));
    }
  }

  // Actionable elements (rows, items, links, buttons) press their id; fields edit and submit.
  function wire(root) {
    root.querySelectorAll(".pyro-action, .pyro-press").forEach(function (element) {
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
        if (event.key === "ArrowLeft" || event.key === "Home" || event.key === "End") ghost(editor);
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
          var decoration = document.getElementById(field.id + "-decoration");
          var incomplete = decoration && decoration.querySelector(".pyro-incomplete");
          if (!incomplete) {
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
  if (first) first.focus();
  connect();
})();
