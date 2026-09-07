// Pyrocosm's browser side: the only script any Pyrocosm application serves. It knows nothing
// about the application. It connects a WebSocket, applies patches the server sends (replace an
// element's content, set a field's value, enable or check a control), and reports what the user
// does by the id of the element they did it to. Everything else is server-rendered HTML.
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

  function apply(patch) {
    var element = document.getElementById(patch.id);
    if (!element) return;
    switch (patch.kind) {
      case "replace":
        var follow = element.scrollHeight - element.scrollTop - element.clientHeight < 8;
        element.innerHTML = patch.html;
        if (follow) element.scrollTop = element.scrollHeight;
        wire(element);
        break;
      case "value": if (element.value !== patch.html) element.value = patch.html; break;
      case "enable": element.disabled = false; break;
      case "disable": element.disabled = true; break;
      case "check": element.checked = true; break;
      case "uncheck": element.checked = false; break;
      case "select": element.value = patch.html; break;
    }
  }

  function connect() {
    var protocol = location.protocol === "https:" ? "wss://" : "ws://";
    socket = new WebSocket(protocol + location.host + "/socket");
    socket.onopen = function () { status(true); };
    socket.onclose = function () { status(false); setTimeout(connect, 1000); };
    socket.onmessage = function (event) {
      try { apply(JSON.parse(event.data)); } catch (error) { console.error(error); }
    };
  }

  // Actionable elements (rows, items, links, buttons) press their id; fields edit and submit.
  function wire(root) {
    root.querySelectorAll(".pyro-action, .pyro-press").forEach(function (element) {
      if (element.dataset.pyroWired) return;
      element.dataset.pyroWired = "1";
      element.addEventListener("click", function (event) {
        event.preventDefault();
        send({ kind: "press", id: element.id });
      });
    });


    root.querySelectorAll(".pyro-field").forEach(function (field) {
      if (field.dataset.pyroWired) return;
      field.dataset.pyroWired = "1";
      field.addEventListener("input", function () {
        send({ kind: "edit", id: field.id, text: field.value, caret: field.selectionStart || 0 });
      });
      field.addEventListener("keydown", function (event) {
        if (event.key === "Enter" && !event.shiftKey) {
          var decoration = document.getElementById(field.id + "-decoration");
          var incomplete = decoration && decoration.querySelector(".pyro-incomplete");
          if (!incomplete) {
            event.preventDefault();
            send({ kind: "submit", id: field.id, text: field.value });
            field.value = "";
          }
        }
      });
    });

    root.querySelectorAll(".pyro-toggle").forEach(function (toggle) {
      if (toggle.dataset.pyroWired) return;
      toggle.dataset.pyroWired = "1";
      toggle.addEventListener("change", function () { send({ kind: "toggle", id: toggle.id }); });
    });

    root.querySelectorAll(".pyro-choice").forEach(function (choice) {
      if (choice.dataset.pyroWired) return;
      choice.dataset.pyroWired = "1";
      choice.addEventListener("change", function () {
        send({ kind: "choose", id: choice.id, index: parseInt(choice.value, 10) || 0 });
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
    send({ kind: "key", text: pieces.join("+") });
  });

  wire(document);
  connect();
})();
