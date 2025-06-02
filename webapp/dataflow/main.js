var proto = ({'https:': 'wss://', 'http:': 'ws://'})[window.location.protocol];
var ws = new WebSocket(proto + location.host);
var enc = new TextEncoder();

function send(path, number) {
    ws.send(enc.encode("asdf" + " " + number))
}

send("/abc/def", 123.456)

// Store it in the window object for console access.
window.ws = ws
