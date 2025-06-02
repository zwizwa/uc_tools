var proto = ({'https:': 'wss://', 'http:': 'ws://'})[window.location.protocol];
var ws = new WebSocket(proto + location.host);
var enc = new TextEncoder();

function send(path, number) {
    var cmd = path + " " + number;
    console.log(cmd);
    ws.send(enc.encode(cmd));
}

function input_handler(Event) {
    var input  = Event.target;
    var id     = input.id;
    var number = input.valueAsNumber;
    send(id, number)
}

addEventListener("input", input_handler)

// send("/abc/def", 123.456)

// Store it in the window object for console access.
window.ws = ws
