// WEBSOCKET
import * as protocol from './protocol.js';

export function start() {
    var proto = ({'https:': 'wss://', 'http:': 'ws://'})[window.location.protocol];
    var ws = new WebSocket(proto + location.host + '/ws');
    ws.onclose   = function()    { console.log('ws: close'); }
    ws.onopen    = function()    { console.log('ws: open'); }
    ws.onmessage = function(raw_msg) {
        /* Just buffer it for now.  Much easier to work with. */
        raw_msg.data.arrayBuffer().then(
            arrayBuf => {
                var msg = protocol.read_from_array(new Uint8Array(arrayBuf));
                console.log(msg);
            })
    }
}
function send(o) {
    ws.send(JSON.stringify(o));
}

