// WEBSOCKET
import * as protocol from './protocol.js';

var ws;

function start() {
    var proto = ({'https:': 'wss://', 'http:': 'ws://'})[window.location.protocol];
    ws = new WebSocket(proto + location.host + '/ws');
    ws.onclose   = function()    { console.log('ws: close'); }
    ws.onopen    = function()    { console.log('ws: open'); }
    ws.onmessage = function(raw_msg) {
        /* Just buffer it for now.  Much easier to work with. */
        raw_msg.data.arrayBuffer().then(
            arrayBuf => {
                var msg = protocol.read_from_array(new Uint8Array(arrayBuf));
                console.log(msg);
                /* The protocol is the LEB128 based tree encoding.
                   However we currently only use this to transport
                   nested tag messadges (T_TAG), or array (T_ARR) of
                   T_TAG messages to indicate a transaction. */
            })
    }
}
function send(o) {
    ws.send(JSON.stringify(o));
}

export { start, send };

