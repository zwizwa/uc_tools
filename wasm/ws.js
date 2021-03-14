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
                var msg = protocol.read_from_arrayBuf(arrayBuf);
                /* The protocol is the LEB128 based tree encoding.
                   However we currently only use this to transport
                   nested tag messadges (T_TAG), or array (T_ARR) of
                   T_TAG messages to indicate a transaction. */
                if (msg.to) {
                    dispatch(msg)
                }
                else {
                    console.log('ws: unknown',msg);
                }
            })
    }
}
/* The convention is that the dispatch chain owns the message and can
   modify it in place before passing it on. */
function dispatch(msg) {
    console.log('dispatch',msg);
}
function send(o) {
    ws.send(JSON.stringify(o));
}

export { start, send };

