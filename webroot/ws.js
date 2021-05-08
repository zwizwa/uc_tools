// WEBSOCKET
import * as protocol from './protocol.js';
import * as tools    from './tools.js';
var check = tools.check;

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
                var msg = protocol.from_arrayBuffer(arrayBuf);
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
    // console.log('dispatch',msg);
    msg.unpack(
        tag => {
            // FIXME: map tag to instance
            var el = check(document.getElementById("wave"));
            check(el.handle);
            el.handle(msg);
        })
}
function send(o) {
    ws.send(protocol.to_arrayBuffer(o))
}

function test() {
    send({to:[0,0,800,200,0,1],from:[0],bin:[]})
}

export { start, send, test };

