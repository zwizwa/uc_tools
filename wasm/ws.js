// WEBSOCKET

export function start() {
    var proto = ({'https:': 'wss://', 'http:': 'ws://'})[window.location.protocol];
    var ws = new WebSocket(proto + location.host + '/ws');
    ws.onclose   = function()    { console.log('ws: close'); }
    ws.onopen    = function()    { console.log('ws: open'); }
    ws.onmessage = function(raw_msg) {
        /* Just buffer it for now.  Much easier to work with. */
        raw_msg.data.arrayBuffer().then(
            arrayBuf => {
                var msg = read_from_array(new Uint8Array(arrayBuf));
                console.log(msg);
            })
    }
}
function read_from_array(b) {
    var env = {i: 0, b: b};
    var msg = read(env);
    return msg;
}
// type tags   // mnemonics
var T_NOP = 0  // n0p, padding
var T_INT = 1  // 1 = I
var T_TUP = 2  // 2ple
var T_BIN = 3  // 3 = B
var T_ARR = 4  // 4 = A
var T_SYM = 5  // 5 = S
var T_TAG = 7  // 7 = T
var T_CMT = 35 // '#'
function skip_comment(env) {
    while (10 == read_byte(env));
}
function read_byte(env) {
    if (null == env.b[env.i]) throw 'eof';
    var b = env.b[env.i];
    env.i = env.i + 1;
    return b;
}
function read_bytes(env, n) {
    var b = new Uint8Array(n);
    for(var i=0; i<n; i=i+1) b[i] = read_byte(env);
    return b;
}
function read_int(env) {
    var sr = 0;
    var shift = 0;
    for(;;) {
        var byte = read_byte(env);
        sr = sr | ((byte & 127) << shift);
        shift = shift + 7;
        if (!(byte & 128)) {
            // Last byte
            if (byte & 64) {
                // Signed
                return sr - (1 << shift);
            }
            else {
                return sr;
            }
        }
    }
}
function read_multi(n, rd) {
    var a = []
    for (var i=0; i<n; i=i+1) { a.push(rd()) }
    return a;
}
function read_tup(env) {
    var size = read_int(env);
    return read_multi(size, _ => read(env))
}
function read_arr(env) {
    var size = read_int(env);
    var type = read_int(env);
    return read_multi(size, _ => read_type(env, type))
}
function read_bin(env) {
    var size = read_int(env)
    return read_bytes(env, size)
}
function read_sym(env) {
    var bs = read_bin(env)
    return new TextDecoder().decode(bs)
}
function read_tag(env) {
    var nf = read_int(env); var from = read_multi(nf, _ => read_int(env));
    var nt = read_int(env); var to   = read_multi(nt, _ => read_int(env));
    var nb = read_int(env); var bin  = read_bytes(env, nb);
    return {from: from, to: to, bin: bin}
}
function read_type(env, type) {
    if (type == T_INT) return read_int(env);
    if (type == T_TUP) return read_tup(env);
    if (type == T_ARR) return read_arr(env);
    if (type == T_BIN) return read_bin(env);
    if (type == T_SYM) return read_sym(env);
    if (type == T_TAG) return read_tag(env);
    throw 'type error';
}
function read(env) {
    var type = read_int(env)
    if (type == T_NOP) return read(env);
    if (type == T_CMT) return skip_comment(env);
    return read_type(env, type)
}
function send(o) {
    ws.send(JSON.stringify(o));
}

