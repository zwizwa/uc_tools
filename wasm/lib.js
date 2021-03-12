var s = [];
function s_app1(f)    { s.push(f(s.pop())); }
function s_mapp1(o,m) { s.push(o[m](s.pop())); }

var proto = ({'https:': 'wss://', 'http:': 'ws://'})[window.location.protocol];
var ws = new WebSocket(proto + location.host + '/ws');
ws.onclose   = function()    { console.log('ws: close'); }
ws.onopen    = function()    { console.log('ws: open'); }
ws.onmessage = function(raw_msg) {
    /* Just buffer it for now.  Much easier to work with. */
    raw_msg.data.arrayBuffer().then(
        arrayBuf => { read_from_array(new Uint8Array(arrayBuf)) })
}
function read_from_array(b) {
    var env = {i: 0, b: b};
    var msg = read(env);
    return msg;
}
var T_NOP = 0
var T_INT = 1
var T_TUP = 2
var T_BIN = 3
var T_ARR = 4
var T_SYM = 5
var T_TAG = 7 // not supported
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
    return read_byte(env) // FIXME
}
function read_tup(env) {
    var tup = [];
    var size = read_int(env);
    for (var i=0; i<size; i=i+1) { tup.push(read(env)); }
    return tup;
    
}
function read_arr(env) {
    var arr = [];
    var size = read_int(env);
    var type = read_int(env);
    for (var i=0; i<size; i=i+1) { arr.push(read_type(env, type)); }
    return arr;
}
function read_bin(env) {
    var size = read_int(env)
    return read_bytes(env, size)
}
function read_sym(env) {
    var bs = read_bin(env)
    return new TextDecoder().decode(bs)
}
function read_type(env, type) {
    if (type == T_INT) return read_int(env);
    if (type == T_TUP) return read_tup(env);
    if (type == T_ARR) return read_arr(env);
    if (type == T_BIN) return read_bin(env);
    if (type == T_SYM) return read_sym(env);
    //if (type == T_TAG) return read_tag(env);
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
