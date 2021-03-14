// Protocol code.  This contains two parts: wire protocol (packed
// format based on LEB128 signed integers), and the unpacked "path"
// form, similar to TAG_U32 in C.

// 1. WIRE PROTOCOL

// Current implementation assumes the entire message can be buffered.
// We do the main parsing from an u8 view into the full buffer, and
// create other views as needed.
function read_from_arrayBuf(arrayBuf) {
    var env = {
        i: 0,
        ab: arrayBuf,
        u8: new Uint8Array(arrayBuf)
    };
    var msg = read(env);
    return msg;
}
function read_byte(env) {
    var b = env.u8[env.i];
    if (b == null) throw 'eof';
    env.i = env.i + 1;
    return b;
}
function read_bytes(env, n) {
    var ab_slice = env.ab.slice(env.i, env.i + n);
    env.i = env.i + n;
    return ab_slice;
}
// Note that endianness is the host endianness, which leads to code
// that is not portable.  We use this function as a patch point for
// later in case the issue ever pops up.
function int16_le(arrayBuf) {
    return new Int816Array(arrayBuf);
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
function read_int(env) {
    var sr = 0;
    var shift = 0;
    for(;;) {
        var b = read_byte(env);
        sr = sr | ((b & 127) << shift);
        shift = shift + 7;
        if (!(b & 128)) {
            // Last byte
            if (b & 64) {
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
    for (var i=0; i<n; i=i+1) { a.push(rd()); }
    return a;
}
function read_tup(env) {
    var size = read_int(env);
    return read_multi(size, _ => read(env));
}
function read_arr(env) {
    var size = read_int(env);
    var type = read_int(env);
    return read_multi(size, _ => read_type(env, type));
}
function read_bin(env) {
    var size = read_int(env)
    return read_bytes(env, size);
}
function read_sym(env) {
    var bs = read_bin(env)
    return new TextDecoder().decode(bs);
}
function read_tag(env) {
    var nf = read_int(env); var from = read_multi(nf, _ => read_int(env));
    var nt = read_int(env); var to   = read_multi(nt, _ => read_int(env));
    var nb = read_int(env); var bin  = read_bytes(env, nb);
    return new Message(from, to, bin);
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
    return read_type(env, type);
}




// 2. PATH FORM

class Message {
    constructor(from, to, bin) {
        this.from   = from,
        this.to     = to,
        this.bin    = bin,
        this.unpack = function(fun) { return unpack(this, fun); }
    }
}

// Apply a protocol message to a function.  This essentially
// implements pattern matching.
function unpack(msg, fun) {
    var args = []
    for(var i=0; i<fun.length; i=i+1) {
        var arg = msg.to.shift()
        args.push(arg)
    }
    return fun.apply(null, args)
}




export { read_from_arrayBuf, unpack, Message };
