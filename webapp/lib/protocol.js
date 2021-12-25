// Protocol code.  This contains two parts: wire protocol (packed
// format based on LEB128 signed integers), and the unpacked "path"
// form, similar to TAG_U32 in C.

import * as tools    from './tools.js';

// 1. WIRE PROTOCOL

// 1.1 READ

// Current implementation assumes the entire message can be buffered.
// We do the main parsing from an u8 view into the full buffer, and
// create other views as needed.
function from_arrayBuffer(arrayBuf) {
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
    var a = [];
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


// 1.2 WRITE

// Use the same approach as for read: an ArrayBuffer.  For now, assume
// the buffer is big enough.  FIXME: Later, grow the buffer when
// necessary.
function write_byte(env, b) {
    env.u8[env.i] = b;
    env.i += 1;
}
function write_bytes(env, bufView) {
    /* We assume that bufView.buffer is defined, e.g. that bufView is
       an ArrayBuffer view such as UintArray. */
    var buf = new Uint8Array(bufView.buffer);
    for(var i=0; i<buf.length; i+=1) {
        env.u8[env.i] = buf[i];
        env.i += 1;
    }
}
function write_int(env, n) {
    for(;;) {
        if ((n > 63) || (n < -64)) {
            write_byte(env, 128 | (n & 127));
            n >>= 7;
        }
        else {
            write_byte(env, n & 127)
            return;
        }
    }
}
function write_tag(env, msg) {
    write_int(env, msg.from.length); tools.each(msg.from, i => write_int(env, i));
    write_int(env, msg.to.length);   tools.each(msg.to,   i => write_int(env, i));
    if (null == msg.bin) {
        write_int(env, 0);
    }
    else {
        write_int(env, msg.bin.length);
        write_bytes(env, msg.bin);
    }
}
function term_type(term) {
    if ("number" == typeof(term)) return T_INT; // no floats atm
    if (term.to != null) return T_TAG;
    return T_INT;
}
function write_type(env, type, term) {
    if (type == T_INT) { return write_int(env, term); }
    if (type == T_TAG) { return write_tag(env, term); }
    throw 'type error';
}
function write(env, term) {
    var type = term_type(term);
    write_int(env, type);
    write_type(env, type, term);
}
function to_arrayBuffer(term) {
    var ab = new ArrayBuffer(1000); // FIXME
    var env = {
        ab: ab,
        u8: new Uint8Array(ab),
        i: 0
    }
    write(env, term);
    return ab.slice(0,env.i);
}




// 2. PATH FORM

class Message {
    constructor(from, to, bin) {
        this.from   = from,
        this.to     = to,
        this.bin    = bin,
        this.unpack = function(fun) { return unpack(this, fun); }
    }
    // FIXME: Int16Array's endianness is implementation dependent.  We
    // do not need this yet as all practical cases use little endian,
    // but at the very least leave a patch point for later.
    int16_le() {
        var a = new Int16Array(this.bin);
        // console.log(a);
        return a;
    }
    uint8() {
        var a = new Uint8Array(this.bin);
        // console.log(a);
        return a;
    }
}

// Apply a protocol message to a function, consuming the top address
// tags.  This essentially implements pattern matching.
function unpack(msg, fun) {
    var args = []
    for(var i=0; i<fun.length; i=i+1) {
        var arg = msg.to.shift()
        args.push(arg)
    }
    return fun.apply(null, args)
}




export { from_arrayBuffer, to_arrayBuffer, unpack, Message };
