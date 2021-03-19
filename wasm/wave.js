// First ever GUI object.  Comments here serve a howto guide.
// Basic idea is that a GUI object (a view) consists of two parts:
//
// - XHTML / SVG file with initial rendering
//
// - That points here: to a module with behavior
//
// - The module exposes an init() method which is passed the DOM
//   element containing the initial DOM object.
//
// - The module exposes a handle() method, which takes TAG_U32 style
//   path-indexed mesages exposing render commands.

import * as tools    from './tools.js'
import * as protocol from './protocol.js'
import * as event    from './event.js'

// FIXME: Currently the websocket is a global entity, i.e. there is
// only one message sender.  Later it might be necessary to inject
// that dependency but for now that seems like unnecessary overhead.
import * as ws       from './ws.js'

// Catch null and undefined early.
var check = tools.check;

function path_set_d(path, arr, stride, offset) {
    var prev_y, path_d;
    for (var i=offset; i<arr.length; i+=stride) {
        var y = arr[i];
        if (prev_y == null) {
            prev_y = y;
            path_d = 'M0.5,' + y;
        }
        else {
            path_d += 'l1,' + (y - prev_y);
            prev_y = y;
        }
    }
    path.setAttribute('d',path_d);
}

const selectors = ["#min","#max"]
function path_handle(el, msg) {
    msg.unpack(
        status => {
            // The message is most likely generated as a response to a
            // GUI event, which would have been set up as an RPC
            // response with a single status code and a binary
            // payload.
            // console.log('status',status);

            // The binary payload contains interleaved min, max values
            // as uint16_t little endian.
            var arr  = msg.int16_le();

            path_set_d(check(el.querySelector("#min")), arr, 2, 0)
            path_set_d(check(el.querySelector("#max")), arr, 2, 1)

            // The convention is that we need to reply if the return
            // path is not empty.  We do expect it to be empty because
            // this is an RPC reply.
            if (msg.from.length > 0) {
                console.log('from',msg.from)
            }
        })
}

// FIXME
const dir_meta = [{name: "min"}, {name: "max"}];

function handle(msg) {
    msg.unpack(
        tag => {
            switch(tag) {
            case -1: return protocol.dir(msg, dir_meta);
            case  0: return path_handle(this, msg)
            }
        })
}
function rel_coords(ev) {
    // console.log(ev);
    var br = ev.target.env.background.getBoundingClientRect();
    // console.log(br);
    return {x: ev.clientX - br.left, w: br.width,
            y: ev.clientY - br.top,  h: br.height}
}

var mouse_listeners = {
    wheel: function(ev) {
        var win = rel_coords(ev);
        var level_inc = ev.deltaY > 0 ? 1 : -1;
        var msg = 
            new protocol.Message(
                [0], // FIXME: we need to know our own address
                [0, 0, win.w, win.h, win.x, level_inc]);
        ws.send(msg);
        //console.log(msg);
    },
    // This is for tracking the current object, which is useful in
    // combination with a global keydown handler.
    mouseover: event.mouseover,
    mouseout:  event.mouseout,

    mousedown: function(ev) {
        if (ev.buttons & 1) { // left button
            var win = rel_coords(ev);
            var level_inc = win.y > (win.h/2) ? 1 : -1;
            var msg = 
                new protocol.Message(
                    [0], // FIXME: we need to know our own address
                    [0, 0, win.w, win.h, win.x, level_inc]);
            ws.send(msg);
        }
        //console.log(ev.buttons);
        //console.log(ev, x, y);
    },
    keydown: function(ev) {
        console.log(ev);
    },
    contextmenu: function(ev) {
        // Turn off context menu
        ev.preventDefault();
    }
}
function add_listeners(el, listeners) {
    for (const key in listeners) {
        el.addEventListener(key, listeners[key])
    }
}
function init(el, env) {
    el.env = env
    el.handle = handle
    console.log("wave.js init")
    var env = {
        background: check(el.querySelector("#background"))
    };
    tools.each(
        // Events go to path elements or the background rect.
        ["#background","#min","#max"],
        sel => {
            var el1 = check(el.querySelector(sel))
            el1.env = env;
            add_listeners(el1, mouse_listeners);
        })
    return el; // chaining...
}

export { init, handle };
