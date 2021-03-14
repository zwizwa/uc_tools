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

// Catch null and undefined early.
var check = tools.check;

function path_set_d(path, arr) {
    var prev_y=arr[0]
    var path_d='M0,'+prev_y;
    tools.each(arr, function(y) {
        path_d += 'l1,' + (y - prev_y);
        prev_y = y;
    });
    path.setAttribute('d',path_d);
}

const selectors = ["#min","#max"]
function path_handle(el, msg) {
    msg.unpack(
        path_nb => {
            var sel  = check(selectors[path_nb])
            var path = check(el.querySelector(sel))
            // Interpret the binary payload as a signed 16-bit array.
            var arr  = msg.int16();
            path_set_d(path, arr)
            // The convention is that we need to reply if the return
            // path is not empty.
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

function init(el, env) {
    el.env = env
    el.handle = handle
    console.log("wave.js init")
    return el; // chaining...
}

export { init, handle };
