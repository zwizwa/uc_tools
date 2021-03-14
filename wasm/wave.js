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
    var prev_y, path_d;
    tools.each(arr, function(y) {
        if (null == prev_y) {
            prev_y = y;
            path_d = 'M0,' + y;
        }
        path_d += 'l1,' + (y - prev_y);
        prev_y = y;
    });
    path.setAttribute('d',path_d);
}

const selectors = ["#min","#max"]
function path_handle(el, msg) {
    // Since JavaScript doesn't have pattern matching, we use a custom
    // "popper" that takes a number of tags off of the top of
    // msg.path, determined by the number of arguments of the
    // function, and then applies the function, essentially
    // implementing matching.
    // FIXME: msg.shift_apply()
    protocol.unpack(
        msg,
        function(path_nb) {
            //console.log('path_nb',path_nb)
            var sel  = check(selectors[path_nb])
            var path = check(el.querySelector(sel))
            // Interpret the binary payload as a signed 16-bit array.
            var arr  = msg.int16();
            path_set_d(path, arr)
        })
}

const dir_meta = [{name: "min"},{name: "max"}];

function handle(msg) {
    // console.log('handle',this)

    // The general idea of the path protocol is that the first tag in
    // the path determines where to send the message.  In the
    // JavaScript setting we are allowed to modify the tag in-place.
    var tag = msg.path.shift()
    switch(tag) {
        case -1: return protocol.dir(msg, dir_meta);
        case  0: return path_handle(this, msg)
    }
}

function init(el, env) {
    el.env = env
    el.handle = handle
    console.log("wave.js init")
    //console.log(el)
    //console.log(el.querySelector("#min"))
    return el; // chaining...
}

export { init, handle };
