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
const check = tools.check;


/* Scaling is done at this end.  The MinMax code that gets the time
   slice doesn't actually know how to interpret the data.  This is
   kept abstract so the same code can be used to process multichannel
   signals.  We do specialize numeric and logic drawing. */
var path_update = {
    /* Signed 16 bit integer display */
    int16_le: function(path, arr, chan_nb, chan_path /* 0=min, 1=max */) {
        const offset = chan_path + 2 * chan_nb;
        const env    = check(path.env);
        const config = check(env.config);
        const stride = check(config.nb_channels) * 2;
        const height = env.background.getBoundingClientRect().height;
        const chan_h = height / config.nb_channels;
        const scale  = chan_h / 0x10000;
        const mid    = (chan_h / 2) + (chan_nb * chan_h);

        let prev_y, path_d;
        for (let i=offset; i<arr.length; i+=stride) {
            const val = arr[i];
            const y = mid - (val * scale);

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
    },
    /* Bit channel display. */
    uint8: function(path, arr, chan_nb, chan_path /* 0=min, 1=max */) {
        const env     = check(path.env);
        const config  = check(env.config);
        const stride  = 2;
        const height  = env.background.getBoundingClientRect().height;
        const chan_h  = height / config.nb_channels;
        const mid     = (chan_h / 2) + (chan_nb * chan_h);
        const y0      = mid + (chan_h/4)
        const y1      = mid - (chan_h/4)

        let prev_y, path_d;
        for (let i=chan_path; i<arr.length; i+=stride) {
            const bit = (arr[i] >> chan_nb) & 1;
            const y = bit ? y1 : y0;

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

            // The binary payload can be interleaved min, max values
            // as uint16_t little endian (arr_type == "int16_le"), or
            // 8 bit logic array.
            const config = check(el.env.config);
            const arr_type = check(config.arr_type);
            // Interpret the binary array.
            const arr = msg[arr_type]();
            const update = path_update[arr_type];
            for (let chan_nb = 0; chan_nb < check(config.nb_channels); chan_nb++) {
                const outline_el = check(el.querySelector("#outline" + chan_nb));
                tools.each(
                    // chan_path: 0=min, 1=max.
                    // numbers are more convenient for computing offsets.
                    [0,1],
                    chan_path => {
                        const path_el = check(outline_el.querySelector(selectors[chan_path]));
                        update(path_el, arr, chan_nb, chan_path);
                    });
            }

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
function ev_rel_coords(ev) {
    // console.log(ev);
    const br = ev.target.env.background.getBoundingClientRect();
    // console.log(br);
    return {x: ev.clientX - br.left, w: br.width,
            y: ev.clientY - br.top,  h: br.height}
}

var mouse_listeners = {
    wheel: function(ev) {
        const win = ev_rel_coords(ev);
        const level_inc = ev.deltaY > 0 ? 1 : -1;
        const msg = 
            new protocol.Message(
                [0], // FIXME: we need to know our own address
                [0, 0, win.w, win.x, level_inc]);
        ws.send(msg);
        //console.log(msg);
    },
    // This is for tracking the current object, which is useful in
    // combination with a global keydown handler.
    mouseover: event.mouseover,
    mouseout:  event.mouseout,

    mousedown: function(ev) {
        if (ev.buttons & 1) { // left button
            const win = ev_rel_coords(ev);
            // FIXME: do this differently when there are multiple
            // channels.  or revise.  it's not very intuitive.
            const level_inc = win.y > (win.h/2) ? 1 : -1;
            const msg = 
                new protocol.Message(
                    [0], // FIXME: we need to know our own address
                    [0, 0, win.w, win.x, level_inc]);
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
function init(el, config) {
    // We are called from a module="wave.js" reference in the svg.
    // el is the SVG
    console.log("wave.js init");
    console.log(el);
    // The tag_u32 message handler.
    el.handle = handle;
    // JavaScript events are sent to specific nodes.  We tag them all
    // with the context they need to perform event handling.
    const env = {
        config: config,
        background: check(el.querySelector("#background"))
    };
    el.env = env;

    // Wire it it.
    function connect(el1) {
        el1.env = env;
        add_listeners(el1, mouse_listeners);
    }
    connect(env.background);
    function connect_outline(outline) {
        connect(check(outline.querySelector("#min")));
        connect(check(outline.querySelector("#max")));
    }
    const outline0 = check(el.querySelector("#outline0"));
    connect_outline(outline0);

    // SVG contains only one outline.  Add more if needed.
    for (let c=1; c<config.nb_channels; c++) {
        const outline = outline0.cloneNode(true); //deep
        outline.setAttribute("id", "outline" + c);
        connect_outline(outline);
        el.appendChild(outline);
    }
    return el; // chaining...
}

export { init, handle };
