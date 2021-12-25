// Main test app

import * as ws       from "./ws.js";
import * as view     from "./view.js";
import * as protocol from "./protocol.js"
import * as tools    from "./tools.js"

// Expose modules to toplevel, e.g. for debugging on console.
window.mod = {ws, view, protocol, tools}

function zeros(nb) {
    // FIXME: This needs to parse the Arraybuffer instead.
    var arr = []
    for (var i=0; i<nb; i++) {
        arr.push(0);
    }
    return arr
}
function start() {
    view.wave({
        nb_channels: 8,
        arr_type: "uint8",
    }).then(el => {
        tools.set_cell("cell", el)
    });
}

ws.start()
start()



