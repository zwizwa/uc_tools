// Shortcuts for starting different kinds of apps.

import * as view     from "./view.js";
import * as protocol from "./protocol.js";

function zeros(nb) {
    // FIXME: This needs to parse the Arraybuffer instead.
    var arr = []
    for (var i=0; i<nb; i++) {
        arr.push(0);
    }
    return arr
}
// This is linked to the main button.
function test1() {
    view.wave({
        nb_channels: 1,
        // how to decode binary message into array
        arr_type: "int16_le",
        // for this type, the binary payload contains min, max samples
        // consecutively.
    }).then(el => {
        view.set_cell(el)
        var msg = new protocol.Message([],[0,0])
        msg.int16_le = function() { return zeros(800 * 2); }
        el.handle(msg)
    });
}
function test2() {
    view.wave({
        nb_channels: 8,
        // how to decode binary message into array
        arr_type: "uint8",
        // for this type, the binary payload contains min, max samples
        // in consecutive bytes, and each byte contains 8 logic channels.
    }).then(el => {
        view.set_cell(el)
        var msg = new protocol.Message([],[0,0])
        msg.int16_le = function() { return zeros(800 * 2); }
        el.handle(msg)
    });
}
export {test1, test2}


