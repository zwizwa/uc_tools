import * as ws       from "./ws.js";
import * as view     from "./view.js";
import * as protocol from "./protocol.js"
ws.start()
// It seems simplest to just expose these to toplevel code by their
// module names.
window.ws = ws
window.view = view
window.protocol = protocol


// Expose some testing functionality to main (window) namespace.
// Set the main cell
function set_cell(element) {
    var cell = document.getElementById("cell")
    cell.innerHTML=""
    cell.appendChild(element)
}
// This is linked to the main button.
function test1() {
    view.wave({
        nb_channels: 2,
        // how to decode binary message into array
        arr_type: "int16_le",
        // for this type, the binary payload contains min, max samples
        // consecutively.
    }).then(el => {
        set_cell(el)
        var msg = new protocol.Message([123],[0,0])
        msg.int16_le = function() {
            // FIXME: This needs to parse the Arraybuffer instead.
            var arr = []
            for (var i=0; i<800; i++) {
                arr.push(100+100*Math.sin(i/30));
            }
            return arr
        }
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
        set_cell(el)
        var msg = new protocol.Message([123],[0,0])
        msg.int16_le = function() {
            // FIXME: This needs to parse the Arraybuffer instead.
            var arr = []
            for (var i=0; i<800; i++) {
                arr.push(100+100*Math.sin(i/30));
            }
            return arr
        }
        el.handle(msg)
    });
}


window.test = {
    test: test1,
    set_cell: set_cell,
}
