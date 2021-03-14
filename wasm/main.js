import * as ws       from "./ws.js";
import * as view     from "./view.js";
import * as protocol from "./protocol.js"
ws.start()
// It seems simplest to just expose these to toplevel code by their
// module names.
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
function test() {
    view.wave().then(
        el => {
            set_cell(el)
            var msg = {
                path: [0,0]
            }
            msg.int16 = function() {
                // FIXME: This needs to parse the Arraybuffer instead.
                var arr = []
                for (var i=0; i<800; i++) {
                    arr.push(100+100*Math.sin(i/30));
                }
                return arr
            }
            el.handle(msg)
        }
    )
}
window.test = {
    test: test,
    set_cell: set_cell,
}
