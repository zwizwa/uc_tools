import * as ws       from "./ws.js";
import * as view     from "./view.js";
import * as protocol from "./protocol.js"
ws.start()
// It seems simplest to just expose these to toplevel code by their
// module names.
window.view = view
window.protocol = protocol



