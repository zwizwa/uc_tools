import * as ws       from "./ws.js";
import * as view     from "./view.js";
import * as protocol from "./protocol.js"
import * as app      from "./app.js"
ws.start()

// Expose modules to toplevel, e.g. for debugging on console.
window.mod = {ws, view, protocol, app}

app.start()

