// Main test app

import * as ws       from "./ws.js";
import * as view     from "./view.js";
import * as protocol from "./protocol.js"
import * as tools    from "./tools.js"

// Expose modules to toplevel, e.g. for debugging on console.
window.mod = {ws, view, protocol, tools}


ws.start()


