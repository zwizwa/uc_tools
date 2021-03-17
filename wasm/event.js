/* To handle keydown events for objects that do not support them, this
   workaround can be used.  To use: add the mouseover/mouseout
   handlers from this module, which will track the current object.
   Then the keydown event listener for document.body can be used to
   deliver emulated keydown events to those objects. */

import * as debug from './debug.js'

var current = null;
function mouseover(ev) {
    if (current != ev.target) {
        current = ev.target;
        //console.log('entering',current)
    }
}
function mouseout(ev)  {
    //console.log('leaving',current)
    current = null;
}
function keydown(ev) {
    debug.innerHTML(ev.key)
    console.log(ev, current);
}
document.body.addEventListener("keydown",keydown);

export { mouseover, mouseout, current }
