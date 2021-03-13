import { start as ws_start } from "./ws.js";
ws_start()

// LIB
// Render html in text form to DOM element.
function render(html) {
    var tmp = document.createElement('div');
    tmp.innerHTML=html;
    return tmp.firstChild;
}
// Fetch renderable object from server and render.
function fetch_element(file) {
    return fetch(file)
        .then(r => r.text())
        .then(html => render(html))
}
function fetch_view(file, id) {
    fetch_element(file)
        .then(el => {
            //console.log(el)
            el.setAttribute("id", id)
            var module = el.getAttribute("module")
            import('./' + module).then(m => m.init(el))
            // svg.setAttribute("id", id)
            set_cell(el) // debug
        })
}
// Set the main cell
function set_cell(element) {
    var cell = document.getElementById("cell")
    cell.innerHTML=""
    cell.appendChild(element)
}


// VIEW OBJECTS
//
// Keep it simple. View objects are constructed from prototypes stored
// as regular html/svg files on the server.
export function wave(id) {
    fetch_view('wave.svg', id)
}


export function button() {
    wave("wave").then(wave => set_cell(wave))
}
