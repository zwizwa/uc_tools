// Shared behavior for browser view elements, as part of M-V-VM architecture.
// A view consists of two parts: a .html or .svg file containing the template.
// And a .js module referenced in that template to implement behavior.
//
// Letting the template refer to the behavior allows multiple layout
// templates share the same behavior, which seems more appropriate
// than the other way around.

function fetch_view(file, id, config) {
    return fetch_element(file)
        .then(el => {
            el.setAttribute("id", id)
            var module = el.getAttribute("module")
            return import('./' + module).then(m => m.init(el, config))
        })
}

// Fetch renderable object from server and render.
function fetch_element(file) {
    //var opts = {cache: "force-cache"};
    var opts = {}
    return fetch(file, opts)
        .then(r => r.text())
        .then(html => render(html))
}

// Render html in text form to DOM element.
function render(html) {
    var tmp = document.createElement('div');
    tmp.innerHTML=html;
    return tmp.firstChild;
}

// Widget instantiation
function wave(config) {
    return fetch_view('wave.svg', 'wave', config)
}


export { wave }
