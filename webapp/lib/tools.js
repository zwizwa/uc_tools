function each(o, f) {
    var i;
    if (o.length !== undefined) {
        // Array
        for (i=0; i<o.length; i++) { f(o[i],i,o); }
        return o;
    }
    if (o.forEach !== undefined) {
        o.forEach(f);
        return o;
    }
    console.log("error: each", o, f);
}

function check(obj) {
    if ((obj == null) || (obj == undefined)) { throw obj; }
    return obj;
}

function set_cell(id, element) {
    let cell = document.getElementById(id)
    cell.innerHTML=""
    cell.appendChild(element)
}


export { each, check, set_cell };
