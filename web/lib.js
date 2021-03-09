var s = [];
function s_app1(f)    { s.push(f(s.pop())); }
function s_mapp1(o,m) { s.push(o[m](s.pop())); }

var proto = ({'https:': 'wss://', 'http:': 'ws://'})[window.location.protocol];
var ws = new WebSocket(proto + location.host + '/ws');
ws.onclose   = function()    { console.log('ws: close'); }
ws.onopen    = function()    { console.log('ws: open'); }
ws.onmessage = function(msg) {
    console.log(msg);
    if (msg.data instanceof ArrayBuffer) { exec([['l',msg.data]]); }
    else { exec(JSON.parse(msg.data)); }
}
function send(o) {
    ws.send(JSON.stringify(o));
}
function render(html) {
    var tmp = document.createElement('div');
    tmp.innerHTML=html;
    return tmp.firstChild;
}
function insert(parent, child) {
    var c = parent.children;
    for(var i=0;i<c.length;i++) {
        if(child.id < c[i].id) {
            return parent.insertBefore(child,c[i]);
        }
    }
    parent.appendChild(child);
}
var s_op = {
    // This is taken from stack_ws.erl
    drop()      { s.pop(); },
    app1()      { s_app1(s.pop()); },
    mapp1()     { var m=s.pop(); s_mapp1(s.pop(),m); },
    print()     { console.log(s.pop()); },
    send()      { send(s.pop()); },
    ref()       { s_mapp1(document,'getElementById'); },
    replace()   { var old = s.pop(); old.parentNode.replaceChild(s.pop(),old); },
    body()      { document.body.innerHTML = s.pop(); },
    insert()    { var parent = s.pop(); insert(parent, s.pop()); },
    delete()    { var el = s.pop(); el.parentNode.removeChild(el); },
    render()    { s_app1(render); },
    words()     { var ws = []; for(var w in s_op) { ws.push(w); }; s.push(ws); },
    pathd()     { var path = s.pop(); path.setAttribute('d',s.pop()); },
    exec()      { exec(s.pop()); },
    tag()       { var t = s.pop(); var val = s.pop(); s.push([t, val]); },
    def()       { var d = s.pop(); s_op[d[0]] = function() { exec(d[1]); }; },
    // debug
    eval()      { s_app1(eval); },
    log()       { console.log(s); },
    clear()     { s = []; },
};
var m_op = {
    l(arg)  { s.push(arg); },
    s(name) { s_op[name](); },
};
function exec(prog) {
    for(var i=0;i<prog.length;i++) {
         var ins = prog[i];
         var opc = ins[0];
         var arg = ins[1];
         m_op[opc](arg);
    }
}
