#!/usr/bin/lua

-- Been thinking for a while on how to implement HOAS tricks in Lua.
-- The key element here is that it is probably best to use some kind
-- of surface syntax (e.g. s-expressions), and then convert that
-- surface syntax to HOAS.  It will probably not be a good idea to
-- write programs in HOAS directly.

local function example_program(s)
   local f = s:lambda(function(x) return s:add(x,1) end)
   return s:app(f, 2)
end

local se = require('lib.se')
local l = se.list

local example_sexpr =
   l('let*',
     l(l('f',l('lambda',l('x'),l('add','x',1)))),
     l('f',2))


-- A Scheme syntax to Lua HOAS converter is more like a Lua to Scheme
-- compiler.  So maybe start there instead?

