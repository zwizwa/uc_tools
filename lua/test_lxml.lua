#!./lua.sh
local lxml = require('lib.lxml')

local function t(e)
   io.stderr:write(lxml.elements_to_string({e}) .. "\n")
end
t({'div',{},{}})
t({'div',{a='abc',b=123},{}})
t({'h1',{},{'Hello'}})


