-- Write Lua modules in Scheme.  See lib_scm.lua for a usage example.
local se   = require('lure.se')
local slc2 = require('lure.slc2')

function compile_module(str)
   local exprs = se.read_string_multi(str) ; assert(exprs)
   local lua = slc2.new():compile({'module-begin',exprs})
   return slc2.eval(lua)
end

return compile_module
