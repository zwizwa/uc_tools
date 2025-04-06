#!./lua.sh
require('lib.tools.log')
local cproc = require('lib.cproc')
function test1()
   -- cproc.bus_mapper(cproc, cproc.vec4, output)
   cproc.test.render_proc()
end
test1()
