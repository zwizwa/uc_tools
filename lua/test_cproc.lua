#!./lua.sh
require('lib.tools.log')
local cproc = require('lib.cproc')
function test1()
   -- cproc.test.render_map_cproc_struct()
   --cproc.test.parallel()
   cproc.test.serial()
end
test1()
