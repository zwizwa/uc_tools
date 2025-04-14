#!./lua.sh
require('lib.tools.log')
local cproc = require('lib.cproc')
function test1()
   -- cproc.test.render_map_cproc_struct() -- test broke
   cproc.test.parallel()
   cproc.test.serial()
   cproc.test.parallel_of_serial()

end
test1()
