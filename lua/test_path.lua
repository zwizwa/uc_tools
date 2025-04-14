#!./lua.sh
local path = require('lib.tools.path')({separator='_'})
require('lib.tools.log')

local tree = {
   a = {
      c = 123
   },
   b = {
      d = 456
   },
   e = 789
}

log_desc(path.nested_to_flat(tree))
