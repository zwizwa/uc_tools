#!./lua.sh

-- This is a parameterized module, i.e. the require statement returns
-- a function.
local lib_tools_path = require('lib.tools.path')
require('lure.log')

local function test1()
   local path = lib_tools_path({separator='_'})

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
end

local function test2()
   local path = lib_tools_path({separator='/'})
   local list = path.dotted_to_list("abc/def/ghi")
   log_desc({list=list})
end



-- test1()
test2()




