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

local function test3()
   local path = lib_tools_path({separator='/'})
   local spec = {
      {true,  '*/abc', 'def/abc'},
      {false, '*/abc', 'def'},
      {false, '*/abc', 'def/abc/123'},
      {true,  'abc/*', 'abc/def'},
      {false, 'abc/*', 'abc'},
      {false, 'abc/*', 'abc/def/123'},
   }
   for i,tuple in pairs(spec) do
      local expected, wildcard_dotted, path_dotted = unpack(tuple)
      local match = path.match_wildcard_dotted(wildcard_dotted, path_dotted)
      log_desc({expected,match,wildcard_dotted, path_dotted})
      assert(expected == match)
   end
end


-- test1()
test2()


test3()

