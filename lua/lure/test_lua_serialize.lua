require ('lure.log')

local tab  = require('lure.tab')
local diff = require('lure.diff')
local m    = require('lure.lua_serialize')
local tree = require('lure.tree')

local function run()
   local function eval(str)
      local f = loadstring("return " .. str)
      local v = f()
      -- log_desc({v=v})
      return v
   end
   -- TODO: table equality

   local function test_diff(a, b)
      log_desc({diff = diff.diff(a, b)})
   end


   local function test(obj)
      local str = m.serialize(obj)
      log(str)
      local new_obj = eval(str)
      assert(tree.is_equal(obj, new_obj))
   end

   -- test_diff({a=123},{b=1234})


   test({a={b={c=123,d=456},e=789}})
   test({1,2,3,abc=123})
   test({a=0.123})
   -- test({def="abc"})  -- FIXME: String quoting
   -- error("debug_stop")

   test()

end

return { run = run }
