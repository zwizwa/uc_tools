require ('lure.log')
local tab  = require('lure.tab')
local diff = require('lure.diff')
local m    = require('lure.lua_serialize')
local function run()
   local function eval(str)
      local f = loadstring("return " .. str)
      local v = f()
      -- log_desc({v=v})
      return v
   end
   -- TODO: table equality

   local function test(tree)
      local str = m.serialize(tree)
      log(str)
      local new_tree = eval(str)
      -- log_desc({old=tree,new=new_tree})
      local tree_diff = diff.diff(tree, new_tree)
      -- log_desc({tree_diff = tree_diff})
      assert(tab.is_empty(tree_diff))
   end


   test({a={b={c=123,d=456},e=789}})
   test({1,2,3,abc=123})
   test({a=0.123})
   -- test({def="abc"})  -- FIXME: String quoting
 
   -- error("debug_stop")
end

return { run = run }
