#!./lua.sh
require('lure.log')
local run_cat = require('lib.run_cat')({
      script_dir = "cat/",
      -- plugin_dir = "cat_plugin/",
      read_cat = require('lib.read_cat'),
      log_desc = log_desc,
})

log_desc(cat)

local env = {}
local state = run_cat.new(env)

local function push(self, v)
   assert(v)
   table.insert(self.ds, v)
   return rv
end

local function test(str)
   log_desc(str)
   push(state, str)
   state:interpret_string()
end

test("1 2 3 ps")
test("1 2 + p")
test("test")  -- cat/test

