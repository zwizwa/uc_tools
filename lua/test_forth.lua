#!./lua.sh
require('lure.log')
local forth = require('lib.forth')({
      script_dir = "forth/",
      -- plugin_dir = "forth_plugin/",
      read_cat = require('lib.read_cat'),
      log_desc = log_desc,
})

log_desc(forth)

local env = {}
local interpret, state = forth.new(env)

local function test(str)
   forth.interpret_line(
      str,
      function(word)
         log_desc(word)
         interpret(word)
      end)
end

test("1 2 3 ps")
test("1 2 + p")
test("test")  -- forth/test

