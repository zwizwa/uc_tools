#!/usr/bin/lua

local prompt = require('prompt')
local function log(str) io.stderr:write(str) end
local function log_desc(thing) log(prompt.describe(thing)) end

log("test_dfl.lua begin\n")

local dfl = require('lib.dfl')

function test()

   -- Higher order abstract syntax
   local function f(self, a, b)
      local c = self:mul(a, a)
      local d = self:mul(b, b)
      return self:add(c, d)
   end

   -- Instantiate eval semantics
   log("- eval:\n" .. f(dfl.eval, 1, 2) .. "\n")

   -- ... compilation semantics:
   local dag = dfl.compile(f, 2)
   log("- dag:\n")
   log_desc(dag)
   log("\n")

   local code = dfl.print(f, 2, "testfun")
   log("- code:\n")
   io.stdout:write(code)

end

test()
log("test_dfl.lua end\n")

