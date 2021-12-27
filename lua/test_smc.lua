#!/usr/bin/env lua
package.path = package.path .. ";./?.lua"
local se  = require('lib.se')
local smc = require('lib.smc')
local prompt = require('prompt')
local function log(str)
   io.stderr:write(str)
end
local function log_desc(obj)
   log(prompt.describe(obj))
   log("\n")
end
local function test()
   local file = "test.sm"
   local stream = io.open(file,"r")
   local parser = se.new(stream)
   parser.log = function(self, str) io.stderr:write(str) end
   local expr = parser:read()
   -- log_desc(expr)
   local interp = smc.new()
   interp.write = function(self, str) io.stdout:write(str) end

   interp:compile_passes(expr)
end
test()
