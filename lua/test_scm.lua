#!/usr/bin/env lua
package.path = package.path .. ";./?.lua"
local se  = require('lib.se')
local scm = require('lib.scm')
local prompt = require('prompt')
local function log(str)
   io.stderr:write(str)
end
local function log_desc(obj)
   log(prompt.describe(obj))
   log("\n")
end
local function test()
   local file = "test.scm"
   local stream = io.open(file,"r")
   local parser = se.new(stream)
   parser.log = function(self, str) log(str) end
   local expr = parser:read()
   -- log_desc(expr)
   local interp = scm.new()
   interp.write = function(self, str) log(str) end

   interp:compile2(expr)
end
test()
