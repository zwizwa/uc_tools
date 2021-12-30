#!/usr/bin/env lua
package.path = package.path .. ";./?.lua"
local se  = require('lib.se')
local scheme = require('lib.scheme')
local prompt = require('prompt')
local function log(str)
   io.stderr:write(str)
end
local function log_desc(obj)
   log(prompt.describe(obj))
   log("\n")
end
local function eval(file)
   assert(file)
   local stream = io.open(file,"r")
   local parser = se.new(stream)
   parser.log = function(self, str) io.stderr:write(str) end
   local expr = parser:read()
   -- log_desc(expr)
   local interp = scheme.new()
   -- interp.config.first_pass_prefix = "dbg_pass1_"
   interp.write = function(self, str) io.stdout:write(str) end

   local val = interp:eval(expr)
   log(val .. "\n")
end

-- local file = "test.sm"
eval(arg[1])

