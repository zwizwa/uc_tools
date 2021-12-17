#!/usr/bin/env lua
package.path = package.path .. ";./?.lua"

local se = require('lib.se')
local prompt = require('prompt')
local function log(str)
   io.stderr:write(str)
end
local function log_desc(obj)
   log(prompt.describe(obj))
   log("\n")
end
log("test_sexpr.lua\n")

local function test()
   local file = "test.scm"
   local stream = io.open(file,"r")
   local parser = se.new(stream)
   parser.log = function(self, str) log(str) end
   parser.list = function(self, lst) return lst end
   parser.atom = function(self, atom) return atom end
   local expr = parser:read()
   log_desc(expr)
end


test()
