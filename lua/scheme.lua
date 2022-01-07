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


   local exprs = parser:read_multi()
   local interp = scheme.new()

   local env = se.empty

   local val =
      interp:eval_top(
         {'module',{se.list('module'),exprs}})

   log(val .. "\n")
end

-- local file = "test.sm"
eval(arg[1])

