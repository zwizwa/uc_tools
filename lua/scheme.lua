#!/usr/bin/env lua
package.path = package.path .. ";./?.lua"
local scheme = require('lure.scheme')
local function eval(file)
   assert(file)
   local interp = scheme.new()
   local val = interp:eval_file(file)
   io.stdout:write(val .. "\n")
end

-- local file = "test.sm"
eval(arg[1])

