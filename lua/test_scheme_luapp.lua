#!/usr/bin/env lua
-- Test for scheme_luapp Lua code printer.
package.path = package.path .. ";./?.lua"

local se            = require('lib.se')
local scheme_luapp  = require('lib.scheme_luapp')
require('lib.log_se')
local ins = table.insert

local function test(str)
   local expr = se.read_string(str)
   log_se_n(expr)
   local c = scheme_luapp.new()
   c.write = function(_, str) io.stderr:write(str) end
   c:compile(expr)
end

test([[
(block (
   (a 123)
   (_ (set! a 456))))
]])


