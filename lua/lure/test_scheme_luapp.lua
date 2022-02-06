#!/usr/bin/env lua
-- Test for scheme_luapp Lua code printer pass.
package.path = package.path .. ";./?.lua"

local se            = require('lure.se')
local scheme_luapp  = require('lure.scheme_luapp')
require('lure.log_se')
local ins = table.insert

local function test(str)
   local expr = se.read_string(str)
   log_se_n(expr)
   local c = scheme_luapp.new()
   c.write = function(_, str) io.stderr:write(str) end
   log_w(c:compile(expr))
end

local function run(w)
test([[
(block
  (fun1
    (lambda (x)
      (block
        (rv #<void>)
        (_ (if x
             (block (_ (set! rv 1)))
             (block (_ (set! rv 2))))))))
  (fun2
    (lambda ()
      (block
        (f (lambda (x) (block (_ (return x)))))
        (a 123)
        (_ (set! a 456)))))
)
]])
end

return { run = run }


