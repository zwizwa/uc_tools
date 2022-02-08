#!/usr/bin/env lua
-- Test for scheme_luapp Lua code printer pass.
package.path = package.path .. ";./?.lua"

local se            = require('lure.se')
local comp          = require('lure.comp')
local asset         = require('lure.asset_scm')
local scheme_luapp  = require('lure.scheme_luapp')
require('lure.log_se')
local ins = table.insert

-- The luapp input requires IR, so we generate that from Scheme.
local c_new =
   comp.make_multipass_new(
      {
         'lure.scheme_frontend',
         'lure.scheme_flatten_blocks',
      })

local function run()
   local str = asset['test_scheme_luapp.scm']
   assert(str)
   local exprs = se.read_string_multi(str)
   -- Run the pp separately on each expression
   for expr in se.elements(exprs) do
      log_se_n(expr, "INPUT:")
      local c = c_new()
      local ir = c:compile(expr)
      log_se_n(ir, "IR:")
      local c = scheme_luapp.new()
      -- c.write = function(_, str) io.stderr:write(str) end
      local out = c:compile(ir)
      log_se_n(out, "OUTPUT:")
   end
end


return { run = run }


