#!/usr/bin/env lua
-- Test for backend_lua Lua code printer pass.
package.path = package.path .. ";./?.lua"

local se            = require('lure.se')
local comp          = require('lure.comp')
local asset         = require('lure.asset_scm')
local backend_lua   = require('lure.backend_lua')
require('lure.log_se')
local ins = table.insert

-- The Lua backend input requires IR, so we generate that from Scheme.
local c_new =
   comp.make_multipass_new(
      {
         'lure.scheme_frontend',
         'lure.scheme_flatten',
         'lure.scheme_blockval',
      })

local function run()
   local str = asset['test_backend_lua.scm']
   assert(str)
   local exprs = se.read_string_multi(str)
   -- Run the pp separately on each expression
   for expr in se.elements(exprs) do
      log_se_n(expr, "INPUT:")
      local c = c_new({
            -- block_primitive_return = true
      })
      local ir = c:compile(expr)
      log_se_n(ir, "IR:")
      local c = backend_lua.new()
      -- c.write = function(_, str) io.stderr:write(str) end
      local out = c:compile(ir)
      log_se_n(out, "OUTPUT:")
   end
end


return { run = run }


