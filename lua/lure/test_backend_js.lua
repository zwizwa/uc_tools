#!/usr/bin/env lua
-- Test for backend_js JavaScript code printer pass.
package.path = package.path .. ";./?.lua"

local se             = require('lure.se')
local comp           = require('lure.comp')
local asset          = require('lure.asset_scm')
local pretty         = require('lure.scheme_pretty')
local backend_js     = require('lure.backend_js')
require('lure.log_se')
local ins = table.insert

-- The JavaScript backend input requires IR, so we generate that from Scheme.
local c_new =
   comp.make_multipass_new(
      {
         'lure.scheme_frontend',
         'lure.scheme_flatten',
         'lure.scheme_blockval',
      })

local function run()
   local str = asset['test_backend_js.scm']
   assert(str)
   local exprs = se.read_string_multi(str)
   -- Run the pp separately on each expression
   for expr in se.elements(exprs) do
      log_se_n(expr, "INPUT:")
      local c = c_new()
      local ir = c:compile(expr)
      log("IR:") ; pretty.log_pp(ir)
      -- log_se_n(ir, "IR:")
      local c = backend_js.new()
      -- c.write = function(_, str) io.stderr:write(str) end
      local out = c:compile(ir)
      log_se_n(out, "OUTPUT:\n")
   end
end


return { run = run }


