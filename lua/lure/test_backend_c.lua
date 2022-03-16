#!/usr/bin/env lua
-- Test for backend_c C code printer pass.
-- Currently this only takes the output from lure.scheme_sm
package.path = package.path .. ";./?.lua"

local se            = require('lure.se')
local comp          = require('lure.comp')
local asset         = require('lure.asset_scm')
local pretty        = require('lure.scheme_pretty')
local backend_c     = require('lure.backend_c')
local cc            = require('lure.cc')
require('lure.log_se')
local ins = table.insert

-- The Lua backend input requires IR, so we generate that from Scheme.
local c_new =
   comp.make_multipass_new(
      {
         'lure.scheme_frontend',
         'lure.scheme_flatten',
         'lure.scheme_sm',
         'lure.scheme_flatten',
      })

local function run()
   local cc_version = cc.version()
   local str = asset['test_backend_c.scm']
   assert(str)
   local exprs = se.read_string_multi(str)
   for expr in se.elements(exprs) do
      log_se_n(expr, "INPUT:")
      local c = c_new()
      local ir = c:compile(expr)
      log("IR:") ; pretty.log_pp(ir)
      local c = backend_c.new()
      -- c.write = function(_, str) io.stderr:write(str) end
      local out = c:compile(ir)
      log_se_n(out, "OUTPUT:\n")
      if cc_version then
         -- If there is a C compiler available, compile and run.
         local inp = se.list()
         local out = cc.eval_se(se.iolist(out), inp)
         log_se_n(out, "C_RUN:\n")
      end
   end
end


return { run = run }


