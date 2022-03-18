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


-- State machine compiler.
local c_new_sm =
   comp.make_multipass_new(
      {
         'lure.scheme_frontend',
         'lure.scheme_flatten',
         'lure.scheme_sm',
         'lure.scheme_flatten',
      })

local function run_sm()
   local cc_version = cc.version()
   local str = asset['test_backend_c_sm.scm']
   assert(str)
   local exprs = se.read_string_multi(str)
   for expr in se.elements(exprs) do
      log_se_n(expr, "INPUT:")
      local c = c_new_sm()
      local ir = c:compile(expr)
      log("IR:") ; pretty.log_pp(ir)
      local c = backend_c.new()
      -- c.write = function(_, str) io.stderr:write(str) end
      local out = c:compile(ir)
      log_se_n(out, "OUTPUT:\n")
      if cc_version then
         -- If there is a C compiler available, compile and run.
         local inp = se.list()
         local c_code = backend_c.wrap_main(se.iolist(out))
         local out = cc.eval_se(c_code, inp)
         log_se_n(out, "C_RUN:\n")
      end
   end
end


-- Plain function compiler.

local c_new_top =
   comp.make_multipass_new(
      {
         'lure.scheme_frontend',
         'lure.scheme_flatten',
         -- This is now done as part of backend_c, after rewriting
         -- toplevel expression.
         -- 'lure.scheme_blockval',
      })

local function run_top()
   local cc_version = cc.version()
   local str = asset['test_backend_c_top.scm']
   assert(str)
   local exprs = se.read_string_multi(str)
   for expr in se.elements(exprs) do
      log_se_n(expr, "INPUT:")
      local c = c_new_top({
            -- Produce recursive labels form
            primitive_letrec = 'primitive-letrec',
            -- Ensure that the return value slot of a block is primitive.
            block_primitive_return = true,
            -- Configure scheme_blockval to produce only toplevel
            -- functions, using this label for the inner forms of the
            -- toplevel labels expression.
            main_label = 'inner',
      })
      local ir = c:compile(expr)
      log("IR:") ; pretty.log_pp(ir)
      local c = backend_c.new()
      -- c.write = function(_, str) io.stderr:write(str) end
      local out = c:compile(ir)
      log_se_n(out, "OUTPUT:\n")
      if cc_version then
         -- If there is a C compiler available, compile and run.
         local inp = se.list()
         local c_code = backend_c.wrap_main(se.iolist(out))
         local out = cc.eval_se(c_code, inp)
         log_se_n(out, "C_RUN:\n")
      end
   end
end


local function run()
   -- run_sm()
   run_top()
end


return { run = run }


