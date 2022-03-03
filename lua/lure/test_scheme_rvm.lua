-- This test runs the Ribbit VM implemented in Scheme
-- (lure/test_rvm.scm) inside the Lure Scheme interpreter as a test
-- for the Lure Scheme interpreter.

-- This is unrelated to lure/rvm.lua and lure/rvm_debug.lua, the Lua
-- implementations of Ribbit VM + test routines.

local se              = require('lure.se')
local comp            = require('lure.comp')
local asset           = require('lure.asset_scm')
local scheme_eval     = require('lure.scheme_eval')
local scheme_pretty   = require('lure.scheme_pretty')
local asset           = require('lure.asset_scm')

require('lure.log_se')

local mod = {}

-- Compile the input expression to block form.
local c_new =
   comp.make_multipass_new(
      {
         'lure.scheme_frontend',
         'lure.scheme_flatten',
      })
local filename = 'test_scheme_eval.scm'
local str = asset[filename]

function mod.run()

   -- Full Scheme interpretation
   local input = 'test_rvm.scm'

   local str = asset[input]
   assert(str)

   local exprs = se.read_string_multi(str)

   -- local expr = {'module-begin',exprs}
   local expr = {'begin',exprs}

   -- log_se_n(expr, "INPUT:")
   local c = c_new()
   local ir = c:compile(expr)
   -- log_se_n(ir, "IR:")
   -- log("IR:") ; scheme_pretty.log_pp(ir)

   local prim = require('lure.slc_runtime')
   local rvm = require('lure.rvm')

   -- This is for 'input' variable containing byte code.
   local rvm_debug = require('lure.rvm_debug')

   local function prim_index(o,k)
      local v
      v = rvm_debug[k] ; if v~=nil then return v end
      v = rvm[k]       ; if v~=nil then return v end
      v = prim[k]      ; if v~=nil then return v end
   end

   local e = scheme_eval.new(prim_index)
   local rv = e:eval(ir)
   log_se_n(rv, "EVAL:")
   log("\n")
end

return mod
