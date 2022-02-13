local se        = require('lure.se')
local comp      = require('lure.comp')
local asset     = require('lure.asset_scm')
local scheme_sm = require('lure.scheme_sm')
local pretty    = require('lure.scheme_pretty')
local flat      = require('lure.scheme_flatten')
local eval      = require('lure.scheme_eval')

require('lure.log_se')

local mod = {}

-- Compile the input expression to block form.
local c_new =
   comp.make_multipass_new(
      {
         'lure.scheme_frontend',
         'lure.scheme_flatten',
      })

local tx_new =
   comp.make_multipass_new(
      {
         'lure.scheme_escape',
         'lure.scheme_frontend',
         'lure.scheme_flatten',
      })


local filename = 'test_scheme_sm.scm'
local str = asset[filename]

function mod.run()
   local exprs = se.read_string_multi(str)

   -- Instead of creating a single expression, restart the interpreter
   -- for each expression to isolate the tests.
   -- exprs[2] = se.empty

   for expr in se.elements(exprs) do

      log_se_n(expr, "INPUT:")
      local c = c_new()
      local ir = c:compile(expr)

      -- log("IR:") ; pretty.log_pp(ir)

      local e = scheme_sm.new()
      e.prim = require('lure.slc_runtime')
      local i = 1
      local function trace(tag)
         assert(i < 100)
         -- log_se_n(tag,"TRACE:")
         i = i + 1
      end

      -- e.prim.halt  = halt
      e.prim.trace = trace
      local out = e:compile(ir)

      -- log("OUTPUT_NONFLAT:") ; pretty.log_pp(out)

      -- Flatten before pp
      local f = flat.new() ; out = f:compile(out)
      log("OUTPUT:") ; pretty.log_pp(out)

      -- Interpret as Scheme.

      -- Note that 'block' and 'if' are the same as their primitive
      -- form, so we don't need to remap to 'block@' and 'if@' using
      -- scheme_escape.
      local tx = tx_new()
      local ir_tx = tx:compile(out)
      -- log("TX:") ; pretty.log_pp(ir_tx)

      local e = eval.new()
      e.prim = require('lure.slc_runtime')
      local rv = e:eval(ir_tx)
      log("EVAL:") ; log_se_n(rv)

   end
end

return mod
