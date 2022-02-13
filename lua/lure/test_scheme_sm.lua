local se        = require('lure.se')
local comp      = require('lure.comp')
local asset     = require('lure.asset_scm')
local scheme_sm = require('lure.scheme_sm')
local pretty    = require('lure.scheme_pretty')
local flat      = require('lure.scheme_flatten')
local eval      = require('lure.scheme_eval')
local runtime   = require('lure.slc_runtime')

require('lure.log_se')

local ins = table.insert

local mod = {}

-- Compile the input expression to block form.
local c_new =
   comp.make_multipass_new(
      {
         'lure.scheme_frontend',
         'lure.scheme_flatten',
      })

local re_scheme =
   comp.make_multipass_new(
      {
         'lure.scheme_escape',
         'lure.scheme_frontend',
         'lure.scheme_flatten',
      })


local filename = 'test_scheme_sm.scm'
local str = asset[filename]

-- Implement trace as a machine operation that can halt the
-- machine as an infinite loop guard.
local function make_trace()
   local events = se.empty
   local i = 0
   return function(s, event)
      events = {event, events}
      i = i + 1
      if i > 10 then
         local abort = s.prim['abort']
         local rv = event
         -- Reset for next run
         events = {}
         i = 0
         s:ret(abort(rv))
      else
         s:ret(event)
      end
   end
end

local function make_interp()
   local e = eval.new()
   e.prim = runtime
   e.prim['trace']  = {class = 'mop', mop = make_trace()}
   e.prim['return'] = function(val) return val end
   return e
end

function mod.run()
   local exprs = se.read_string_multi(str)

   -- Instead of creating a single expression, restart the interpreter
   -- for each expression to isolate the tests.

   -- exprs[2] = se.empty  -- only first expression

   for expr in se.elements(exprs) do

      log_se_n(expr, "INPUT:")
      local c = c_new()
      local ir = c:compile(expr)

      -- log("INPUT_IR:") ; pretty.log_pp(ir)
      local input_ir_val = make_interp():eval(ir)
      log("EVAL_INPUT_IR:") ; log_se_n(input_ir_val)

      local smc = scheme_sm.new()
      smc.prim = require('lure.slc_runtime')
      local i = 1

      local out = smc:compile(ir)
      -- log("OUTPUT_NONFLAT:") ; pretty.log_pp(out)

      -- Flatten before pp
      local f = flat.new() ; out = f:compile(out)
      log("OUTPUT_IR:") ; pretty.log_pp(out)


      -- Interpret output IR as Scheme

      -- Note that 'block' and 'if' are the same as their primitive
      -- form, so we don't need to remap to 'block@' and 'if@' using
      -- scheme_escape.
      local ir_tx = re_scheme():compile(out)
      -- log("IR_TX:") ; pretty.log_pp(ir_tx)

      local output_ir_val = make_interp():eval(ir_tx)


      if not (runtime['equal?'])(input_ir_val, output_ir_val) then
         log("EVAL_OUTPUT_IR:") ; log_se_n(output_ir_val)
         error('eval-difference')
      end

      -- FIXME: Re-interpretation of ir fails for some reason giving
      -- bizarre error.  There is some lingering state somehwere,
      -- maybe an accidental global variable?.  It's as if the
      -- evaluation here uses some other environment.
      --
      -- local input_ir_val = make_interp():eval(ir)
      -- log("EVAL_INPUT_IR:") ; log_se_n(input_ir_val)
      -- I think that smc is modifying the ir in-place.

   end
end

return mod
