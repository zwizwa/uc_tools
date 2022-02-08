local se              = require('lure.se')
local comp            = require('lure.comp')
local asset           = require('lure.asset_scm')
local scheme_blockint = require('lure.scheme_blockint')

require('lure.log_se')

local mod = {}

-- Compile the input expression to block form.
local c_new =
   comp.make_multipass_new(
      {
         'lure.scheme_frontend',
         'lure.scheme_flatten_blocks',
      })
local filename = 'test_scheme_blockint.scm'
local str = asset[filename]

function mod.run()
   local exprs = se.read_string_multi(str)

   -- Instead of creating a single expression, restart the interpreter
   -- for each expression to isolate the tests.
   for expr in se.elements(exprs) do
      log_se_n(expr, "INPUT:")
      local c = c_new()
      local ir = c:compile(expr)
      log_se_n(ir, "IR:")
      local e = scheme_blockint.new()
      e.prim = require('lure.slc_runtime')
      local rv = e:eval(ir)
      log_se_n(rv, "OUTPUT:")
      log("\n")
   end
end

return mod
