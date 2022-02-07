local se      = require('lure.se')
local comp    = require('lure.comp')
local asset   = require('lure.asset_scm')
local scheme2 = require('lure.scheme2')

require('lure.log_se')

local mod = {}

-- Compile the input expression to block form.
local c_new =
   comp.make_multipass_new(
      {
         'lure.scheme_frontend',
         'lure.scheme_flatten_blocks',
      })
local filename = 'test_scheme2.scm'
local str = asset[filename]

function mod.run()
   local expr = {'begin',se.read_string_multi(str)}
   log_se_n(expr, "INPUT:")
   local c = c_new()
   local ir = c:compile(expr)
   log_se_n(ir, "IR:")
   local e = scheme2.new()
   local rv = e:eval(ir)
   log_se_n(rv, "OUTPUT:")
end

return mod
