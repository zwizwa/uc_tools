local se     = require('lure.se')
local slc2   = require('lure.slc2')
local iolist = require('lure.iolist')
local asset  = require('lure.asset_scm')
require('lure.log_se')

local function run()
   local str = asset['test_slc2.scm'] ; assert(str)
   -- Note: currently input language is not Scheme, but some in-flux IR.
   local exprs = se.read_string_multi(str)
   assert(se.length(exprs) == 1)
   local expr = se.car(exprs)
   log_se(expr)
   local c = slc2.new()
   log_desc(c)
   local expr1 = c:compile(expr)
   -- Output is an iolist
   log_w(expr1)
end
return { run = run }
