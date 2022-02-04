#!/usr/bin/env lua
-- slc variant that can cmpile test_rvm.scm
package.path = package.path .. ";./?.lua"

local slc    = require('lure.slc')
local se     = require('lure.se')
local comp   = require('lure.comp')
local pretty = require('lure.scheme_pretty')
require('lure.log_se')

local asset = require('lure.asset_scm')


local pprint = pretty.new()

-- Shorthand for container type conversions
local l = se.list
local a = se.list_to_array

local function trace(ir, pass)
   log_w("\n",pass, ":\n")
   pprint:pprint_to_stream(io.stderr,ir)
   -- log_se(ir)
end
local config = {
   trace = trace
}
local multipass = comp.make_multipass({
      'lure.scheme_frontend',
      'lure.scheme_flatten_blocks',
      'lure.scheme_blockval',
})

local function main()
   -- local input = 'test_rvm.scm'
   local input = 'test_scheme_pass.scm'

   local str = asset[input]
   assert(str)
   local exprs = se.read_string_multi(str)
   assert(exprs)
   local expr = {'module-begin',exprs}

   local c = multipass.new(config)
   local expr1 = c:compile(expr)


   -- -- Config slc to use asset table
   -- local c = slc.new({ log = log, asset = asset })
   -- -- HACK: It exposes its reader, which uses the asset table.  This
   -- -- just gives us the s-expressions.  Better: make the reader itself
   -- -- configurable to use an asset table.

   -- -- log_se_n(expr, "INPUT: ")
   -- local expander = scheme_frontend.new()
   -- --log_desc({expander = expander})
   -- -- local expr1 = expander:expand(expr)
   -- local expr1 = expander:compile(expr)
   -- -- log_se_n(expr1, "COMPILED: ")
   -- local pprint = scheme_pretty.new()
   -- -- pprint:pprint_to_stream(io.stderr,expr1)

   -- local flattener = scheme_flatten_blocks.new()
   -- local expr2 = flattener:compile(expr1)
   -- -- pprint:pprint_to_stream(io.stderr,expr2)
   
   -- local test_match = scheme_match.new()
   -- local expr3 = test_match:compile(expr2)
   -- pprint:pprint_to_stream(io.stderr,expr3)
   

end

return { run = main }



