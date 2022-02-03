#!/usr/bin/env lua
-- slc variant that can cmpile test_rvm.scm
package.path = package.path .. ";./?.lua"

local slc    = require('lure.slc')
local se     = require('lure.se')
local prompt = require('prompt')
require('lure.log_se')

local scheme_macro_anf      = require('lure.scheme_macro_anf')
local scheme_pretty         = require('lure.scheme_pretty')
local scheme_flatten_blocks = require('lure.scheme_flatten_blocks')
local scheme_match          = require('lure.scheme_match')

local asset = require('lure.asset_scm')

-- Shorthand for container type conversions
local l = se.list
local a = se.list_to_array


local function main()
   -- Config slc to use asset table
   local c = slc.new({ log = log, asset = asset })
   -- HACK: It exposes its reader, which uses the asset table.  This
   -- just gives us the s-expressions.  Better: make the reader itself
   -- configurable to use an asset table.
   local exprs = c:read_multi('test_rvm.scm')
   assert(exprs)
   local expr = {'module-begin',exprs}

   -- log_se_n(expr, "INPUT: ")
   local expander = scheme_macro_anf.new()
   --log_desc({expander = expander})
   -- local expr1 = expander:expand(expr)
   local expr1 = expander:compile(expr)
   -- log_se_n(expr1, "COMPILED: ")
   local pprint = scheme_pretty.new()
   -- pprint:pprint_to_stream(io.stderr,expr1)

   local flattener = scheme_flatten_blocks.new()
   local expr2 = flattener:compile(expr1)
   -- pprint:pprint_to_stream(io.stderr,expr2)
   
   local test_match = scheme_match.new()
   local expr3 = test_match:compile(expr2)
   pprint:pprint_to_stream(io.stderr,expr3)
   

end

return { run = main }



