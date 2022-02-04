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
   assert(type(ir) == 'table')
   if ir.class then
      log_w(se.iolist(ir))
   else
      pprint:pprint_to_stream(io.stderr,ir)
   end
   -- log_se(ir)
end

local config = {
   trace = trace,
}
local multipass = comp.make_multipass({
      'lure.scheme_frontend',
      'lure.scheme_flatten_blocks',
      'lure.scheme_blockval',
      'lure.scheme_luapp',
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

end

return { run = main }



