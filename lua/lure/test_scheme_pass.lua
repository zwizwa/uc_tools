#!/usr/bin/env lua

-- slc variant that can cmpile test_rvm.scm
package.path = package.path .. ";./?.lua"

-- Compilers to test
local slc2   = require('lure.slc2')

-- Test code
local se     = require('lure.se')
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

local function main()
   -- local input = 'test_rvm.scm'
   local input = 'test_scheme_pass.scm'

   local str = asset[input]
   assert(str)
   local exprs = se.read_string_multi(str)
   assert(exprs)
   local expr = {'module-begin',exprs}

   -- All compilers expect a module-begin form.
   local c = slc2.new(config)
   local lua_iol = c:compile(expr)

   -- The result is an iolist containing the lua expression.
   local lua_mod = slc2.eval(lua_iol)
   log_desc(lua_mod)
end

return { run = main }



