#!/usr/bin/env lua
-- slc variant that can cmpile test_rvm.scm
package.path = package.path .. ";./?.lua"

local slc    = require('lure.slc')
local se     = require('lure.se')
local comp   = require('lure.comp')
local iolist = require('lure.iolist')
local pretty = require('lure.scheme_pretty')
require('lure.log_se')

local asset = require('lure.asset_scm')
local string_dsl = require('lure.string_dsl')


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

local global = {
   ['module-register'] = {}
}

local config = {
   trace = trace,
   global = global,
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
   local lua_iol = c:compile(expr)

   -- The result is an iolist containing the lua expression.
   local lua_str = iolist.to_string(lua_iol.iolist)
   local lua_mod = string_dsl.lua_eval(lua_str)
   assert(lua_mod)
   log_desc(lua_mod)
end

return { run = main }



