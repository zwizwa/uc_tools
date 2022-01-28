#!/usr/bin/env lua
-- slc variant that can cmpile test_rvm.scm
package.path = package.path .. ";./?.lua"

local slc    = require('lib.slc')
local se     = require('lib.se')
local prompt = require('prompt')
require('lib.log')

-- Shorthand for container type conversions
local l = se.list
local a = se.list_to_array

local function main()
   local c = slc.new({ log = log })
   local mod = c:loadscheme('test_rvm.scm')
   -- Execute code, print result
   log_desc({rv = mod.test_add(1,2)})
end

main()

