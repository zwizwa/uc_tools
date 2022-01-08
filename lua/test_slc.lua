#!/usr/bin/env lua
package.path = package.path .. ";./?.lua"

local slc    = require('lib.slc')
local se     = require('lib.se')
local prompt = require('prompt')
local function log(str) io.stderr:write(str) end
function log_desc(obj) log(prompt.describe(obj) .. "\n") end


-- Shorthand for container type conversions
local l = se.list
local a = se.list_to_array


local function test1()
   local expr = l('lambda',l('a','b'),
                  l('let*', l(l('c', l('add','a','b')),
                              l('f', l('lambda',l('x'),'x')),
                              l('d', l('let*',
                                       l(l('x',123)),
                                       'x'))),
                    l('add','c','d'),
                    l('add','d','c')))
   local compiler = slc.new()
   -- compiler:compile(expr)
   local str = compiler:to_string(expr)
   log(str)
   local fun = loadstring("return " .. str)()
   log_desc(fun)
   fun(1, 2)
end

local function test2()
   local mod = slc.new():loadscheme('test_scheme.scm')()
   log_desc({rv = mod.x(1,2)})
end

-- test1()
test2()
