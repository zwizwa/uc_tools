#!/usr/bin/env lua
package.path = package.path .. ";./?.lua"

local slc  = require('lib.slc')
local se   = require('lib.se')

-- Shorthand for container type conversions
local l = se.list
local a = se.list_to_array


local function test()
   local expr = l('lambda',l('a','b'),
                  l('let*', l(l('c', l('add','a','b')),
                              l('f', l('lambda',l('x'),'x')),
                              l('d', l('let*',
                                       l(l('x',123)),
                                       'x'))),
                    l('add','c','d'),
                    l('add','d','c')))
   local c = slc.new()
   c:compile(expr)
end

test()
