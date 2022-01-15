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
   compiler:compile(expr)
end

local function test2()
   log("-- test2: compile to concrete lua\n")
   local mod = slc.new({ log = log }):loadscheme('test_scheme.scm')
   -- Execute code, print result
   log_desc({rv = mod.x(1,2)})
end

local function test3()
   log("-- test3: compile to lua hoas\n")
   local mod = slc.new({ log = log, hoas = "_s" }):loadscheme('test_scheme.scm')
   -- Insert eval semantics.
   local eval = {prim = {}}
   function eval:lambda(nb_args, fun)  return fun end
   function eval:app(fun, ...) return fun(unpack({...})) end
   log_desc(mod(eval))
   -- FIXME: This needs some work, but basic idea is there.
end

-- test1()
test2()
test3()
