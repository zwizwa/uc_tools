#!/usr/bin/env lua
package.path = package.path .. ";./?.lua"

local rt = require('lure.slc_runtime')
local asset = require('lure.asset_scm')

require('lure.log')

-- RUNTIME TESTS

-- slc.lua does not support constant-space tail calls.
-- To work around this we do implement named let using a trampoline.
--
-- This only works when the function defined by named let is only
-- called in tail position.  That is going to be true in most
-- reasonable cases but really should be checked!

-- example:
--
-- (define (test_named_let)
--   (let squares ((n 0) (a 2))
--     (if (> n 3) a (squares (+ n 1) (* a a)))))
--

local function test_named_let()
   log("-- test0: rt.named_let\n")
   return (rt['named-let-trampoline'])(
      {0, 2},
      function(squares)
         return function(n, a)
            -- Point is that the body is intact.
            if n > 3 then
               return a
            else
               return squares(n + 1, a * a)
            end
         end
      end)
end

local function test0()
   log('rv = ' .. test_named_let() .. '\n')
end


-- COMPILER TESTS


local slc    = require('lure.slc')
local se     = require('lure.se')
local prompt = require('prompt')


-- Shorthand for container type conversions
local l = se.list
local a = se.list_to_array


local function test1()
   log("-- test1: compile from sexpr\n")
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

local function loadscheme(config, filename)
   assert(config.asset)
   local comp = slc.new(config)
   local mod = comp:loadscheme(filename)
   return mod
end

local function test2()
   log("-- test2: compile to concrete lua\n")
   local mod = loadscheme({ asset = asset }, 'test_slc.scm')
   -- Execute code, print result
   log_desc({rv = mod.test_add(1,2)})
end

local function test3()
   log("-- test3: compile to lua hoas\n")
   local mod = loadscheme({ log = log, hoas = "_s", asset = asset }, 'test_slc_hoas.scm')
   -- Insert eval semantics.
   local eval = {prim = {}}
   function eval:lambda(nb_args, fun)
      return fun
   end
   function eval:app(fun, ...)
      return fun(unpack({...}))
   end
   function eval:ifte(cond,iftrue,iffalse)
      if cond then return iftrue() else return iffalse() end
   end
   local me = mod(eval)
   log_desc(eval:app(me.test_if))
end

local function test4()
   log("-- test4: debugging\n")
   local filename = 'test_slc_debug.scm'
   local c = slc.new({ log = log, asset = asset })
   local str = c:compile_module_file(filename)
   io.write(str)
end

local function run(w)
--test0()
--test1()
--test2()
--test3()
test4()
end

return { run = run }

