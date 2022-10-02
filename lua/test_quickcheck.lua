#!./lua.sh

-- Property based tests for C code exposed in test_lua51.c
-- Uses https://github.com/luc-tielen/lua-quickcheck

local lqc      = require 'lqc.quickcheck'
local property = require 'lqc.property'
local random   = require 'lqc.random'
local int      = require 'lqc.generators.int'
local report   = require 'lqc.report'

local c = require 'test_lua51'

c.pbuf_a_new()

require 'lure/log'

random.seed()

lqc.init(
   -- Number of tests to run
   50,
   -- Number of shrinks to attempt for failing test cases.  Each
   -- failure invokes the when_fail callback, where we print the args.
   30)

lqc.properties = {}

-- Function under test
function assoc_fun(x, y)
   -- log_desc({run={x=x,y=y}})
   return x + 2 * y  -- not associative
   -- return x + y;  -- associative
end

-- Common configuration wapper for lqc.property
function prop(generators, check)
   property(
      'noname', {
         generators = generators,
         check = check,
         when_fail = function(x, y)
            log_desc({fail={x=x,y=y}})
         end
   })
end


prop({int(100),int(100)},
     function(x,y) return assoc_fun(x,y) == assoc_fun(y,x) end)


-- log_desc({properties = lqc.properties})
lqc.check()
