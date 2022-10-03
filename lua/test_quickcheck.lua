#!./lua.sh

-- Property based tests for C code exposed in test_lua51.c
-- Uses https://github.com/luc-tielen/lua-quickcheck

-- Some guidelines:
--
-- . Write the parameterized C test first using some ad-hoc examples,
--   save that as a C test file, then add a random element here.
--
-- . Write C tests in terms of macro.h ASSERT, then redefine ABORT to
--   use longjmp.  This works well for most state machine code because
--   all allocation can typically done on the C stack.


local lqc      = require 'lqc.quickcheck'
local property = require 'lqc.property'
local random   = require 'lqc.random'
local int      = require 'lqc.generators.int'
local report   = require 'lqc.report'
local Gen      = require 'lqc.generator'

local C = require 'test_lua51'

C.pbuf_a_new()

require 'lure/log'

random.seed()

lqc.init(
   -- Number of tests to run
   51,
   -- Number of shrinks to attempt for failing test cases.  Each
   -- failure invokes the when_fail callback, where we print the args.
   30)

lqc.properties = {}

-- Function under test
function assoc_fun(x, y)
   -- log_desc({run={x=x,y=y}})
   -- return x + 2 * y  -- not associative
   return x + y;  -- associative
end

-- Common configuration wapper for lqc.property
function prop(generators, check)
   property(
      'noname', {
         generators = generators,
         check = check,
         when_fail = function(...)
            log_desc({fail={...}})
         end
   })
end

local _int = int()
log_desc(_int)
local function nat()
   return Gen.new(
      function(...)
         log_desc({nat={...}})
         return math.abs(_int.pick_func(...))
      end,
      _int.shrink_func
   )
end


--prop({int(100),int(100)},
--     function(x,y) return assoc_fun(x,y) == assoc_fun(y,x) end)

prop({nat(),nat(),nat(),nat()},
     function(a,b,c,d)
        log_desc({test={a,b,c,d}})
        return 0 == C.heap_test1(a,b,c+1,d)
     end)


-- log_desc({properties = lqc.properties})
lqc.check()
