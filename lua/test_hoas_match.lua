-- Apparently is possible to turn a constructor into a destructor.

require('lib.log')

local match = require('lib.match')

-- 1. Represent the pattern as a constructor parameterized by a table.
local pattern_syntax =
   function(m)
      return {'add', {'sub', m.a, m.b}, m.c}
   end

-- 2. Compile the constructor into a data structure that can be used
--    for matching.
local pattern_obj = match.compile(pattern_syntax)
log_desc({pattern_obj = pattern_obj})

-- 3. Perform the match.
local expr = {'add',{'sub',300,200},100}
local match_result = match.apply(pattern_obj, expr)
log_desc({expr = expr, match_result = match_result})

