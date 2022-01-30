-- Apparently is possible to turn a constructor into a destructor.

require('lib.log')

local match = require('lib.match')

-- 1. Represent the pattern as a constructor.
local pattern_syntax =
   function(a, b)
      return {'add',a,b}
   end
-- 2. Compile the constructor into a data structure that can be used
--   for matching.
local pattern_obj = match.compile(pattern_syntax, 2)
log_desc({pattern_obj = pattern_obj})
-- 3. Perform the match.
local expr = {'add',100,200}
local match_result = match.eval(expr, pattern_obj)
log_desc({expr = expr, match_result = match_result})
-- 4. Apply handler
function handler(a,b)
   log_desc({handler = {a = a, b = b}})
end
match.apply(pattern_obj, match_result, handler)
