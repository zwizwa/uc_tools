-- Apparently is possible to turn a constructor into a destructor.

require('lib.log')

local match = require('lib.match')
local compile_pattern = match.compile_pattern
local match_pattern   = match.match_pattern

local function apply_match(pat_bundle, match_result, handler)
   assert(match_result)
   local args = {}
   for i=1,#pat_bundle.args do
      local var = pat_bundle.args[i]
      args[i] = match_result[var]
      assert(args[i])
   end
   return handler(unpack(args))
end


-- 1. Represent the pattern as a constructor.
local pattern_syntax =
   function(a, b)
      return {'add',a,b}
   end
-- 2. Compile the constructor into a data structure that can be used
--   for matching.
local pattern_obj = compile_pattern(pattern_syntax, 2)
log_desc({pattern_obj = pattern_obj})
-- 3. Perform the match.
local expr = {'add',100,200}
local match = match_pattern(expr, pattern_obj)
log_desc({expr = expr, match = match})
-- 4. Apply handler
function handler(a,b)
   log_desc({handler = {a = a, b = b}})
end
apply_match(pattern_obj, match, handler)
