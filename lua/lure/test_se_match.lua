
-- Test S-Expression matching on top of generic "inverted constructor"
-- matching.  This inverts the quasiquote constructor and uses the se
-- parser to convert from string to Lua data structure + memoization
-- of string->pattern_obj

local se = require('lure.se')
require('lure.log_se')

-- Patterns are implicitly quasiquoted.
local pats = {
   {"(add ,a ,b)", function(m) return m.a + m.b end},
   {"(sub ,a ,b)", function(m) return m.a - m.b end},
}
local function test_matcher(match)
   local strs = {
      "(add 1 2)",
      "(sub 10 3)",
   }
   for _,str in ipairs(strs) do
      local expr = se.read_string(str)
      local val = match(expr)
      if val == true then val = 'true' end
      if val == false then val = 'false' end
      log_se(expr) ; log(" -> ")
      log(val)
      log("\n")
   end
end

-- Teset the qq renderer separately.
function test_qq_eval(env, str)
   local expr = se.read_string(str)
   local expr1 = se.qq_eval(env, expr)
   log_desc({qq_eval = expr1, expr = expr})
end

local se_match = require('lure.se_match')
local mtch = se_match.new()

function test_interp_new(expr)
   return mtch(expr, pats)
end



local function run(w)
   --test_qq_eval({a = 1, b = 2}, "(unquote a)" )
   --test_qq_eval({a = 1, b = 2}, ",a" )  -- FIXME: Something not right here but above works
   --test_qq_eval({a = 1, b = 2}, "(add ,a ,b)" )
   test_qq_eval({a = 1, b = 2}, "(,a . ,b)")


   test_matcher(test_interp_new)

end

return { run = run }
