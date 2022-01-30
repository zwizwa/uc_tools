
local se = require('lib.se')
local se_match = require('lib.se_match')
require('lib.log_se')

local matcher = se_match.new()
local function match(e,p) return matcher:match(e,p) end

-- Patterns are implicitly quasiquoted.
local function test_interp(expr)
   local pats = {
      {"(add ,a ,b)", function(m) return m.a + m.b end},
      {"(sub ,a ,b)", function(m) return m.a - m.b end},
   }
   return match(expr,pats)
end

local function test_expr(str, a, b)
   local expr = se.read_string(str)
   local val = test_interp(expr)
   log_se(expr) ; log(" -> ")
   log(val)
   log("\n")
end

test_expr("(add 1 2)")
test_expr("(sub 10 3)")


-- FIXME: To use the other matcher, maybe convert
-- "(add ,a ,b)" to "l('add',_.a,_.b)" or "{'add',{'_.a',{'_.b','#<empty>'}}}"
-- Let's do that right here.
-- Actually it just needs a quasiquoting "renderer":
-- The important part is the compilation to match pattern data structure, not lua function.
-- For quasiquoting that would then not need to go through the evaluator.
function test_qq_eval(env, str)
   local expr = se.read_string(str)
   local expr1 = se.qq_eval(env, expr)
   log_desc({qq_eval = expr1, expr = expr})
end
test_qq_eval({a = 1, b = 2}, "(add ,a ,b)" )
