
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
