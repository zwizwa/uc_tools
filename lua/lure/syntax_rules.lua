-- FIXME: I don't know what I'm doing yet, but it seems syntax-rules
-- is useful enough even if it is incomplete.

-- 1. Implement just the pattern language
-- 2. Ignore literals
-- 3. Ignore hygiene
-- 4. Ignore ellipsis

-- The pattern language deconstruction can be mapped to se_match
-- syntax that is used in other places.  The reconstruction is then
-- quasiquote plus match dictionary references.

local se = require('lure.se')


-- Convert a matching pattern to the syntax used by se_match.
--
-- visit_sym is called for each sym and returns true/false determining
-- whether sym is quoted or unquoted.
local function expr_unquoted(expr, visit_sym)
   assert(visit_sym)
   if se.is_pair(expr) then
      return se.cons(expr_unquoted(se.car(expr), visit_sym),
                     expr_unquoted(se.cdr(expr), visit_sym))
   elseif expr == se.empty then
      return expr
   elseif type(expr) == 'string' and visit_sym(expr) then
      return se.list("unquote", expr)
   else
      return expr
   end
end

local function rule_unquoted(expr)
   local pat, new = se.unpack(expr, {n=2})

   log_se(pat) ; log("\n")
   local syms = {}
   local function collect(sym) syms[sym] = true ; return true end
   log_se(expr_unquoted(pat, collect)) ; log("\n")
   log_desc({syms=syms})

   log_se(new) ; log("\n")
   local free = {}
   local function ref(sym)
      -- Quote if symbol is bound in pattern
      if syms[sym] then return true end
      -- Otherwise don't quote, and collect it as unbound.  This list
      -- of introduced symbols should later be handled differently.
      -- It seems that we need to distinguish between two things: is
      -- this an identifier (variable or macro) that is present in the
      -- environment or not.  If not, we can assume that we are
      -- introducing it and rename it (otherwise it would be an
      -- unbound variable).
      free[sym] = true
      return false
   end
   log_se(expr_unquoted(new, ref))
   log_desc({free=free}) ; log("\n")
end


return {
   rule_unquoted = rule_unquoted,
   expr_unquoted = expr_unquoted,
}
