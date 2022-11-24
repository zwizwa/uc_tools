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
local l = se.list


-- Convert a matching pattern to the syntax used by se_match.
--
-- visit_sym is called for each sym and returns true/false determining
-- whether sym is quoted or unquoted.
local function expr_unquoted(expr, visit_sym)
   assert(visit_sym)
   if se.is_pair(expr) then
      local function rec(i)
         return expr_unquoted(expr[i], visit_sym)
      end
      return se.cons(rec(1), rec(2))
   elseif expr == se.empty then
      return expr
   elseif type(expr) == 'string' then
      local uq = visit_sym(expr)
      return uq(expr)
   else
      return expr
   end
end

local function rule_unquoted(expr)
   local pat, new = se.unpack(expr, {n=2})
   local function id(x)
      return x
   end
   local function uq(x)
      return l("unquote", x)
   end
   local m = "m"
   local function ref(x)
      return l("unquote", l("table-ref", m, l("quote", x)))
   end

   log_se(pat) ; log("\n")
   local syms = {}
   local function collect(sym) syms[sym] = true ; return uq end
   local uq_pat = expr_unquoted(pat, collect)
   log_se(uq_pat) ; log("\n")
   log_desc({syms=syms})

   log_se(new) ; log("\n")
   local free = {}
   local function sym_or_free(sym)
      -- Dereferemce os if symbol is bound in pattern
      if syms[sym] then return ref end
      -- Otherwise don't quote, and collect it as unbound.  This list
      -- of introduced symbols should later be handled differently.
      -- It seems that we need to distinguish between two things: is
      -- this an identifier (variable or macro) that is present in the
      -- environment or not.  If not, we can assume that we are
      -- introducing it and rename it (otherwise it would be an
      -- unbound variable).
      free[sym] = true
      return id
   end
   local uq_new = expr_unquoted(new, sym_or_free)
   log_se(uq_new) ; log("\n")
   log_desc({free=free})
   local rv_exp =
      l("lambda",l("expr"),
        l("match","expr",
          uq_pat,
          l("lambda", l(m),
            l("quasiquote", uq_new))))
   log_se(rv_exp) ; log("\n")
   return rv_exp
end

local function macro(expr)
   local _, literals, rule = se.unpack(expr, {n=3})
   return rule_unquoted(rule)
end


return {
   rule_unquoted = rule_unquoted,
   expr_unquoted = expr_unquoted,
   macro = macro
}
