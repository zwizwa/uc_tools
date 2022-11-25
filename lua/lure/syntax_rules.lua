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
local a2l = se.array_to_list


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

-- FIXME: Make one that maps to mcase from scheme_macros.lua

local function id(x)
   return x
end
local function uq(x)
   return l("unquote", x)
end
local function table_ref(tab)
   return function(var_name)
      return l("unquote", l("table-ref", tab, l("quote", var_name)))
   end
end



local function compile(des, con, ref)

   -- log_se(des) ; log("\n")
   local syms = {}
   local function collect(sym) syms[sym] = true ; return uq end
   local compiled_des = expr_unquoted(des, collect)
   -- log_se(compiled_des) ; log("\n")
   -- log_desc({syms=syms})

   -- log_se(con) ; log("\n")
   local free = {}
   local function sym_or_free(sym)
      -- Dereferemce os if symbol is bound in destern
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
   local compiled_con = expr_unquoted(con, sym_or_free)
   -- log_se(compiled_con) ; log("\n")
   -- log_desc({free=free})

   return { syms = syms,
            free = free,
            des = compiled_des,
            con = compiled_con }
end

-- Compile it to:
-- compile-qq-pattern: compiles quasi-quoted patterns to matcher's representation
-- match-qq-pattern: invoke matcher with compiled patterns and handler clauses

local function macro(expr, config)
   assert(config and config.state and config.state.gensym)
   local function gensym() return config.state:gensym() end
   local _, literals, rules = se.unpack(expr, {n=2, tail=true})
   local patcomp = {} -- compiled patterns
   local clauses = {} -- match patterns
   for rule in se.elements(rules) do
      local des, con  = se.unpack(rule, {n=2})
      local patvar    = gensym()
      local table_var = gensym()
      local compiled  = compile(des, con, table_ref(table_var))
      table.insert(
         patcomp,
         l(patvar,
           l("compile-qq-pattern",
             l("quote", compiled.des))))
      table.insert(
         clauses,
         l("cons",
           patvar,
           l("lambda", l(table_var),
             l("quasiquote", compiled.con))))
   end
   local expr_var = gensym()
   local rv_exp =
      l("let",
        a2l(patcomp),
        l("lambda",l(expr_var),
          l("match-qq-patterns", expr_var,
            {"list", a2l(clauses)})))
   -- log_se(rv_exp) ; log("\n")
   return rv_exp
end


-- FIXME: older scheme macro attempt.  can probably be removed

-- -- FIXME: Fix configurable macros by using syntax scope + lexical
-- -- scope during expansion.

-- -- (let ((v (table-ref m (quote v)))))
-- macro['match-qq'] = function(expr, c)
--    need_gensym(c)
--    local _, match_expr, clauses = se.unpack(expr, {n = 2, tail = true})
--    local mod_bs = se.empty

--    local function compile(clause)
--       local pattern, handle = se.unpack(clause, {n = 1, tail = true})
--       local cpat = match.compile(se.constructor(pattern))
--       local var_list = se.empty
--       for var in pairs(cpat.vars) do
--          var_list = {var.var, var_list}
--       end
--       local m = c.state:gensym()  -- table containing matches
--       local function make_binding(var_name)
--          return l(var_name, l('table-ref', m, l('quote', var_name)))
--       end
--       local bindings = se.map(make_binding, var_list)
--       -- The cpat is not printable, so we have to rebuild it at
--       -- runtime.  Use module-level variables for this.
--       local patvar = c.state:gensym()
--       local mod_binding =
--          l(patvar,
--            l(c.compile_qq_pattern or 'compile-qq-pattern',
--              l('quote', pattern)))
--       mod_bs = {mod_binding, mod_bs}
--       local handler  = l('lambda',l(m),{'let',{bindings,handle}})
--       return l('cons',patvar,handler)
--    end
--    local compiled_clauses = se.map(compile, clauses)
--    return l('module-let', mod_bs,
--             l(c.match_qq_patterns or 'match-qq-patterns', match_expr,
--               {'list',compiled_clauses}))
-- end




return {
   rule_unquoted = rule_unquoted,
   expr_unquoted = expr_unquoted,
   macro = macro
}


