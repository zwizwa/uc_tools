-- Scheme macros are implemented as s-expression to s-expression converters.
-- Used by e.g. scheme.lua but could be reused by other dialects.

-- The 'lambda' special form is now the only binder.  Reductions are
-- solved in subsequent passes.  'block' is not allowed in expansion
-- here as it is a mixed sequence/binding form that is more useful as
-- an intermediate form.  For frontend it is simplest to keep binding
-- and sequencing separate.

-- Some macros have an additional config parameter.  This is useful to
-- use the code here to implement language-specific macros with
-- slightly modify the behavior.

require('lure.log')
local se    = require('lure.se')
local match = require('lure.match')
local macro = {}
local l = se.list
local r = se.reverse

-- Map module-begin to begin in case top level forms are not special.
macro['module-begin'] = function(expr)
   local _, mod_body = se.unpack(expr, { n = 1, tail = true })
   return {'begin',mod_body}
end

-- Map definitions in begin form to letrec.
macro['begin'] = function(expr, config)
   local c = config or {}
   local function module_file(name) return name .. ".sm" end
   local _, exprs = se.unpack(expr, {n = 1, tail = true})
   local bindings = se.empty
   local function done()
      if se.is_empty(bindings) then
         -- 'begin' is common, so optimize lack of defs case
         return {c.let or 'sequence',exprs}
      else
         return {c.letrec or 'letrec', {r(bindings), exprs}}
      end
   end
   while true do
      if se.is_empty(exprs) then
         return done()
      end
      local expr, rest = se.unpack(exprs, {n = 1, tail = true})
      if type(expr) == 'table' and expr[1] == (c.define or 'define') then
         -- For now we only support (define (name ...) ...)
         local _define, spec, def_rest = se.unpack(expr, { n = 2, tail = true })
         if type(spec) == 'string' then
            -- (define name thing)
            local thing = se.unpack(def_rest, {n = 1})
            bindings = {l(spec, thing), bindings}
         else
            -- (define (name arg ...) ...)
            local name, args = se.unpack(spec, { n = 1, tail = true })
            assert(type(name) == 'string')
            bindings = {l(name, {c.lambda or 'lambda',{args,def_rest}}), bindings}
         end
         exprs = rest
      elseif type(expr) == 'table' and expr[1] == (c.import or 'import') then
         -- Splice import form
         local _, name = se.unpack(expr, { n = 2 })
         assert(type(name) == 'string')
         local filename = (c.module_file or module_file)(name)
         local read_multi = c.import_read_multi
         assert(read_multi)
         local import_exprs = read_multi(filename)
         exprs = se.append(import_exprs, rest)
      else
         return done()
      end
   end
end

-- Implement letrec on top of let and set!
macro['letrec'] = function(expr, c)
   assert(c and c.void)
   local _, bindings, exprs = se.unpack(expr, {n = 2, tail = true})
   if se.is_empty(bindings) then
      -- Base case is needed to avoid letrec->begin->letrec loop.
      return {c.let or 'sequence',exprs}
   end
   local void_bindings = se.map(
      function(binding)
         local name, val = se.unpack(binding, {n = 2})
         return l(name, c.void or l('sequence'))
      end,
      bindings)
   local set_variables = se.map(
      function(binding)
         local name, val = se.unpack(binding, {n = 2})
         return l(c.set or "set!", name, val)
      end,
      bindings)
   return {c.let or 'let', {void_bindings, {{c.begin or 'begin', set_variables}, exprs}}}
end

local function need_gensym(config, name)
   if not (config and config.state and config.state.gensym) then
      error((name or 'macro') .. " needs gensym")
   end
end

-- This needs a let-insertion to make sure there is only one
-- evaluation.  Symbol generation will need to be provided by caller.
macro['case'] = function(expr, config)
   need_gensym(config,'case')
   local sym = config.state:gensym()
   local _, vexpr, clauses = se.unpack(expr, {n = 2, tail = true})
   local function ifexpr(clause, els)
      -- FIXME: This is a partial implementation for rvm
      local match, exprs = se.unpack(clause, {n = 1, tail = true})
      local val = se.unpack(match, {n = 1})
      return l('if',l('eq?',sym,val),{'begin',exprs},els)
   end
   return l('let',l(l(sym, vexpr)),
            se.foldr(ifexpr, config.void or l('sequence'), clauses))
end


-- Let:
--
-- 1. Named let trampoline
--
-- When generating lambdas it makes sense to bind them to names.  This
-- makes Lua backtraces and generated source code easier to read.
--
-- 2. Non-sequential binding
--
-- Most languages have sequential let*-style binding forms.  To
-- implement the let "bulk binding", it seems simplest just to use a
-- lambda.

-- FIXME: Instead of using a trampoline, map it to a loop construct?
macro['named-let'] = function(expr, c)
   need_gensym(c)
   local tag_name =
      function(src_name)
         return c.state:gensym(src_name .. "_")
      end
   local loop_name = maybe_bindings
   local var_init_expr, loop_body = se.unpack(rest, {n = 1, tail = true})
   local loop_vars = se.map(se.car,  var_init_expr)
   local init_expr = se.map(se.cadr, var_init_expr)
   assert(loop_vars)
   assert(init_expr)
   if c.named_let_trampoline then
      local loop_name_iter = tag_name(loop_name .. "_tick")
      local trampoline_expr =
         l(c.named_let_trampoline,{c.make_state or 'vector',init_expr},
           l('lambda',l(loop_name),
             l('let',l(l(loop_name_iter,
                         l('lambda',loop_vars,{'begin',loop_body}))),
               loop_name_iter)))
      return trampoline_expr
   else
      return l('begin',
               l('define',{loop_name, loop_vars}, {'begin',loop_body}),
               {loop_name, init_expr})
   end
end

-- Frontend uses lambda as only binding form, and will reduce after
-- renaming.
macro['let'] = function(expr, c)
   _, maybe_bindings, rest = se.unpack(expr, {n = 2, tail = true})
   if type(maybe_bindings) == 'string' then
      return {'named-let',{maybe_bindings, rest}}
   elseif se.length(maybe_bindings) == 0 then
      -- Don't make it worse...
      return {'begin', rest}
   else
      local vars  = se.map(se.car,  maybe_bindings)
      local exprs = se.map(se.cadr, maybe_bindings)
      local lambda = {'lambda',{vars,rest}}
      return {lambda,exprs}
   end
end
macro['let*'] = function(expr, c)
   _, bindings, rest = se.unpack(expr, {n = 2, tail = true})
   if se.length(bindings) == 1 then
      return {'let',{bindings,rest}}
   else
      return l('let',l(se.car(bindings)),
               {'let*', {se.cdr(bindings), rest}})
   end
end


-- Use match.lua to implement a small matcher DSL.
-- LHS (des)  is a literal pattern with variable names or numbers unquoted.
-- RHS (cons) has the same form, but in addition supports free variables that map to gensyms.
--
-- Free variables in rewriter clauses represent generated symbols.
--
local function gensym_free_vars(config, env)
   need_gensym(config)
   local s = config.state
   local free = {}
   local function index(_,k)
      local v
      v = env[k]         ; if v then return v end
      v = rawget(free,k) ; if v then return v end
      v = s:gensym()     ; free[k] = v ; return v
   end
   setmetatable(free, {__index = index})
   return free
end
local ins = table.insert
local function mcase(...)
   local cpat   = {}
   local handle = {}
   for _, clause in ipairs({...}) do
      local from, to = unpack(clause)
      local from_cons = se.constructor(from)
      local to_cons   = se.constructor(to)
      local from_cpat = match.compile(from_cons) ; -- log_desc({from_cpat = from_cpat})
      ins(cpat, from_cpat)
      ins(handle, to_cons)
   end
   return function(expr, config)
      local expr1 = se.cdr(expr)
      for i=1,#cpat do
         local m = match.apply(cpat[i], expr1)
         if m then
            local mf = gensym_free_vars(config, m)
            return (handle[i])(mf)
         end
      end
      error("macro '" .. form_name .. "' match error")
   end
end

-- FIXME: Put these in a separate file maybe?

-- macro["let*"] = mcase({"(,1 . ,2)", "(block ,1 (begin . ,2))"})
macro["or"]   = mcase({"(,1   ,2)", "(let ((,tmp ,1)) (if ,tmp ,tmp ,2))"})
macro["and"]  = mcase({"(,1   ,2)", "(let ((,tmp ,1)) (if (not ,tmp) ,tmp ,2))"})

return macro
