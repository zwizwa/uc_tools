-- Scheme macros are implemented as s-expression to s-expression converters.
-- Used by e.g. scheme.lua but could be reused by other dialects.

-- We assume let*, set!, lambda are primitives, which are essentially
-- basic blocks with (SSA) variable declarations + assignment for
-- creating loops.

-- Reductions:
--
-- module-begin -> module
-- begin        -> letrec
-- letrec       -> let*, set!
--
-- We do not depend on '#<void>' here, instead we require that let*
-- supports undefined bindings, e.g. (let* ((a)) ...), and empty
-- statements (let* ())

-- Some macros have an additional config parameter.  This is useful to
-- use the code here to implement language-specific macros with
-- slightly modify the behavior.


local se = require('lib.se')
local macro = {}
local l = se.list
local r = se.reverse

-- Map module-begin to begin in case top level forms are not special.
macro['module-begin'] = function(expr)
   local _, mod_body = se.unpack(expr, { n = 1, tail = true })
   return {'begin',mod_body}
end

local function void(c)
      return l(c.let or 'let*',l())
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
         return {c.let or 'let*',{l(), exprs}}
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
         local import_exprs = se.read_file_multi(filename)
         exprs = se.append(import_exprs, rest)
      else
         return done()
      end
   end
end

-- Implement letrec on top of let* and set!
macro['letrec'] = function(expr, config)
   local c = config or {}
   local _, bindings, exprs = se.unpack(expr, {n = 2, tail = true})
   if se.is_empty(bindings) then
      -- Base case is needed to avoid letrec->begin->letrec loop.
      return {c.let or 'let*',{l(),exprs}}
   end
   local void_bindings = se.map(
      function(binding)
         local name, val = se.unpack(binding, {n = 2})
         return l(name)
      end,
      bindings)
   local set_variables = se.map(
      function(binding)
         local name, val = se.unpack(binding, {n = 2})
         return l(c.set or "set!", name, val)
      end,
      bindings)
   return {c.let or 'let*', {void_bindings, {{c.begin or 'begin', set_variables}, exprs}}}
end

-- This needs a let-insertion to make sure there is only one
-- evaluation.  Symbol generation will need to be provided by caller.
macro['case'] = function(expr, config)
   assert(config.gensym)
   local sym = config.gensym()
   local _, vexpr, clauses = se.unpack(expr, {n = 2, tail = true})
   local function ifexpr(clause, els)
      -- FIXME: This is a partial implementation for rvm
      local match, exprs = se.unpack(clause, {n = 1, tail = true})
      local val = se.unpack(match, {n = 1})
      return l('if',l('eq?',sym,val),{'begin',exprs},els)
   end
   return se.foldr(ifexpr, config.void or '#<void>', clauses)
end

return macro
