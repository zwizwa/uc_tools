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


local se = require('lib.se')
local macro = {}
local l = se.list
local r = se.reverse

-- Map module-begin to begin in case top level forms are not special.
macro['module-begin'] = function(expr)
   local _, mod_body = se.unpack(expr, { n = 1, tail = true })
   return {'begin',mod_body}
end

local void = l('let*',l())

-- Map definitions in begin form to letrec.
-- FIXME: Generalize, e.g.
-- define      -> {letrec,lambda}
-- define-task -> {letrec-task,lambda}
macro['begin'] = function(expr, defkinds)
   --if not defkinds then
   --   defkinds = {define = {letrec = 'letrec', lambda = 'lambda'}}
   --end
   local _, exprs = se.unpack(expr, {n = 1, tail = true})
   local bindings = se.empty
   local function done()
      if se.is_empty(bindings) then
         -- 'begin' is common, so optimize lack of defs case
         return {'let*',{l(), exprs}}
      else
         return {'letrec', {r(bindings), exprs}}
      end
   end
   while true do
      if se.is_empty(exprs) then
         return done()
      end
      local expr, rest = se.unpack(exprs, {n = 1, tail = true})
      if type(expr) == 'table' and expr[1] == 'define' then
         -- For now we only support (define (name ...) ...)
         local _, spec, fun_body = se.unpack(expr, { n = 2, tail = true })
         local name, args = se.unpack(spec, { n = 1, tail = true })
         assert(type(name) == 'string')
         bindings = {l(name, {'lambda',{args,fun_body}}), bindings}
         exprs = rest
      elseif type(expr) == 'table' and expr[1] == 'import' then
         -- Splice import form
         if not ext then ext = ".sm" end
         local _, name = se.unpack(expr, { n = 2 })
         assert(type(name) == 'string')
         local filename = name .. ext
         local import_exprs = se.read_file_multi(filename)
         exprs = se.append(import_exprs, exprs)
      else
         return done()
      end
   end
end

-- Implement letrec on top of let* and set!
macro['letrec'] = function(expr)
   local _, bindings, exprs = se.unpack(expr, {n = 2, tail = true})
   if se.is_empty(bindings) then
      -- Base case is needed to avoid letrec->begin->letrec loop.
      return {'let*',{l(),exprs}}
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
         return l("set!", name, val)
      end,
      bindings)
   return {'let*', {void_bindings, {{'begin', set_variables}, exprs}}}
end



return macro
