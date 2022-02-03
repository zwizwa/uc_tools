-- Shared language front-end
-- Preprocessor that performs the following:
--
-- MACRO-EXPAND
-- A-NORMAL FORM
-- VARIABLE RENAMING (FIXME)
-- SIMPLIFY BLOCK (TODO)
-- SINGLE LAMBDA EXPR (TODO)
--
-- Separate later passes:
--
-- BLOCK FLATTENING
--
-- OUTPUT CLEANUP / LANG PPRINT
--
-- Macro expansion and ANF seem to go hand-in hand.
-- Variable renaming is useful for later block-flattening.

local se = require('lure.se')
local comp = require('lure.comp')

local ins = table.insert
local a2l = se.array_to_list
local l = se.list



-- Bind macros to state object for gensym.
local macro = {} ; do
   for name, m in pairs(require('lure.scheme_macros')) do
      -- log("MACRO: " .. name .. "\n")
      macro[name] = function(s, expr)
         return m(expr, { state = s })
      end
   end
end


local function s_id(s, thing) return thing end

local expander = {
   ['string'] = s_id,
   ['number'] = s_id,
   ['pair'] = function(s, expr)
      local car, cdr = unpack(expr)
      local m = s.macro[car]
      if m ~= nil then
         return m(s, expr), 'again'
      else
         return expr
      end
   end
}

local function trace(tag, expr)
   -- log('\n') ; log_se_n(expr, tag .. ": ")
end


local function expand_step(s, expr)
   trace("STEP",expr)
   local typ = se.expr_type(expr)
   local f = expander[typ]
   if f == nil then error('expand: bad type ' .. typ) end
   return f(s, expr)
end

local function expand(s, expr)
   local again = 'again'
   while again do
      assert(again == 'again')
      expr, again = expand_step(s, expr)
   end
   return expr
end

local void = '#<void>'

local form = {
   ['block'] = function(s, expr)
      local _, bindings, forms = se.unpack(expr, {n = 2, tail = true})
      local function tx_binding(binding)
         local var, expr = comp.unpack_binding(binding, void)
         trace('BINDING',var)
         local cexpr = s:compile(expr)
         local rvar = s:rename(var)
         return l(rvar, cexpr)
      end
      local function tx_form(seqform)
         return s:compile(seqform)
      end
      return {'block',
              {se.map(tx_binding, bindings),
               se.map(tx_form, forms)}}
   end,
   ['set!'] = function(s, expr)
      local _, var, vexpr = se.unpack(expr, {n = 3})
      return s:anf(
         l(vexpr),
         function(e) return l('set!', s:rename(var), se.car(e)) end)
   end,
   ['if'] = function(s, expr)
      local _, econd, etrue, efalse = se.unpack(expr, {n = 4})
      return s:anf(
         l(econd),
         function(e) return l('if', se.car(e), s:compile(etrue), s:compile(efalse)) end)

   end,
   ['lambda'] = function(s, expr)
      local _, vars, body = se.unpack(expr, {n = 2, tail = true})
      local rvars = se.map(function(v) return s:rename(v) end, vars)
      return l('lambda', rvars, s:compile({'begin',body}))
   end
}


local function anf(s, exprs, fn)
   local normalform = {}
   local bindings = {}
   for e in se.elements(exprs) do
      -- FIXME: This should compile before checking if it's primitive.
      if type(e) == 'string' then
         ins(normalform, s:rename(e))
      elseif type(e) == 'table' then
         -- Composite
         local sym = s:gensym()
         ins(bindings, l(sym, s:compile(e)))
         ins(normalform, sym)
      else
         -- Other values
         ins(normalform, e)
      end
   end
   if #bindings == 0 then
      return fn(a2l(normalform))
   else
      return l('block',
               a2l(bindings),
               fn(a2l(normalform)))
   end
end

local function apply(s, expr)
   trace("APPLY", expr)
   return anf(s, expr, function(e) return e end)
end

local compiler = {
   ['string'] = s_id,
   ['number'] = s_id,
   ['pair'] = function(s, expr)
      local car, cdr = unpack(expr)
      local f = s.form[car]
      if f ~= nil then
         return f(s, expr)
      else
         return apply(s, expr)
      end
   end
}
local function compile(s, expr)
   expr = expand(s, expr)
   trace("COMPILE",expr)
   local typ = se.expr_type(expr)
   local f = compiler[typ]
   if f == nil then error('compile: bad type ' .. typ) end
   return f(s, expr)
end

local function gensym(s, prefix)
   -- Gensyms should never clash with source variables.  We can't
   -- guarantee that atm.  FIXME.
   s.count = s.count + 1
   local sym = (prefix or "r") .. s.count
   s.gensyms[sym] = true
   return sym
end

-- FIXME: Incorrect!
-- This needs to track lexical scope.
local function rename(s, var)
   assert(type(var) == 'string')
   if s.gensyms[var] then return var end
   local sym = s:gensym()
   s.renamed[sym] = var
   return sym
end



local class = {
   expand_step = expand_step,
   expand = expand,
   macro = macro,
   form = form,
   compile = compile,
   gensym = gensym,
   rename = rename,
   anf = anf,
}

local function new()
   local obj = { count = 0, renamed = {}, gensyms = {} }
   setmetatable(obj, { __index = class })
   return obj
end

class.new = new
return class

