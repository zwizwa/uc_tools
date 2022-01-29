-- I'd like to isolate these passes:
--
-- MACRO-EXPAND
-- A-NORMAL FORM
-- VARIABLE RENAMING
-- OUTPUT CLEANUP
--
-- Macro expansion and ANF seem to go hand-in hand.

local se = require('lib.se')
local comp = require('lib.comp')

local ins = table.insert
local a2l = se.array_to_list
local l = se.list



-- Bind macros to state object for gensym.
local macro = {} ; do
   for name, m in pairs(require('lib.scheme_macros')) do
      log("MACRO: " .. name .. "\n")
      macro[name] = function(s, expr)
         return m(expr, { state = s })
      end
   end
end

-- Define types
local function expr_type(e)
   local typ = type(e)
   if typ == 'table' then
      if e[1] ~= nil and e[2] ~= nil then
         return 'pair'
      else
         log_desc(e)
         error('bad table')
      end
   end
   return typ
end

local function s_id(s, thing) return thing end

local expander = {
   ['string'] = s_id,
   ['number'] = s_id,
   ['pair'] = function(s, expr)
      local car, cdr = unpack(expr)
      local m = s.macro[car]
      if m ~= nil then
         return m(s, expr), true
      else
         return expr
      end
   end
}

local function trace(tag, expr)
   log('\n')
   log_se_n(expr, tag .. ": ")
end


local function expand_step(s, expr)
   trace("STEP",expr)
   local typ = expr_type(expr)
   local f = expander[typ]
   if f == nil then error('expand: bad type ' .. typ) end
   return f(s, expr)
end

local function expand(s, expr)
   local again = true
   while again do
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
         return l(var, s:eval(expr))
      end
      local function tx_form(seqform)
         return s:eval(seqform)
      end
      return {'block',
              {se.map(tx_binding, bindings),
               se.map(tx_form, forms)}}
   end,
   ['set!'] = function(s, expr)
      local _, var, vexpr = se.unpack(expr, {n = 3})
      return l('set!', var, s:eval(vexpr))
   end,
   ['lambda'] = function(s, expr)
      local _, vars, body = se.unpack(expr, {n = 2, tail = true})
      return l('lambda', vars, s:eval({'begin',body}))
   end
}

local function prim_val(expr)
   -- FIXME: Define this better
   return type(expr) ~= 'table'
end

local function apply(s, expr)
   trace("APPLY",expr)
   local normalform = {}
   local bindings = {}
   for e in se.elements(expr) do
      if not prim_val(e) then
         local sym = s:gensym()
         ins(bindings, l(sym, s:eval(e)))
         ins(normalform, sym)
      else
         ins(normalform, e)
      end
   end
   if #bindings == 0 then
      return expr
   else
      return l('block',a2l(bindings),a2l(normalform))
   end
   return expr
end

local evaluator = {
   ['string'] = s_id,
   ['number'] = s_id,
   ['pair'] = function(s, expr)
      local car, cdr = unpack(expr)
      local f = s.form[car]
      if f ~= nil then
         return f(s, expr), true
      else
         return apply(s, expr)
      end
   end
}
local function eval(s, expr)
   expr = expand(s, expr)
   trace("EVAL",expr)
   local typ = expr_type(expr)
   local f = evaluator[typ]
   if f == nil then error('eval: bad type ' .. typ) end
   return f(s, expr)
end
local function gensym(s, prefix)
   s.count = s.count + 1
   return (prefix or "r") .. s.count
end

local class = {
   expand_step = expand_step,
   expand = expand,
   macro = macro,
   form = form,
   eval = eval,
   gensym = gensym,
}

local function new()
   local obj = { count = 0 }
   setmetatable(obj, { __index = class })
   return obj
end

class.new = new
return class

