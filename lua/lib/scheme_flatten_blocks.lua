-- Block flattener.
--
-- Note that this needs variable renaming (really?)
--
-- (block
--   (a (block () d e f))
--   ...)
--
-- (block
--   (_ d)
--   (_ e)
--   (a f)
--   ...)
--
-- FIXME: Don't do this one without a test suite.  Also, might be
-- simpler in CPS.  I made it work before in smc.lua using s.var as
-- "current hole".

local se = require('lib.se')
local iolist = require('lib.iolist')
local comp = require('lib.comp')

local ins = table.insert
local a2l = se.array_to_list
local l = se.list
local car = se.car
local cadr = se.cadr
local cdr = se.cdr


-- Define types

local function s_id(s, thing) return thing end

-- Extend blocks to use '_' bindings for statments, placing variable
-- declarations and statements on the same footing, which is the case
-- for all target languages.

function flatten(s, block_expr)
   local _, bindings, stmts = se.unpack(block_expr, {n=2, tail=true})
   for binding in se.elements(bindings) do
      if se.length(binding) == 1 then
         ins(s.seq, l(car(binding), '#<void>'))
      else
         local var, vexpr = se.unpack(binding, {n=2})
         ins(s.seq, l(var, s:compile(vexpr)))
      end
   end
   for stmt, rest in se.elements(stmts) do
      if (type(stmt) == 'table') and stmt[1] == 'block' then
         flatten(s, stmt)
      else
         local v = ifte(is_empty(stat), s.var, '_')
         ins(s.seq, l(v, s:compile(stmt)))
      end
   end
   -- This doesn't return anything.  Everything is in s.seq
end

local compile_form = {
   ['block'] = function(s, expr)
      local _, bindings, stmts = se.unpack(expr, {n=2, tail=true})
      local seq = {}
      s:parameterize(
         { seq = seq },
         function()
            s:flatten(expr)
         end)
      return l('block',a2l(seq))
   end,
   -- Forms that are not in this table are treated same as pp_app
   ['lambda'] = function(s, expr)
      local _, vars, body = se.unpack(expr, {n=2, tail=true})
      return {'lambda',{vars,se.map(function(e) return s:compile(e) end, body)}}
   end
}
local function default(s, expr)
   return expr
end

local compiler = {
   ['string'] = s_id,
   ['number'] = s_id,
   ['boolean'] = s_id,
   ['pair'] = function(s, expr)
      local car, cdr = unpack(expr)
      local pp = s.compile_form[car]
      if pp ~= nil then
         return pp(s, expr)
      else
         return default(s, expr)
      end
   end
}
local function compile(s, expr)
   local typ = se.expr_type(expr)
   local f = compiler[typ]
   if f == nil then error('compile: bad type ' .. typ) end
   return f(s, expr)
end

local class = {
   compile = compile,
   compile_form = compile_form,
   parameterize = comp.parameterize,
   flatten = flatten,
}
local function new()
   local obj = { }
   setmetatable(obj, { __index = class })
   return obj
end
class.new = new
return class

