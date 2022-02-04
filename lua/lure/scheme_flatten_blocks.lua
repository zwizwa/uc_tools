-- Block flattener.
--
-- Note that this needs variable renaming because otherwise a variable
-- from a deeper block can potentially shadow one in a higher up
-- block.
--
-- (block
--   (a (block ((b c) (d e)) f g h))
--   ...)
--
-- (block
--   (b c)
--   (d e)
--   (_ f)
--   (_ g)
--   (a h)
--   ...)
--
-- Maybe it's simpler to just do on s-expressions.  Indeed: first
-- flatten the inner blocks, then for each block bound to a variable,
-- perform the transformation above.


-- In addition this should do
-- (block ((a (if (block b c) (block d e)))))
--
-- (block
--   ((a)
--   (_ (if (block ((_ b)
--                 (_ (set! a c)))
--          (block ((_ d)
--                 (_ (set! a e)))
--      ))
-- )
--
-- Maybe better to split decl and set!


local se = require('lure.se')
local iolist = require('lure.iolist')
local comp = require('lure.comp')

local ins = table.insert
local a2l = se.array_to_list
local l = se.list
local car = se.car
local cadr = se.cadr
local cdr = se.cdr


-- Define types

function flatten_block(s, block_expr)
   local _, bindings = se.unpack(block_expr, {n=1, tail=true})
   local seq = {} -- Easier to to the accumulation using side-effects.

   for binding in se.elements(bindings) do
      if se.length(binding) == 1 then
         ins(seq, l(car(binding), '#<void>'))
      else
         local var, vexpr = se.unpack(binding, {n=2})
         -- Let the compiler flatten any sub blocks.
         local cexpr = s:compile(vexpr)
         if se.is_expr(cexpr, 'block') then
            -- Everything that bubbles up does not have a sequence
            -- part: all statements are converted to '_' bindings.
            local bindings = se.cdr(cexpr)
            for binding, rest in se.elements(bindings) do
               if not se.is_empty(rest) then
                  ins(seq, binding)
               else
                  -- Last one is special.  It can not already have a
                  -- binding associated.
                  assert('_' == car(binding))
                  ins(seq, l(var, cadr(binding)))
               end
            end
         else
            ins(seq, l(var, cexpr))
         end
      end
   end
   return {'block',a2l(seq)}

end

local compile_form = {
   ['block'] = flatten_block,
   -- Forms that are not in this table are treated same as pp_app
   ['lambda'] = function(s, expr)
      local _, vars, body = se.unpack(expr, {n=2, tail=true})
      return {'lambda',{vars,se.map(function(e) return s:compile(e) end, body)}}
   end,
   ['if'] = function(s, expr)
      local _, var, etrue, efalse = se.unpack(expr, {n=4})
      return l('if',var,s:compile(etrue),s:compile(efalse))
   end,
}
local function default(s, expr)
   return expr
end

local compiler = {
   ['pair'] = function(s, expr)
      local car, cdr = unpack(expr)
      local comp = s.compile_form[car]
      if comp ~= nil then
         return comp(s, expr)
      else
         return default(s, expr)
      end
   end
}
local function compile(s, expr)
   local typ = se.expr_type(expr)
   local f = compiler[typ]
   if f then
      return f(s, expr)
   else
      -- No change
      return expr
   end
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

