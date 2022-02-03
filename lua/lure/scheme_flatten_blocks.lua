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
-- It's crazy to try these without a pattern matcher.
--
-- Also, maybe better to split decl and set!


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

local function s_id(s, thing) return thing end

function flatten_block(s, block_expr)
   local _, bindings, stmts = se.unpack(block_expr, {n=2, tail=true})
   local seq = {} -- Easier to to the accumulation using side-effects.

   -- Transform current expression to "all bindings" form.
   local ign_bindings = se.map(
      function(stmt) return l('_', stmt) end,
      stmts)
   local all_bindings = se.append(bindings, ign_bindings)

   for binding in se.elements(all_bindings) do
      if se.length(binding) == 1 then
         ins(seq, l(car(binding), '#<void>'))
      else
         local var, vexpr = se.unpack(binding, {n=2})
         -- Let the compiler flatten any sub blocks.
         local cexpr = s:compile(vexpr)
         if se.is_expr(cexpr, 'block') then
            -- Everything that bubbles up does not have a sequence
            -- part: all statements are converted to '_' bindings.
            local _, bindings = se.unpack(cexpr, {n=2})
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
   return l('block',a2l(seq))

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
   ['string'] = s_id,
   ['number'] = s_id,
   ['boolean'] = s_id,
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

