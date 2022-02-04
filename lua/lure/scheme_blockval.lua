-- Add explicit assignment at end of blocks.

-- FIXME: All these need expression result assignment and
-- explicit return.  Solve that in a preprocessing step, e.g:

-- (lambda () (block (
--   (a #<nil>)
--   (_ (if cond
--        (set! a 1)
--        (set! a 2)))
--   (return a)
--   ))

-- Doing assignment first.  'return' is a special case.


local se        = require('lure.se')
local se_match  = require('lure.se_match')
local iolist    = require('lure.iolist')
local lure_comp = require('lure.comp')
local l = se.list
local a2l = se.array_to_list
local ins = table.insert

local scheme_frontend = require('lure.scheme_frontend')

local class = {}

-- Cherry-pick some methods (micro-mixin?)
class.parameterize = lure_comp.parameterize
class.indented     = lure_comp.indented
class.tab          = lure_comp.tab


local function trace(tag, expr)
   -- log('\n') ; log_se_n(expr, tag .. ": ")
end

function class.compile(s,expr)
   return s:comp(expr)
end

function ifte(a,b,c)
   if a then return b else return c end
end


-- The "continuation" (the place to store the block return value) is
-- passed down nested 'if' and 'block' expressions.  All other
-- expressions are either function calls, which respect the regular
-- binding setup, or 'set!' which doesn't produce a result.  No
-- control flow is changed.

-- Note: trampoline conversion could be done here: at each 'return'
-- point, the expression could be wrapped, returned to the trampoline
-- to then resume operation.  This requires primitives to be
-- distinguished from composite functions.  Basically it would only be
-- necessary for those functions that eventually end up in a loop.

function class.comp_bindings(s,bindings)
   local bs = {}
   local function pass_continuation(expr)
      -- Blocks should have been flattened, but recurse anyway.
      return se.is_expr(expr,'if') or se.is_expr(expr,'block')
   end
   s:parameterize(
      {var = s.var},
      function()
         local cont_var = s.var
         for binding, rest in se.elements(bindings) do
            local var, expr = se.unpack(binding, {n=2})
            local last = se.is_empty(rest)
            trace("BIND",l(last, cont_var or "void", var, expr))
            local cexpr
            if last then
               assert(var == '_')
               if cont_var then
                  if pass_continuation(expr) then
                     s.var = cont_var
                     cexpr = s:compile(expr)
                  else
                     if cont_var.unique == 'return' then
                        cexpr = l('return',s:compile(expr))
                     else
                        cexpr = l('set!',cont_var,s:compile(expr))
                     end
                  end
               else
                  -- Don't change anything if the return value is
                  -- ignored.
                  cexpr = s:compile(expr)
               end
            else
               if pass_continuation(expr) and var ~= '_' then
                  -- Split the binding up in a declaration and a
                  -- continuation passed downwards.
                  s.var = var
                  ins(bs, l(var, scheme_frontend.void))
                  var = '_'
               else
                  -- Bind primitive value, or execute block or if
                  -- without storing value.
                  s.var = nil
               end
               cexpr = s:compile(expr)
            end
            ins(bs, l(var, cexpr))
         end
      end)
   return a2l(bs)
end

-- Insert a block to make comp_bindings work
function need_block(expr)
   if se.is_expr(expr,'block') then return expr end
   return l('block',l('_',expr))
end

function class.comp(s,expr)
   return s.match(
      expr,
      {
         -- Reduced block form where all statements have been included
         -- as bindings to '_' to indicate ignored value.
         {"(block . ,bindings)", function(m)
             trace("BLOCK", expr)
             return {'block', s:comp_bindings(m.bindings)}
         end},
         -- Reduced lambda form with single body expression..
         -- Second form is generic.
         {"(lambda ,vars ,expr)", function(m)
             trace("LAMBDA", expr)
             return s:parameterize(
                {var = scheme_frontend.make_var('return')},
                function()
                   return l('lambda', m.vars, s:comp(need_block(m.expr)))
                end)
         end},
         {"(if ,cond ,etrue ,efalse)", function(m)
             trace("IF", expr)
             return l('if', m.cond,
                      s:comp(need_block(m.etrue)),
                      s:comp(need_block(m.efalse)))
         end},
         {",other", function(m)
             trace("OTHER", expr)
             return m.other
         end}
      }
   )
end

local function new()
   -- FIXME: Make sure match raises error on mismatch.
   local obj = { match = se_match.new(), indent = 0 }
   setmetatable(obj, { __index = class })
   return obj
end
class.new = new
return class

