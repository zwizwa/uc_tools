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


local function _trace(tag, expr)
   log('\n') ; log_se_n(expr, tag .. ": ")
end
local function trace(tag, expr)
   -- _trace(tag, expr)
end

function class.compile(s,expr)
   s.label_depth = 0
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
   -- trace("BINDINGS", bindings)
   local bs = {}
   local function bind(var, vexpr)
      assert(var)
      assert(vexpr)
      ins(bs, l(var, vexpr))
   end


   local function pass_continuation(expr)
      -- Blocks should have been flattened, but recurse anyway.
      return se.is_expr(expr,'if')
          or se.is_expr(expr,'labels')
          or se.is_expr(expr,'block')
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
                     cexpr = s:comp(expr)
                     assert(cexpr)
                  else
                     if cont_var.unique == 'return' then
                        cexpr = l('return',s:comp(expr))
                     else
                        cexpr = l('set!',cont_var,s:comp(expr))
                     end
                  end
               else
                  -- Don't change anything if the return value is
                  -- ignored.
                  cexpr = s:comp(expr)
                  assert(cexpr)
               end
            else
               if pass_continuation(expr) and var ~= '_' then
                  -- Split the binding up in a declaration and a
                  -- continuation passed downwards.
                  s.var = var
                  bind(var, scheme_frontend.void)
                  var = '_'
               else
                  -- Bind primitive value, or execute block or if
                  -- without storing value.
                  s.var = nil
               end
               cexpr = s:comp(expr)
               assert(cexpr)
            end
            bind(var, cexpr)
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
   trace("COMP", expr)
   local rv = s.match(
      expr,
      {
         {"(block . ,bindings)", function(m)
             trace("BLOCK", expr)
             return {'block', s:comp_bindings(m.bindings)}
         end},
         {"(labels ,bindings ,inner)", function(m)
             trace("LABELS", expr)
             return s:parameterize(
                { label_depth = s.label_depth + 1 },
                function()
                   local function label_binding(binding)
                      local var, vexpr = se.unpack(binding, {n=2})
                      return l(var, s:comp(vexpr))
                   end
                   return l('labels',
                            se.map(label_binding, m.bindings),
                            s:comp(m.inner))
                end)
         end},
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
   assert(rv)
   return rv
end

local function new(config)
   local obj = { match = se_match.new(), config = config or {} }
   setmetatable(obj, { __index = class })
   return obj
end
class.new = new
return class

