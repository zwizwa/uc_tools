-- Liveness analysis
--
-- For each variable, keep track of which expression referenced it
-- last.  Then in a second pass insert 'free' hints.

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

function class.comp_bindings(s,bindings)
   return se.map(
      function(binding)
         local var, vexpr = se.unpack(binding, {n=2})
         return l(var, s:comp(vexpr))
      end,
      bindings)
end

function class.comp(s,expr)
   return s.match(
      expr,
      {
         {"(block . ,bindings)", function(m)
             trace("BLOCK", expr)
             return {'block', s:comp_bindings(m.bindings)}
         end},
         {"(lambda ,vars ,expr)", function(m)
             trace("LAMBDA", expr)
             return s:parameterize(
                {var = scheme_frontend.make_var('return')},
                function()
                   return l('lambda', m.vars, s:comp(m.expr))
                end)
         end},
         {"(if ,cond ,etrue ,efalse)", function(m)
             trace("IF", expr)
             return l('if', m.cond,
                      s:comp(m.etrue),
                      s:comp(m.efalse))
         end},
         {"(app ,fun . ,args)", function(m)
             trace("APP", expr)
             local fun  = s:comp(m.fun)
             local args = se.map(function(a) return s:comp(a) end, m.args)
             return {'app', {fun, args}}
         end},
         {"(hint . ,_)", function(m)
             return expr
         end},
         {",other", function(m)
             if (se.expr_type(m.other) ~= var) then
                return expr
             else
                -- FIXME: Yeah where to put the information?
                -- In order to mark the expression, we need to keep
                -- track of the last expression as well!
                -- This is not trival to represent here...
                return expr
             end
         end}
      }
   )
end

local function new()
   local obj = { match = se_match.new(), indent = 0 }
   setmetatable(obj, { __index = class })
   return obj
end
class.new = new
return class

