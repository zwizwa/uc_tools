-- Escap primtive forms.  The output can then be fed back into
-- the Scheme frontend to create an interpretation of a particular IR.


local se        = require('lure.se')
local se_match  = require('lure.se_match')
local lure_comp = require('lure.comp')
local l = se.list
local a2l = se.array_to_list
local ins = table.insert

local class = {}

-- Cherry-pick some methods (micro-mixin?)
class.parameterize = lure_comp.parameterize
class.indented     = lure_comp.indented
class.tab          = lure_comp.tab

function class.compile(s,expr)
   return s:comp(expr)
end

function class.comp(s,expr)
   return s.match(
      expr,
      {
         {"(block . ,bindings)", function(m)
             local bindings =
                se.map(
                   function(binding)
                      return s.match(
                         binding,
                         {{"(,var ,expr)",
                           function(m)
                              return l(m.var, s:comp(m.expr))
                           end}})
                   end,
                   m.bindings)
             return {'block@', bindings}
         end},
         {"(lambda ,vars ,expr)", function(m)
             return l('lambda@', m.vars, s:comp(m.expr))}
         end},
         {"(if ,cond ,etrue ,efalse)", function(m)
             return l('if@', m.cond,
                      s:comp(m.etrue),
                      s:comp(m.efalse))
         end},
         {",other", function(m)
             return m.other
         end}
      }
   )
end

local function new()
   local obj = { match = se_match.new() }
   setmetatable(obj, { __index = class })
   return obj
end
class.new = new
return class

