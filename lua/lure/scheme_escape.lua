-- Escape primtive forms + remove var objects such that the output can
-- then be fed back into the Scheme frontend to create an
-- interpretation of a particular IR.

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


local function strip_vars(expr)
   local function var2string(expr)
      -- Create a source name from var's unique name + prev source tag.
      local name = expr.unique
      if expr.var then
         name = name .. "." .. expr.var
      end
      return name
   end
   return se.fmap('var', var2string, expr)
end


function class.compile(s,expr)
   return s:comp(strip_vars(expr))
end

function class.comp_bindings(s, bindings)
   return
      se.map(
         function(binding)
            return s.match(
               binding,
               {{"(,var ,expr)",
                 function(m)
                    return l(m.var, s:comp(m.expr))
                 end}})
         end,
         bindings)
end

function class.comp(s,expr)
   local function comp(e) return s:comp(e) end
   return s.match(
      expr,
      {
         {"(block . ,bindings)", function(m)
             return {'block@', s:comp_bindings(m.bindings)}
         end},
         {"(labels ,bindings ,inner)", function(m)
             return l('labels@', s:comp_bindings(m.bindings), s:comp(m.inner))
         end},
         {"(lambda ,vars ,expr)", function(m)
             return l('lambda@', m.vars, s:comp(m.expr))
         end},
         {"(if ,cond ,etrue ,efalse)", function(m)
             return l('if@',
                      s:comp(m.cond),
                      s:comp(m.etrue),
                      s:comp(m.efalse))
         end},
         {"(set! ,var ,val)", function(m)
             return l("set!@", m.var, s:comp(m.val))
         end},
         {"(return ,expr)", function(m)
             -- Note that expr is not necessarily a variable, so
             -- recursion is necessary.
             return l("return@", s:comp(m.expr))
         end},
         {"(goto ,expr)", function(m)
             return l("goto@", s:comp(m.expr))
         end},
         {"(app . ,fun_and_args)", function(m)
             return {'app@', se.map(comp, m.fun_and_args)}
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

