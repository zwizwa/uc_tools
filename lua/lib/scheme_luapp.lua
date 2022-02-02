-- Lua code pretty-printer from lambda, block, if, set!
-- Based on se_match

local se       = require('lib.se')
local se_match = require('lib.se_match')
local iolist   = require('lib.iolist')
local comp     = require('lib.comp')
local l = se.list

local class = {}

-- Cherry-pick some methods (micro-mixin?)
class.parameterize = comp.parameterize
class.indented     = comp.indented
class.tab          = comp.tab
class.w            = comp.w


-- Convention for newline/tab:

-- Expressions are always compiled in binding position, are already at
-- indented position and should not print newline.

function class.w_bindings(s, bindings)
   s:indented(
      function()
         for binding in se.elements(bindings) do
            s:w(s:tab())
            s.match(
               binding,
               {{"(_ ,expr)", function(b)
                    s:i_comp(b.expr)
                end},
                  {"(,var ,expr)", function(b)
                      s:w("local ", b.var, " = ")
                      s:i_comp(b.expr)
                      -- FIXME: print orig var name in comment
            end}})
            s:w("\n")
         end
   end)
end
function class.w_body(s, expr)
   s.match(
      expr,
      -- The do .. end block can be omitted in a function body.
      {{"(block ,bindings)", function(m)
           s:w_bindings(m.bindings)
       end},
       {"(unquote body)", function(m)
           s:i_comp(m.body)
      end}})
end

-- Top level entry point
function class.compile(s,expr)
   -- Toplevel block can be removed.  We assume the toplevel
   -- expression is a block representing a Lua module.
   s.indent = -1 -- Undo indent in w_bindings
   s.match(
      expr,
      {{"(block ,bindings)", function(m)
           s:w_bindings(m.bindings)
      end}})
   s:w("\n")
end

function class.i_comp(s, expr)
   s:indented(function() s:comp(expr) end)
end

-- Recursive expression compiler.
function class.comp(s,expr)
   s.match(
      expr,
      {
         -- Reduced block form where all statements have been included
         -- as bindings to '_' to indicate ignored value.
         {"(block ,bindings)", function(m)
             s:w(s:tab(),"do\n")
             s:w_bindings(m.bindings)
             s:w(s:tab(),"end\n")
         end},
         -- Reduced lambda form with single body expression..
         -- Second form is generic.
         {"(lambda ,vars ,expr)", function(m)
             s:w("function()\n")
             s:w_body(m.expr)
             s:w(s:tab(),"end")
         end},
         {"(set! ,var ,val)", function(m)
             s:w(m.var, " = ", m.val)
         end},
         {",atom", function(m)
             if type(m.atom) == 'number' then
                s:w(m.atom)
             elseif type(m.atom) == 'string' then
                s:w(m.atom)
             else
                log_se_n(expr,"BAD: ")
                error("syntax error")
             end
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

