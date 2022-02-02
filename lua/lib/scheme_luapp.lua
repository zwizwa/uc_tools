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

function class.compile(s,expr)
   return s.match(
      expr,
      {
         {"(block ,bindings)",
          function(m)
             for binding in se.elements(m.bindings) do
                s.match(
                   binding,
                   {{"(_ ,expr)",
                      function(b)
                         s:compile(b.expr)
                         s:w("\n")
                      end},
                    {"(,var ,expr)",
                     function(b)
                        s:w(b.var, " = ")
                        s:compile(b.expr)
                        s:w("\n")
                     end}})
             end
         end},
         {"(lambda ,bindings . ,body)",
          function(m)
             s:w("FIXME:lambda\n")
             return
                {'lambda',
                 {m.bindings,
                  s:compile(m.body)}}
          end},
         {"(set! ,var ,val)",
          function(m)
             s:w(m.var, " = ", m.val)
          end},
         {"(unquote other)",
          function(m)
             if type(m.other) == 'number' then
                s:w(m.other)
             else
                log_se_n(expr,"BAD: ")
                error("syntax error")
             end
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

