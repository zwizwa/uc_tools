-- FIXME: Probably best to remove this in favor of scheme_match.lua

-- Test for generic traversal using se_match.
-- The idea is that the traversal pattern IS the data structure.
-- The s-expressions are just the medium in which this is embedded.
-- So what we do here is define an iteration pattern for the base language.

-- FIXME: Memoize construction also?

local se = require('lib.se')
local se_match = require('lib.se_match')
local l = se.list

local class = {}

function compile(s,expr)
   return s.match(
      expr,
      {
         {"(block ,bindings . ,body)",
          function(m)
             return
                {'block',
                 {m.bindings,
                  s:compile(m.body)}}
          end},
         {"(lambda ,bindings . ,body)",
          function(m)
             return
                {'lambda',
                 {m.bindings,
                  s:compile(m.body)}}
          end},
         {"(unquote other)",
          function(m)
             return m.other
          end}
      }
   )
end
class.compile = compile

local function new()
   local obj = { match = se_match.new() }
   setmetatable(obj, { __index = class })
   return obj
end
class.new = new
return class

