-- Test for generic traversal using se_match.
-- The idea is that the traversal pattern IS the data structure.
-- The s-expressions are just the medium in which this is embedded.
-- So what we do here is define an iteration pattern for the base language.
-- But first, dotted syntax is necessary.
local se = require('lure.se')
local match = require('lure.match')
local l = se.list

local class = {}

function class.compile(s, expr)
   return s.match(
      expr,
      {
         {function(p) return {'block',{p.bindings,p.body}} end,
          function(m) return {'block',{m.bindings,m.body}} end},

         {function(p) return l('lambda',p.args,p.body) end,
          function(m) return l('lambda',m.args,m.body) end},

         {function(p) return p.other end,
          function(m) return m.other end}
      }
   )
end

local function new()
   local obj = { match = match.match }
   setmetatable(obj, { __index = class })
   return obj
end
class.new = new
return class

