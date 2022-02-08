-- Compiler from block language to State Machine, with all lambdas
-- combined with combinator forms, all tail calls replaced by goto,
-- and all other functions other than primitives inlined.

local se       = require('lure.se')
local comp     = require('lure.comp')
local se_match = require('lure.se_match')

local l2a = se.list_to_array
local l = se.list

local class = {}

class.parameterize = comp.parameterize

local void = "#<void>"

local function trace(tag, expr)
   -- log_se_n(expr, tag .. ":")
end

local function ifte(c,t,f)
   if c then return t else return f end
end

function frame(args, env)
   return {args, env}
end

function class.comp(s, expr)
   return s.match(
      expr,
      {
         {"(block . ,bindings)", function(m)
             return {'block',se.map(
                function(binding)
                   return s.match(
                      binding,
                      {{"(,var ,expr)", function(b)
                           return l(b.var, s:comp(b.expr))
                       end}})
                end, m.bindings)}
         end},
         {"(if ,c ,t ,f)", function(m)
             return l('if', m.c, s:comp(m.t), s:comp(m.f))
         end},
         {"(lambda ,args ,body)", function(m)
             return s:parameterize(
                { env = frame(m.args, s.env) },
                function()
                   return l('lambda', m.args, s:comp(m.body))
             end)
         end},
         {",other", function(m)
             return expr
         end},
   })
end

function class.compile(s,expr)
   return s:comp(expr)
end

function class.new()
   local s = { match = se_match.new()  }
   setmetatable(s, {__index = class})
   return s
end

return class
