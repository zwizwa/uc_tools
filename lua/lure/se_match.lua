-- Sugared "string DSL" matcher in style of match.lua
-- See test_se_match.lua for an example

local se_match = {}
local string_dsl = require('lure.string_dsl')
local match      = require('lure.match')
local se         = require('lure.se')

local memo_eval = string_dsl.memo_eval

-- str -> cons -> pat
local function pquasiquote(s,str)
   local expr = se.read_string(str)
   local function cons(probe) return se.qq_eval(probe, expr) end
   return match.compile(cons)
end

local function do_se_match(s, expr, string_clauses)
   for _,clause in ipairs(string_clauses) do
      local spat, fhandle = unpack(clause)
      local cpat = memo_eval(s, pquasiquote, spat)
      local m = match.apply(cpat, expr)
      if m then return fhandle(m) end
   end
   -- FIXME: Make this configurable?
   error('pattern mismatch')
   -- return false
end

function se_match.new(config)
   local s = { memo = {} }
   for k,v in pairs(config or {}) do s[k] = v end
   return function(expr, string_clauses)
      return do_se_match(s, expr, string_clauses)
   end
end

return se_match
