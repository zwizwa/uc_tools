-- Sugared "string DSL" matches in style of match.lua
-- See test_hoas_match.lua for an example

-- FIXME: Rename this file to match_dsl and smatch to lua_match.  Keep
-- se_match.

local smatch = {}
local string_dsl = require('lure.string_dsl')
local match      = require('lure.match')

local memo_eval = string_dsl.memo_eval
local lambda    = string_dsl.lambda
local function plambda(s,str)
   return match.compile(lambda(s,str))
end

local function do_smatch(expr, string_clauses, s)
   assert(s)
   local ctx_var = s.ctx_var or '_'
   for _,clause in ipairs(string_clauses) do
      local spat, shandle = unpack(clause)
      local cpat = memo_eval(s, plambda, spat)
      local m = match.apply(cpat, expr)
      if m then
         local fhandle = memo_eval(s, lambda, shandle)
         return fhandle(m)
      end
   end
   return false
end
function smatch.smatcher(config)
   local obj = { memo = {} }
   for k,v in pairs(config) do obj[k] = v end
   return function(expr, string_clauses)
      return do_smatch(expr, string_clauses, obj)
   end
end



return smatch
