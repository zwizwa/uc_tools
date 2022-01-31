-- Sugared "string DSL" matches in style of match.lua
-- See test_hoas_match.lua for an example

-- FIXME: Rename this file to match_dsl and smatch to lua_match.  Keep
-- se_match.

local smatch = {}
local string_dsl = require('lib.string_dsl')
local match      = require('lib.match')

-- 1. Lua syntax

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

-- 2. S-Expression quasi quoting.
local se = require('lib.se')
local function pquasiquote(s,str)
   local expr = se.read_string(str)
   return match.compile(
      function(probe)
         return se.qq_eval(probe, expr)
      end)
end

-- For this one, the handler clause is the function.  I'm not
-- comfortable yet with relying on Lua evaluation for later
-- compatibility since this will likely be used all over the place.
-- Converting se to string is not an issue.

local function do_se_match(expr, string_clauses, s)
   assert(s)
   local ctx_var = s.ctx_var or '_'
   for _,clause in ipairs(string_clauses) do
      local spat, fhandle = unpack(clause)
      local cpat = memo_eval(s, pquasiquote, spat)
      -- log_desc({cpat = cpat})
      local m = match.apply(cpat, expr)
      if m then return fhandle(m) end
   end
   return false
end
function smatch.se_matcher(config)
   local obj = { memo = {} }
   for k,v in pairs(config) do obj[k] = v end
   return function(expr, string_clauses)
      return do_se_match(expr, string_clauses, obj)
   end
end


return smatch
