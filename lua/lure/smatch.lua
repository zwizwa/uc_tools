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

-- On memo_eval args:
-- 1: memoization state
-- 2: function to apply to arg (this is the string->thing compiler)
-- 3: the syntax string to compile
--
-- This happens 2 times: once for the pattern and once for the
-- handler, using two different compilation functions, so they need
-- separate memoization states.

local function do_smatch(expr,
                         string_clauses,
                         memo_pattern,
                         memo_handle)
   --log_desc({string_clauses=string_clauses})
   assert(memo_pattern)
   assert(memo_handle)
   for _,clause in ipairs(string_clauses) do
      local str_pattern, str_handle = unpack(clause)
      local cpat = memo_eval(memo_pattern, plambda, str_pattern)
      -- log_desc({cpat=cpat})
      local m = match.apply(cpat, expr)
      if m then
         local fhandle = memo_eval(memo_handle, lambda, str_handle)
         -- log_desc({fhandle=fhandle})
         return fhandle(m)
      end
   end
   return false
end
function smatch.smatcher(config)
   -- There are two memo_eval instances: one for pattern compilation
   -- and one for handler compilation.
   local memo_pattern = { memo = {} }
   local memo_handle  = { memo = {} }
   for k,v in pairs(config) do
      -- This is for k = env, var
      --
      -- Both evaluators use the same constructor environment and the
      -- same variable name for the argument of the wrapper function.
      --
      memo_pattern[k] = v
      memo_handle[k] = v
   end
   return function(expr, string_clauses)
      return do_smatch(expr,
                       string_clauses,
                       memo_pattern,
                       memo_handle)
   end
end



return smatch
