-- Use the matcher to implement unification.
--
local se       = require('lure.se')
local se_match = require('lure.se_match')
local match    = se_match.new()

local function unify(...)
   return match(
      se.list(...),
      {{"((var ,a) (var ,b))",
        function(m)
           return m
        end},
       {"(,a ,b)",
        function(m)
           return m
        end}})
end

local function var(a) return se.list('var', a) end

local function test()
   require('lure.log')
   log_desc(unify(var('x'), var('y')))
end

return {
   unify = unify,
   test = test,
}

