local se       = require('lure.se')
local unify    = require('lure.unify')
local tab      = require('lure.tab')

local var = unify.var
local function vec(a,n) return se.list('vec', a, n) end

local function run()
   require('lure.log')
   local function log_se_tab(env)
      for _,k in ipairs(tab.sorted_keys(env)) do
         log(k .. " = ")
         log_se(env[k])
         log("\n")
      end
   end

   local function uni(env,a,b)
      log("\n")
      local rv = unify.unify_env(env,a,b)
      log_se_tab({a=a,b=b})
      log_se_tab(env)
      return rv
   end

   -- Basic tests
   uni({}, var('x'), var('y'))
   uni({}, var('x'), 'int')
   uni({}, 'int', var('y'))
   uni({}, 'int', 'int')
   uni({}, 'int', 'float')
   uni({}, vec('int',3), vec('int',3))

   -- Accumulating tests
   local env

   env = {}
   uni(env, var('x'), vec('int',3))
   uni(env, var('y'), vec('int',3))
   for k,v in pairs(env) do
      log(k .. " = ")
      log_se(v)
      log("\n")
   end

   -- Some actual tests with asserts.
   env = {}
   assert(true == uni(env, vec(vec(var('x'),3),4), vec(vec('int',3),4)))
   assert(env.x == 'int')

   env = {}
   assert(false == uni(env, vec(vec(var('x'),3),4), vec(vec('int',3),400)))
   log_desc(env)
   -- This is predictable, but is a side effect of the recursion and
   -- is definitely not something to rely on.  Just here to make this
   -- point.  Variable gets bound before the failure occurs.
   assert(env.x == 'int') -- read ^^^

   -- FIXME: Too tired to think this through, so probably best to
   -- first integrate it in seq.lua and see where it goes wrong, then
   -- add cases here and fixe the algo.


end


return { run = run }
