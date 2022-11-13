-- Use the matcher to implement type unification.
--
local se       = require('lure.se')
local se_match = require('lure.se_match')
local match    = se_match.new()

local function var(a) return se.list('var', a) end


-- FIXME: The environment updates need to be checked still

local function unify_env(env, a, b)
   local function unify(x, y) return unify_env(env, x, y) end
   return match(
      se.list(a, b),
      {
       {"((var ,a) (var ,b))",
        function(m)
           assert(type(m.a) == 'string')
           assert(type(m.b) == 'string')
           -- Store as indirection
           env[m.a] = var(m.b)
           env[m.b] = var(m.a)
           return true
        end},
       {"((var ,a) ,b)",
        function(m)
           assert(type(m.a) == 'string')
           env[m.a] = m.b
           return true
        end},
       {"(,a (var ,b))",
        function(m)
           assert(type(m.b) == 'string')
           env[m.b] = m.a
           return true
        end},
       {"((,cons_a . ,args_a) (,cons_b . ,args_b))",
        function(m)
           -- log_desc({cons_match=m})
           -- Syntax errors are fatal.
           assert(type(m.cons_a) == 'string')
           assert(type(m.cons_b) == 'string')
           -- Check that type constructors match.
           if m.cons_a ~= m.cons_b then return false end
           local accu = true
           se.zip(
              function(a, b)
                 -- FIXME: Early abort would be ok here.
                 accu = accu and unify(a, b)
              end,
              m.args_a, m.args_b)
           return accu
        end},
       {"(,a ,b)",
        function(m)
           -- log_desc({else_match=m})
           -- FIXME: This is a little rough, but works for now.
           return m.a == m.b
        end}})
end

local function vec(a,n) return se.list('vec', a, n) end

local function test()
   require('lure.log')
   local function check(a, b)
      local env = {}
      local rv = unify_env(env,a,b)
      return log_desc({a=a,b=b,rv=rv,env=env})
   end
   check(var('x'), var('y'))
   check(var('x'), 'int')
   check('int', var('y'))
   check('int', 'int')
   check('int', 'float')
   check(vec('int',3),vec('int',3))
end

return {
   unify = unify,
   test = test,
}

