-- Use the matcher to implement type unification.
--
local se       = require('lure.se')
local se_match = require('lure.se_match')
local match    = se_match.new()

local function var(a) return se.list('var', a) end
local function is_var(v) return se.is_expr(v,'var') end


-- FIXME: The environment updates need to be checked still

local function unify_env(env, a, b)
   local function unify(x, y) return unify_env(env, x, y) end

   local function unify_var(v, e)
      assert(type(v) == 'string')
      assert(not is_var(e))
      local e0 = env[v]
      -- Unbound, then bound it to the expression
      if e0 == nil then env[v] = e ; return true end
      -- FIXME:
      -- Already bound: here we need to normalize (create a-symmetry
      -- to avoid loops), define one variable in terms of another one.
      return unify(e0, e)
   end

   return match(
      se.list(a, b),
      {
       {"((var ,a) (var ,b))",
        function(m)
           assert(type(m.a) == 'string')
           assert(type(m.b) == 'string')
           -- There isn't any more information
           -- Store as indirection
           -- FIXME: I need to see examples to make sense of this.
           -- For now assert
           assert(not env[m.a])
           assert(not env[m.b])
           env[m.a] = var(m.b)
           env[m.b] = var(m.a)
           return true
        end},
       {"((var ,a) ,b)",
        function(m)
           return unify_var(m.a, b)
        end},
       {"(,a (var ,b))",
        function(m)
           return unify_var(m.b, a)
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

return {
   unify_env = unify_env,
   var = var,
}

