-- Use the matcher to implement type unification.
--
local se       = require('lure.se')
local se_match = require('lure.se_match')
local match    = se_match.new()

local function var(a) return se.list('var', a) end

local function unify(...)
   return match(
      se.list(...),
      {{"((var ,a) (var ,b))",
        function(m)
           return se.list('=', m.a, m.b)
        end},
       {"((var ,a) ,b)",
        function(m)
           return {[m.a] = m.b}
        end},
       {"(,a (var ,b))",
        function(m)
           return {[m.b] = m.a}
        end},
       {"((,cons_a . ,args_a) (,cons_b . ,args_b)",
        function(m)
           log_desc({cons_match=m})
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
           log_desc({else_match=m})
           -- FIXME: This is a little rough, but works for now.
           return m.a == m.b
        end}})
end

local function vec(a,n) return se.list('vec', a, n) end

local function test()
   require('lure.log')
   local function check(a, b)
      return log_desc({a,b,unify(a,b)})
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

