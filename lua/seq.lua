#!./lua.sh

-- Core idea in seq is closing over register feedback.
-- How to do it in Lua?

-- Take a state machine (s -> m (s, o)) and an initial value of type
-- s, and convert it into just m o.

-- In seq this is done in a monad.  Not going to use monads in Lua.
-- Can it be tucked away in a context?

require 'lure.log'

local function prog(c, i)
   local function counter_sm(s)
      return c.add(s, c.one), s
   end
   local counter = c.close(0, counter_sm)
   return counter -- c.add(counter, i)
end


-- Interpretation as Lua stateful processors, abstracted as functions,
-- with hidden state.

-- There's a missing ingredient.  What is the rep of a signal?  Pair
-- of init and update.  It's really about signals, not processors.
-- Then pure operations are just lifted.

-- But there is still confusion between signal and value.

local function pure(const)
   return {
      init = {}, -- No state
      update = function(s) return {}, const end
   }
end

local function liftA2(f)
   return function(a, b)
      log_desc({a=a,b=b})
      return {
         init = {a.init, b.init},
         update = function(z_s)
            local z_sa, z_sb = unpack(z_s)
            local sa, oa = a.update(z_sa)
            local sb, ob = b.update(z_sb)
            return {sa, sb}, f(oa, ob)
         end,
      }
   end
end

-- FIXME: Not correct
local function close(init, update)
   return {
      init = init,
      update = update,
   }
end

local c = {
   one   = pure(1),
   add   = liftA2(function(a,b) return a+b end),
   close = close
}

local f = prog(c, pure(0))
log_desc({
      f,
      f.update(f.init),
})

