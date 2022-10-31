#!./lua.sh

-- Core idea in seq is closing over register feedback.
-- How to do it in Lua?

-- Take a state machine (s -> m (s, o)) and an initial value of type
-- s, and convert it into just m o.

-- In seq this is done in a monad.  Not going to use monads in Lua.
-- Can it be tucked away in a context?

require 'lure.log'

local function prog1(c, i)
   local function counter_sm(s)
      return c.add(s, c.one), s
   end
   local counter = c.close(0, counter_sm)
   return counter -- c.add(counter, i)
end

local function prog2(c, i)
   function update(s) return c.add1(s), s end,
   return c.close(0, update)
end




-- Implementation: a signal is a lazy list.  It cannot just be a
-- generator because a signal might be evaluated multiple times.







-- FIXME: The "pure" implementation doesn't work because there needs
-- to be an idea of monad that can host the state signals.

-- Interpretation as Lua stateful processors, abstracted as functions,
-- with hidden state.

-- How to represent a signal in Lua?  First attempt tried to use init,
-- update pair.  I ran into an issue with trying to write down close.
-- It seems some knot tying is needed.  ( FIXME: Clarify this! )


local function pure(const)
   return {
      init = {}, -- No state
      update = function(s) return {}, const end
   }
end

local function lift1(f)
   return function(a)
      return {
         init = a.init,
         update = function(z_sa)
            local sa, oa = a.update(z_sa)
            return sa, f(oa)
         end,
      }
   end
end

local function lift2(f)
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

-- The main confusion here is that the update function passed to close
-- operates on signals, while the update function in the
-- representation operates on values.  What close does is to glue
-- those together.

local function close(init, sig_update)
   return {
      -- The initial value of the signal is just given
      init = init,
      -- The value update method needs to be dervied from the signal
      -- update method by probing it.
      update = function(s_val)
         local s_sig = {
         }
         local s1_sig, out_sig = sig_update(s_sig)
      end
   }
end

local c = {
   one   = pure(1),
   add1  = lift1(function(a) return a+1 end),
   add   = lift2(function(a,b) return a+b end),
   close = close
}

local f = prog(c, pure(0))
log_desc({
      f,
      f.update(f.init),
})

