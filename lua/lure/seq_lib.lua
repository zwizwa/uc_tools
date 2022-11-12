-- Library of stream processor functions.  The parameter 'c' contains
-- the language semantics, i.e. it gives access to primitive functions
-- and special forms.

-- FIXME: Return a map with functions instead.  That makes it possible
-- to add some metadata.
return function(c)
   local lib = {}

   -- This function builds a counter stream using the recursion
   -- operator on an init value and an update function.
   function lib.counter(init, inc)
      init = init or 0
      inc  = inc  or 1
      -- The 'rec' or recursion operator creates a stream from an
      -- update function.
      return c.rec1(
         -- Inital state variable.
         init,
         -- Update function, takes current state...
         function(state)
            return
               -- and returns next state...
               state + inc,
               -- and stream value
               state
         end)
   end

   -- C code generation does not support scalar values.  This can be
   -- worked around by wrapping a processor body in a 1-element
   -- vector.
   function lib.scalar(thunk)
      return c.vec(1, function(_) return thunk() end)
   end



   -- Some notes about audio filters.
   --
   -- To limit bookkeeping, the initial state values are assumed to be
   -- set to the steady state of a zero input signal.  If possible,
   -- this should be encoded as 0.


   -- First order lowpass.
   function lib.lp1(coef, init)
      init = init or 0
      -- This function maps an input stream to an output stream.
      -- The state signal is only visible inside the update function.
      return function(input)
         return c.rec1(
            init,
            function(state)
               local next_state = state + coef * (input - state)
               local output = state
               return next_state, output
            end)
      end
   end


   return lib
end
