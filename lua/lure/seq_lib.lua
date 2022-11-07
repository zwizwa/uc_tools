-- Library of stream processor functions.  The parameter 'c' contains
-- the language semantics, i.e. it gives access to primitive functions
-- and special forms.
return function(c)
   local lib = {}

   -- This function builds a counter stream using the recursion
   -- operator on an init value and an update function.
   function lib.counter(init, inc)
      init = init or 0
      inc  = inc  or 1
      -- The 'rec' or recursion operator creates a stream from an
      -- update function.
      return c.rec(
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
      return function(input)
         return c.rec(
            init,
            function(state)
               local next_state = state + c * (input - state)
               return next_state, state
            end)
      end
   end


   return lib
end
