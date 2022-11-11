-- Forward mode automatic differentiation using dual numbers
-- https://en.wikipedia.org/wiki/Dual_number

-- This is a wrapper around a base type, so no fancy representation is
-- necessary.  Just use a tuple.
local function dual(val, diff)
   local n = { val, diff }
   return n
end
local function unpack_dual(n)
   return unpack(n)
end

return function(c)
   local m = {}
   local function lift2(f)
      return function(a, b)
         local a_0, a_e = unpack_dual(a)
         local b_0, b_e = unpack_dual(b)
         local r_0, r_e = f(a_0, a_e, b_0, b_e)
         return dual(r_0, r_e)
      end
   end
   m.add = lift2(function(a_0, a_e, b_0, b_e)
         return c.add(a_0, b_0),
                c.add(a_e, b_e) end)
   m.sub = lift2(function(a_0, a_e, b_0, b_e)
         return c.sub(a_0, b_0),
                c.sub(a_e, b_e) end)
   m.mul = lift2(function(a_0, a_e, b_0, b_e)
         return c.mul(a_0, b_0),
                c.add(c.mul(a_0, b_e),
                      c.mul(b_0, a_e)) end)
   return m;
end

