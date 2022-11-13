-- Forward mode automatic differentiation using dual numbers
-- https://en.wikipedia.org/wiki/Dual_number
-- https://en.wikipedia.org/wiki/Automatic_differentiation

-- This is a wrapper around a base type, so no fancy representation is
-- necessary.  Just use a tuple.
local function dual(val, diff)
   return { val, diff }
end
local function undual(n)
   return unpack(n)
end

return function(c)
   local m = {}
   local function lift1(f)
      return function(_a)
         local a,da = undual(_a)
         return dual(f(a,da))
      end
   end
   local function lift2(f)
      return function(_a, _b)
         local a,da = undual(_a)
         local b,db = undual(_b)
         return dual(f(a,da,b,db))
      end
   end
   m.add = lift2(function(a, da, b, db)
         return c.add(a, b),
                c.add(da, db) end)
   m.sub = lift2(function(a, da, b, db)
         return c.sub(a, b),
                c.sub(da, db) end)
   m.mul = lift2(function(a, da, b, db)
         return c.mul(a, b),
                c.add(c.mul(a, db),
                      c.mul(b, da)) end)
   m.sin = lift1(function(a, da)
         return c.sin(a),
                c.mul(da, c.cos(a)) end)
   m.cos = lift1(function(a, da)
         return c.cos(a),
                c.mul(c.neg(da), c.sin(a)) end)
   m.exp = lift1(function(a, da)
         local expa = c.exp(a)
         return expa,
                c.mul(da, expa)
   end)
   return m;
end

