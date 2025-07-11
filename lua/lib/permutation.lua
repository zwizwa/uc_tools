-- Permutations represented as lua 1-based array indices.
-- E.g.
-- . identity {1,2}
-- . 2-element swap is {2,1}
local m = {}

function m.invert(p)
   local inv = {}
   for i,j in ipairs(p) do
      inv[j] = i
   end
   return inv
end

-- Apply permutation p to x (which can be an arbitrary vector, or
-- itself a permutation)
function m.apply(p, x)
   local n = #p
   assert(n == #x)
   local rv = {}
   for i=1,n do
      rv[i] = x[p[i]]
   end
   return rv
end


function m.test()
   local p

   -- First, get the forward/reverse right.
   -- The way permutations are encoded
   p = {2, 3, 1}

   -- Applying a permutation to a vector will construct a new vector where
   -- Element 1 is taken from old position 2
   -- Element 2 is taken from old position 3
   -- Element 3 is taken from old position 1
   local els = {'A','B','C'}
   log_desc({p=p,
             els=els,
             p_els=m.apply(p,els),
             pp_els=m.apply(p, m.apply(p,els)),
   })



   p = p or { 1, 2, 3, 7, 8, 5, 6, 9, 10, 11, 12, 4, }  -- real world example
   local ip = m.invert(p)
   local c1 = m.apply(p, ip)
   local c2 = m.apply(ip, p)
   log_desc({p=p,ip=ip,c1=c1,c2=c2})
end

return m
