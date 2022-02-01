-- Operations on sets, represented as element to boolean maps.  Some
-- of these are meaningful on ordinary maps, as long as the values
-- evaluate as true.  When relevant, values of the rightmost map are
-- retained.

local set = {}

function set.from_list(l)
   local s = {}
   for i,e in ipairs(l) do
      s[e] = true
   end
   return s
end

function set.intersect(a,b) -- a ^ b
   local s = {}
   for k,v in pairs(a) do
      if a[k] then
         s[k]=v
      end
   end
   return s
end

function set.union(a,b) -- a v b
   local s = {}
   for k,v in pairs(a) do s[k]=v end
   for k,v in pairs(b) do s[k]=v end
   return s
end


function set.empty(a)
   -- is there a better way than iterating?
   local n = 0
   for k,v in pairs(a) do n=n+1 end
   return n == 0
end

function set.subtract(a,b) -- a \ b
   local s = {}
   for k,v in pairs(a) do
      if not b[k] then
         s[k]=v
      end
   end
   return s
end

-- Determine the ordering of two sets.  Sets are a partial order.
-- This operation is only well-defined if one is a subset of the
-- other.
function set.compare(a,b)
   local empty_a_min_b = set.empty(set.subtract(a,b))
   local empty_b_min_a = set.empty(set.subtract(b,a))

   -- not table means table is empty
   if empty_a_min_b and empty_b_min_a then return 'eq' end
   if empty_a_min_b then return 'lt' end
   if empty_b_min_a then return 'gt' end
   -- not defined: these sets cannot be ordered
   return 'nd'
end

-- This function can be plugged into table.sort
function set.lt(a,b)
   local cmp = set.compare(a,b)
   -- Throw an error if the ordering between a and b is not defined.
   assert('nd' ~= cmp)
   return (cmp == 'lt')
end


-- Convenience function to compare two lists
function set.eq_lists(a, b)
   return 'eq' == set.compare(set.from_list(a), set.from_list(b))
end

function set.map(fn, set)
   local new_set = {}
   for k,v in pairs(set) do
      if v then
         new_set[fn(k)] = true
      end
   end
   return new_set
end

return set
