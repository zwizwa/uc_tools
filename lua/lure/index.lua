-- Do more with "index abstraction".  It's quite useful.

-- Basically, don't represent a 2D (ore higher dimensional) table
-- using nested data structures.  Represent it as an index function
-- instead.
--
-- One application is that it makes transpose trivial: swap the indices.


-- In macro expansion it is fairly typical to want to transpose tables
-- like tab[i][j] -> tab[j][i]
--
-- This could be done on the table itself...
-- {a = {q = 1, r = 2}, b = {q = 3, r = 4}} ->
-- {q = {a = 1, b = 3}, r = {a = 2, b = 4}}
--
-- But it seems better to implement such operations on indices.
function index1(tab) return function(i)   return tab[i]    end end
function index2(tab) return function(i,j) return tab[i][j] end end

function transpose(index)
   return function(i,j) return index2(j,i) end
end

function to_array(index, nb)
   assert(nb)
   local arr = {}
   for i=1,nb do arr[i] = index(i) end
   return arr
end

return {
   index2 = index2,
   to_array = to_array,
   transpose = transpose,
}
