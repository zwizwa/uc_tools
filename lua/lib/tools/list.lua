local list = {}

-- apply function to all elements in an array and return result
function list.map(fun, arr)
   assert(fun)
   assert(arr)
   local results = {}
   for i,val in ipairs(arr) do
      results[i] = fun(val)
   end
   return results
end

return list
