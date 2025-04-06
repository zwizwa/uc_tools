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

function list.concat(list_of_lists)
   local result = {}
   for i,list in ipairs(list_of_lists) do
      for j,el in ipairs(list) do
         table.insert(result, el)
      end
   end
   return result
end


return list
