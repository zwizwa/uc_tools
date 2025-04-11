local list = {}

-- apply function to all elements in an array and return result
function list.map(fun, arr, start)
   assert(fun)
   assert(arr)
   local results = {}
   start = start or 1
   for i=start,#arr do
      results[i-start+1] = fun(arr[i])
   end
   return results
end

-- same, but also provide list index
function list.imap(fun, arr, start)
   assert(fun)
   assert(arr)
   local results = {}
   start = start or 1
   for i=start,#arr do
      results[i-start+1] = fun(i, arr[i])
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
