local list = {}

-- get table keys as list
function list.keys(s)
   local l = {}
   for k,v in pairs(s) do
      table.insert(l, k)
   end
   return l
end

-- convert set representation back to list
list.from_set = list.keys

-- split list according to predicate
function list.split(l, pred)
   local d = {t = {}, f = {}}
   for i,e in ipairs(l) do
      if(pred(e)) then
         table.insert(d.t, e)
      else
         table.insert(d.f, e)
      end
   end
   return d
end

-- keys, optionally sorted
function list.keys_sorted(map)
   local ks = list.from_set(map)
   table.sort(ks)
   return ks
end

-- get keys from a map, sort by value of key
function list.keys_by_member(map, sort_key)
   local ks = list.keys(map)
   local function lt(a,b)
      return map[a][sort_key] < map[b][sort_key]
   end
   table.sort(ks, lt)
   return ks
end

-- apply function to all elements in an array and return result
local function map(fun, arr, start)
   assert(fun)
   assert(arr)
   local results = {}
   start = start or 1
   for i=start,#arr do
      results[i-start+1] = fun(arr[i])
   end
   return results
end
list.map = map

-- same, but also provide list index
local function imap(fun, arr, start)
   assert(fun)
   assert(arr)
   local results = {}
   start = start or 1
   for i=start,#arr do
      results[i-start+1] = fun(i, arr[i])
   end
   return results
end
list.imap = imap

-- filter elements in array
local function filter(fun, arr)
   assert(fun)
   assert(arr)
   local results = {}
   for _,el in ipairs(arr) do
      if fun(el) then table.insert(results, el) end
   end
   return results
end
list.filter = filter


local function concat(list_of_lists)
   local result = {}
   for i,list in ipairs(list_of_lists) do
      for j,el in ipairs(list) do
         table.insert(result, el)
      end
   end
   return result
end
list.concat = concat



-- map an object's method
function list.mapm(object, method_name, arr)
   return map(function(el) return object:method_name(el) end, arr)
end

-- convert map to list of pairs
-- key set is normalized by sorting
function list.pairs_from_table(map)
   local keys = list.keyts_sorted(map)
   local lst = {}
   for _,key in ipairs(keys) do
      table.insert(lst, {key, map[key]})
   end
   return lst
end


-- convert an array to a set (table mapping keys to true).
function list.to_set(arr)
   local tab = {}
   for _,el in ipairs(arr) do
      tab[el] = true
   end
   return tab
end


function list.append(...)
   local rv = {}
   for _,l in ipairs({...}) do
      for _,el in ipairs(l) do
         table.insert(rv, el)
      end
   end
   return rv
end

return list


