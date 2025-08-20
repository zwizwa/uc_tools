local tab = {}
function tab.invert(t)
   inv = {}
   for k,v in pairs(t) do
      inv[v] = k
   end
   return inv
end

-- Shallow copy
function tab.copy1(t, into)
   into = into or {}
   for k,v in pairs(t) do
      into[k] = v
   end
   return into
end

function tab.map(fun, inp)
   assert(fun)
   assert(inp)
   local outp = {}
   for k,v in pairs(inp) do
      outp[k] = fun(v)
   end
   return outp
end

-- Similar to set.union, but mutates the first tab
function tab:add_defaults(defaults)
   if not self then self = {} end
   for k,v in pairs(defaults) do
      if not self[k] then self[k] = v end
   end
   return self
end

-- FIXME: Fix when alert.
-- Split in two: wrapper function, and iterator.
-- -- Flatten a table of list | string into list, dereferencing strings.
-- function tab.flatten(dict, key, into_list)
--    if (nil == into_list) then
--       into_list = {}
--    end
--    -- Dereference
--    local list_or_string = dict[key]
--    while (type(list_or_string) == 'string') do
--       list_or_string = dict[key]
--    end

--    if (type(list_
--    assert(list_or_string)
--    if (type(list_or_string) == 'string') then
--       return flatten(dict, list_or_string, into_list)
--    else
--       error(type(list_or_string))
--       return into_list
--    end
-- end

function tab:keys()
   local keys = {}
   for k, _ in pairs(self) do table.insert(keys, k) end
   return keys
end
function tab:sorted_keys()
   local keys = tab.keys(self)
   table.sort(keys)
   return keys
end


-- BUG?
-- function tab:sorted_pairs()
--    local keys = tab.sorted_keys(self)
--    local iter = ipairs(keys)
--    log_desc({iter=iter,keys=keys})
--    return function()
--       local _, key = iter()
--       if key ~= nil then
--          return key, self[key]
--       end
--    end
-- end

function tab.is_empty(t)
   return not next(t)
end

-- Return elements that are different in new and old, with value from new.
function tab.updated(new, old)
   local d = {}
   for k,v in pairs(new) do
      if v ~= old[k] then
         d[k] = v
      end
   end
   return d
end


-- Nested tables.  Note that lib/tools/path.lua does something else:
-- it doesn't use arrays as nested table paths but uses concatenated
-- strings, so I've moved these functions here from path.lua
function tab.nested_put(tab, key_list, val)
   local sub_tab = tab
   local n = #key_list
   assert(n > 0)
   -- Descend into table, automatically creating subtables.
   for i=1,n-1 do
      assert(type(sub_tab) == 'table')
      local key = key_list[i]
      if sub_tab[key] == nil then
         sub_tab[key] = {}
      end
      sub_tab = sub_tab[key]
   end
   -- Set the value in the inner table
   sub_tab[key_list[n]] = val
end

-- Get a value in a nested table.
function tab.nested_get(tab, key_list)
   local sub_tab = tab
   local n = #key_list
   assert(n > 0)
   -- Descend into table, stopping descent when subtree is not
   -- defined.
   for i=1,n-1 do
      assert(type(sub_tab) == 'table')
      local key = key_list[i]
      if sub_tab[key] == nil then
         return nil
      end
      sub_tab = sub_tab[key]
   end
   -- Get the value from the inner table
   return sub_tab[key_list[n]]
end

-- Iterate over (key_list, value) pairs in a nested table.
function tab.for_nested(nested, fn)
   local keylist = {}
   local function walk(thing)
      if type(thing) == 'table' then
         for k,v in pairs(thing) do
            table.insert(keylist, k)
            walk(v)
            table.remove(keylist)
         end
      else
         fn(keylist, thing)
      end
   end
   walk(nested)
end


return tab
