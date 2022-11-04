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


return tab
