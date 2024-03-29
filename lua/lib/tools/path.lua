-- Convert between C style nested data structure references, paths as
-- lists, and Lua nested data structures.

-- Part of uc_tools/lua -- https://opensource.org/licenses/MIT

-- The basic idea is that nested data structures are isomorphic to
-- path-indexed flat data structures.  This module provides converters
-- between the two, and related operations.

-- In addition, C style array references are supported.

-- Example:

-- A nested struct { a = { b = { c = 123 } } } is isomorphic to the
-- flat structure { 'a.b.c' = 123 }.  We use Lua string concatenation
-- to represent paths, since they can be used as table keys, and path
-- lists like { 'a', 'b', 'c' } cannot.  However, path lists are still
-- used to manipulate these data structures.

-- The path separator is configurable, but we will refer to 'dotted'
-- as the form that has keys concatenated separated by the separator
-- character.

-- Note: This is a companion to the the TAG_U32 idea in uc_tools C
-- code, which replaces data structures with iteration on resource
-- constrained devices.

return function(cfg)
cfg = cfg or {}
local separator = cfg.separator or '.'
local left_bracket = cfg.left_bracket or '['
local right_bracket = cfg.left_bracket or ']'
local array_base = cfg.array_base or 0



-- Parse a C style array reference into name and one-base
-- Lua array coordinates.  Array base is configurable.

-- Exmaples:
-- 'port[0]' -> {'port',{1}}
-- 'a[1][2]' -> {'a',{2,3}}

-- FIXME: This could use some parse error handling.
local function parse_array_ref(str)
   local name = {}
   local coord_nb = nil
   local coords = {}
   for i=1,#str do
      local char = str:sub(i,i)
      if char == left_bracket then
         coord_nb = #coords + 1
         coords[coord_nb] = {}
      elseif char == right_bracket then
         -- just ignore
      elseif nil == coord_nb then
         -- still parsing name
         table.insert(name, char)
      else
         -- parsing coord
         table.insert(coords[coord_nb], char)
      end
   end
   -- log_desc({name,coords})
   local name_str = table.concat(name)
   local coord_nums = {}
   for i=1,#coords do
      local num = tonumber(table.concat(coords[i]))
      assert(num)
      coord_nums[i] = num
   end
   -- log_desc({name_str, coord_strs})
   return name_str, coord_nums
end

-- FIXME: This could use some parse error handling.
local function dotted_to_list(str)
   local keys = {}
   for key in string.gmatch(str, "([^" .. separator .. "]+)") do
      -- Support C array referencing syntax.
      if key:sub(#key,#key) == right_bracket then
         local key1, coords = parse_array_ref(key)
         -- log_desc({parse_array_ref = {key1, coords}})
         table.insert(keys, key1)
         for i=1,#coords do
            table.insert(keys, (1 - array_base) + coords[i])
         end
      else
         table.insert(keys, key)
      end
   end
   -- log_desc({dotted_to_list = keys})
   return keys
end

-- Re-introduce C syntax
local function prefix_key(prefix, key)
   local tkey = type(key)
   local prefix1
   if prefix == '' then
      -- Skip separator at the beginning
      prefix1 = prefix
   else
      -- Add separator after existing key.
      prefix1 = prefix .. separator
   end

   if tkey == 'string' then
      return prefix1 .. key
   elseif tkey == 'number' then
      return prefix .. left_bracket .. (key - (1 - array_base)) .. right_bracket
   else
      error('unsuported key type: ' .. tkey)
   end
end

-- Convert nested table to flat table with dotted keys.
local function nested_to_flat(nested_tab)
   local flat_tab = {}
   local function paths_inc(nested_tab, prefix)
      for key, val in pairs(nested_tab) do
         local prefixed = prefix_key(prefix, key)
         if type(val) == 'table' then
            paths_inc(val, prefixed)
         else
            flat_tab[prefixed] = val
         end
      end
   end
   paths_inc(nested_tab, "")
   -- log_desc({flat_tab = flat_tab})
   return flat_tab
end

-- Set a value in a nested table.
local function nested_put(tab, key_list, val)
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

-- Convert flat table with dotted keys to nested table.
local function flat_to_nested(flat_tab)
   local nested_tab = {}
   for dotted_key, val in pairs(flat_tab) do
      local key_list = dotted_to_list(dotted_key)
      -- log_desc({key_list=key_list})
      nested_put(nested_tab, key_list, val)
   end
   return nested_tab
end

-- Use Lua metatable to reify lua table dereferencing function (higher
-- order syntax) into a data structure.
--
-- Example: indexer_to_nested(function(m) return m.a.b.c end)}
-- Returns: {'a','b','c'}
--
-- Just showing off a hat trick here, but idea might be useful at some
-- point.
local function indexer_to_nested(dereference)
   local path = {}
   local function index(_dummy_tab, key)
      if nil ~= key then table.insert(path, key) end
      local probe = {}
      setmetatable(probe, {__index = index})
      return probe
   end
   dereference(index())
   return path
end





return {
         dotted_to_list = dotted_to_list,
         nested_to_flat = nested_to_flat,
         flat_to_nested = flat_to_nested,
         indexer_to_nested = indexer_to_nested,
}

end

