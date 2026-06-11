-- I keep running into data representation inconsistency when it comes
-- to representing raw binary data.  This can be done as Lua strings,
-- as arrays of numbers, as hex strings, and in one of my applications
-- also as an array where each byte is wrapped as a table object.
-- Basically this is unworkable, so here is an attempt to make local
-- concrete representation easier, e.g. allow a choice to be made
-- inside a single function or collection of functions what a data
-- packet looks like, and on entry just convert it or error out.
--
-- Functions are split in two parts:
-- . Specific a_to_b() functions are provided
-- . These are combined into any->type methods (main purpose of this module)

local m = {}

-- Type names:
-- bin:    Lua string containing binary data
-- array:  Lua table containing a 1-base array of numbers

-- Include specific from/to types.
local function array_to_bin(arr)
   assert(arr)
   local strs = {}
   for i=1,#arr do strs[i] = string.char(arr[i]) end
   local str = table.concat(strs)
   -- log_desc({array_to_bin_str=str})
   return str
end
function bin_to_array(bin)
   assert(bin)
   local arr = {}
   for i=1,#bin do arr[i] = string.byte(bin, i) end
   return arr
end

m.array_to_bin = array_to_bin
m.bin_to_array = m.bin_to_array


-- Include any to type conversions.
function m.bin(thing)
   local t = type(thing)
   if t  == 'string' then return thing end
   if t  == 'table'  then return array_to_bin(thing) end
   error("as.bin: bad type: " .. t)
end

function m.array(thing)
   local t = type(thing)
   if t  == 'table'  then return thing end
   if t  == 'string' then return bin_to_array(thing) end
   error("as.bin: bad type: " .. t)
end


function m.array_of_numbers()
end
m.aon = m.array_of_numbers


function m.sized_array(thing, size, pad)
   assert(size)
   assert(pad ~= nil)
   local arr = m.array(thing)
   local rv = {}
   for i=1,size do
      local el = arr[i]
      if el == nil then el = pad end
      rv[i] = el
   end
   return rv
end

return m
