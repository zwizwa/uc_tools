-- This is called map, but is not clear if that is a good name.  I've
-- added the assert(new_el) to preserve the 'map' semantics that the
-- structure of the iteration doesn't change.

local function map(fn, iter)
   return function()
      local els = {iter()}
      if els[1] == nil then return nil end
      local new_els = {fn(unpack(els))}
      assert(new_els[1])
      return unpack(new_el)
   end
end

-- For more general transformations, use filter, which can drop
-- elements by returning nil (but can not cut off a stream).
local function filtermap(fn, iter)
   return function()
      while true do
         local els = {iter()}
         if els[1] == nil then return nil end
         local new_els = {fn(unpack(els))}
         if new_els[1] ~= nil then return unpack(new_els) end
      end
   end
end

-- To cut off a stream an even more general processor is needed that
-- returns a control code.  FIXME: Implement that when needed.

return {
   map = map,
   filtermap = filtermap,
}
