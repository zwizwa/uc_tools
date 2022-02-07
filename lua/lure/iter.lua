-- Unroll an iterator (generator)
local function take(iter, nb)
   local arr = {}
   while true do
      if #arr >= nb then break end
      local val = iter()
      if not val then break end
      ins(arr, val)
   end
   return arr
end

return {
   take = take
}
