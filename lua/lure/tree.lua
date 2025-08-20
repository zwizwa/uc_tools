local tab = require('lure.tab')

local function is_equal(a, b)
   local aks = tab.keys(a)
   local bks = tab.keys(b)
   if (#aks ~= #bks) then
      -- Different number of keys.
      return false
   end
   -- If the number of keys match it is sufficient to check that for
   -- every key in a there is a match in b.
   for k,ak in pairs(a) do
      local bk = b[k]
      if ak ~= bk then
         -- The only correct scenario here is that both are tables.
         if type(ak) ~= 'table' then return false end
         if type(bk) ~= 'table' then return false end
         if not is_equal(ak,bk) then
            -- Deep compare failed
            return false
         end
      end
   end
   return true
end

return {
   is_equal = is_equal
}
