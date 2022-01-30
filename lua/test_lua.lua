-- pairs does everything, including the "array indices"
-- ipairs seems to do 1,2,... until it encounters nil

require('lib.log')
local function test(tab)
   log("\nTAB:\n")
   log_desc(tab)
   log("pairs:\n")
   for k,v in pairs(tab) do
      log_desc({k=k,v=v})
   end
   log("ipairs:\n")
   for k,v in ipairs(tab) do
      log_desc({k=k,v=v})
   end
end
test({a = 123, b = 345})
test({5,6,7})
test({a = 123,[1] = 5, [2] = 6})

