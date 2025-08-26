-- Test file for uc_tools/hs/UCTools/Lua.hs
local m = {
   test = {
      1, 2, 3,
      k = 123,
      a = { b = 456 },
      4, 5, 6,
   }
}
m.c = 567
function m.funk(a) return a + 1 end
return m
