-- Test file for uc_tools/hs/UCTools/Lua.hs
local q
local m = { test = { } }

local function localfun(a) return a + 1 end

q = 123
m.test.a = 456

function m.funct(a) return a + 1 end
function m:meth(a)  return a + 1 end

m.test.b = {1,2,c=123,3}

m.lam = function(x) return x * x end

-- local m = {
--    test = {
--       101, 102, 103,
--       k = 123,
--       a = { b = 456 },
--       104, 105, 106,
--    }
-- }
-- m.c = 567
return m, q, 1+2, m.lam(3)
