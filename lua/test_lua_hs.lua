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

local a

a = 0
repeat
   a = a + 1
   m.repeat_a = a
until a > 3

a = 3
while a ~= 0 do
   a = a - 1
   m.while_a = a
end

do
   a = 123
   m.do_a = a
end

a = 0
if a == 0 then
   m.if_a = 0
else
   n.if_a = 123
end


for i=1,10 do
   m.for_i = i
end


return m, q, 1+2, m.lam(3)
