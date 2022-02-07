local tab = {}
local ins = table.insert

-- Invert key <-> value
function tab.invert(t, into)
   into = into or {}
   for k,v in pairs(t) do
      into[v] = k
   end
   return into
end

-- Shallow copy
function tab.copy(t, into)
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




return tab
