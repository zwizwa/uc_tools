local tab = {}

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

return tab
