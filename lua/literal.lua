require('lure.log')
local se = require('lure.se')
local l = {}
local function w(thing)
   io.write(thing)
end
function l.print(...)
   local list = se.list(...)
   w("=> ")
   for el, nxt in se.elements(list) do
      w(el)
      if se.is_pair(nxt) then w(", ") end
   end
   w("\n")
end
return l
