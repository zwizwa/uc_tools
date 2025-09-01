require 'lure.log'
local function f()
   return 1,2
end

log_desc({f(),f()})

local a,b,c,d = f(), f()
log_desc({a=a,b=b,c=c,d=d})

log_desc(f() + f())
