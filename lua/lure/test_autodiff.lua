require('lure.log')
local autodiff = require('lure.autodiff')
function run()
   local c = {}
   function c.add(a,b) return {'add',a,b} end
   function c.sub(a,b) return {'sub',a,b} end
   function c.mul(a,b) return {'mul',a,b} end
   local c1 = autodiff(c)
   log_desc(c1)
   log_desc(c1.add({'a0','ae'},{'b0','be'}))
end
return { run = run }
