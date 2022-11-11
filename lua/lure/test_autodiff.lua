require('lure.log')
local autodiff = require('lure.autodiff')
function run()
   local n=0
   local function new_var() n=n+1 ; return n end
   local dag = {}
   local function bind(op, ...)
      local var = new_var()
      dag[var] = {op, {...}}
      return var
   end
   local function op2(op) return function(a,b) return bind(op,a,b) end end
   local function op1(op) return function(a)   return bind(op,a)   end end
   local c = {
      add = op2('add'),
      sub = op2('sub'),
      mul = op2('mul'),
      sin = op1('sin'),
      cos = op1('cos'),
      neg = op1('neg'),
   }
   local c1 = autodiff(c)
   log_desc(c1)
   log_desc(c1.add({'a','da'},{'b','db'}))
   log_desc(c1.sin({'a','da'}))
   log_desc({dag = dag})
end
return { run = run }
