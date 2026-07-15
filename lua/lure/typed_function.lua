-- Experiment function

local m = {}
m.metatable = {
   __call = function(self, ...) return self.fun(...) end
}
function m.typed_function(fun_type, fun)
   local tab = { fun = fun, fun_type = fun_type }
   setmetatable(tab, m.metatable)
   return tab
end
return m
