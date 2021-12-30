-- Hey why not.

local se = require('lib.se')
local form = {}
local prim = {}
local scheme = {form = form, prim = prim}
function prim.add(a, b)
   return a + b
end
form['module'] = function(self, expr)
   local _, _, body = se.unpack(expr, { n = 2, tail = true })
   local rv
   return self:eval({'begin', body})
end
form['begin'] = function(self, expr)
   local _, body = se.unpack(expr, { n = 1, tail = true })
   for e in se.elements(body) do
      rv = self:eval(e)
   end
   return rv
end
form['define'] = function(self, expr)
   local _, spec, body = se.unpack(expr, { n = 2, tail = true })
   local name, args = se.unpack(spec, { n = 1, tail = true })
   assert(type(name) == 'string')
   for arg in se.elements(args) do
      assert(type(arg) == 'string')
   end
   self.def[name] = { env = {}, args = args, body = {'begin', body} }
end
function scheme:push(var, val)
   self.env = se.cons({var = var, val = val}, self.env)
end
function scheme:ref(var_name)
   assert(type(var_name) == 'string')
   for v in se.elements(self.env) do
      if v.var == var_name then
         return v.val
      end
   end
   error('undefined var ' .. var_name)
end
function scheme:apply(expr)
   local fun, args = se.unpack(expr, { n = 1, tail = true })
   local arg_vals = {}
   assert(type(fun) == 'string') -- for now
   for el in se.elements(args) do
      local v = self:eval(el)
      assert(v)
      table.insert(arg_vals, v)
   end
   local def = self.def[fun]
   if def then
      return
         self:save_context(
            {'env'},
            function()
               self.env = def.env
               local i = 1
               for arg in se.elements(def.args) do
                  -- io.write(arg .. " = " .. arg_vals[i] .. "\n")
                  self:push(arg, arg_vals[i])
                  i = i + 1
               end
               return self:eval(def.body)
            end)
   end
   local prim = self.prim[fun]
   assert(prim)
   return prim(unpack(arg_vals))
end

function scheme:eval(expr)
   assert(expr)
   if type(expr) == 'number' then return expr end
   if type(expr) == 'string' then return self:ref(expr) end
   assert(type(expr) == 'table')
   local form, tail = unpack(expr)
   assert(form)
   local form_fn = self.form[form]
   if form_fn then
      return form_fn(self, expr)
   else
      return self:apply(expr)
   end
end
function scheme:save_context(keys, inner_fun)
   local saved = {}
   for i,key in ipairs(keys) do saved[key] = self[key]  end
   local rv = inner_fun()
   for i,key in ipairs(keys) do self[key]  = saved[key] end
   return rv
end
function scheme.new()
   local obj = { def = {} }
   setmetatable(obj, {__index = scheme})
   return obj
end

return scheme
