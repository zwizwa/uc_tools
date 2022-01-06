-- Hey why not.

-- This was just a toy, but it might actually be useful to have an
-- interpreter for the .sm language.  Something that can perform some
-- extra verification?

-- EDIT: No it will be useful to have a compilation target where "run
-- time" bits can be just Lua, and (Scheme) code can be generated
-- using a more powerful system on top (Racket or Haskell).

local se = require('lib.se')
local form = {}
local scheme = {form = form}
form['begin'] = function(self, expr)
   local _, body = se.unpack(expr, { n = 1, tail = true })
   for e in se.elements(body) do
      rv = self:eval(e)
   end
   return rv
end
form['module'] = function(self, expr)
   local _, _, body = se.unpack(expr, { n = 2, tail = true })
   local rv
   return self:eval({'begin', body})
end
form['lambda'] = function(self, expr)
   local _, args, body = se.unpack(expr, { n = 2, tail = true })
   for arg in se.elements(args) do
      assert(type(arg) == 'string')
   end
   return { env = self.env, args = args, body = {'begin', body} }
end
form['define'] = function(self, expr)
   local _, spec, body = se.unpack(expr, { n = 2, tail = true })
   local name, args = se.unpack(spec, { n = 1, tail = true })
   assert(type(name) == 'string')
   self.def[name] = self:eval({'lambda', {args, body}})
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
function scheme:save_context(keys, inner_fun)
   local saved = {}
   for i,key in ipairs(keys) do saved[key] = self[key] end
   local rv = inner_fun()
   for i,key in ipairs(keys) do self[key] = saved[key] end
   return rv
end
form['let*'] = function(self, expr)
   local _, bindings, body = se.unpack(expr, { n = 2, tail = true })
   return
      self:save_context(
         {'env'},
         function()
            for binding in se.elements(bindings) do
               local var, expr1 = se.unpack(binding, { n = 2 })
               assert(type(var) == 'string')
               local val = self:eval(expr1)
               -- Next binding can already use this variable.
               self:push(var, val)
            end
            return self:eval({'begin', body})
         end)
end
function scheme:apply(fun, args)
   return
      self:save_context(
         {'env'},
         function()
            self.env = fun.env
            local i = 1
            for arg in se.elements(fun.args) do
               self:push(arg, args[i])
               i = i + 1
            end
            return self:eval(fun.body)
         end)
end
function scheme:eval_args(args)
   local arg_vals = {}
   for el in se.elements(args) do
      local v = self:eval(el)
      assert(v)
      table.insert(arg_vals, v)
   end
   return arg_vals
end
function scheme:eval_apply(expr)
   local fun, args = se.unpack(expr, { n = 1, tail = true })
   assert(type(fun) == 'string') -- for now
   local arg_vals = self:eval_args(args)
   local def = self.def[fun]
   assert(def)
   if type(def) == 'function' then
      -- primitive
      return def(unpack(arg_vals))
   else
      -- composite
      return self:apply(def, arg_vals)
   end
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
      return self:eval_apply(expr)
   end
end
local prim = {}
function prim.add(a, b)
   return a + b
end
function scheme.new()
   local obj = { def = {} }
   for k,v in pairs(prim) do
      obj.def[k] = v
   end
   setmetatable(obj, {__index = scheme})
   return obj
end

return scheme
