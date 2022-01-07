-- Minimal Scheme interpreter.  This is intended just for evaluating
-- test code.  Speed is not a concern.

-- Primitives are recognized by type 'function', bound in the initial
-- environment by the user.

local function ifte(c,t,f)
   if c then return t else return f end
end


local se = require('lib.se')
local macro = {}
local scheme = {macro = macro}

macro['begin'] = function(self, expr)
   -- FIXME
end
macro['module'] = function(self, expr)
   local _, _, body = se.unpack(expr, { n = 2, tail = true })
   return {'begin', body}
end
macro['define'] = function(self, expr)
   local _, spec, body = se.unpack(expr, { n = 2, tail = true })
   local name, args = se.unpack(spec, { n = 1, tail = true })
   assert(type(name) == 'string')
   -- FIXME
   -- self.def[name] = self:eval({'lambda', {args, body}})
end
macro['let*'] = function(self, expr)
   local _, bindings, body = se.unpack(expr, { n = 2, tail = true })
   -- FIXME: recursively expand to nested let
end
macro['let'] = function(self, expr)
   local _, bindings, body = se.unpack(expr, { n = 2, tail = true })
   -- FIXME: expand to lambda + app
end


local function ref(var_name, env)
   assert(type(var_name) == 'string')
   for v in se.elements(env) do
      if v.var == var_name then
         return v.val
      end
   end
   error('undefined var ' .. var_name)
end
local function push(var, val, env)
   self.env = se.cons({var = var, val = val}, env)
end



function scheme:eval(expr)
   -- This needs to run state machines, so will need proper tail call
   -- handling.  This means that eval cannot be called recursively in
   -- tail position.  Lua 5.1 doesn't have goto, so use a loop
   -- instead.
   while true do
      if type(expr) ~= 'table' then
         if type(expr) == 'string' then
            return ref(expr, self.env)
         else
            -- Constant
            return expr
         end
      end
      -- Composite.
      local form, tail = unpack(expr)
      assert(form)
      local macro_fn = self.macro[form]
      -- Speed is not a concern, so stick to the absolute minimum:
      -- 'if', 'lambda', 'begin', macro expansion and function application.
      if form == 'if' then
         local _, cond, iftrue, iffalse = se.unpack(expr, { n = 4 })
         expr = ifte(self:eval(cond), iftrue, iffalse)
      elseif form == 'begin' then
         local _, first, rest = se.unpack(expr, {n = 2, tail = true})
         if se.pair(first) then
            self:eval(first)
            expr = {'begin', rest}
         else
            expr = rest
         end
      elseif form == 'lambda' then
         local _, args, body = se.unpack(expr, { n = 3 })
         return se.list({env = elf.env, args = se.list_to_array(args), body = body })
      elseif macro_fn then
         expr = macro_fn(self, expr)
      else
         -- Application
         local fun = self:eval(se.car(expr))
         local val = {}
         for el in se.elements(se.cdr(expr)) do
            table.insert(val, self:eval(expr))
         end
         if type(fun) == 'function' then
            return fun(unpack(val))
         else
            local env = fun.env
            for i = 2,#fun.args do
               local var = fun.args[i]
               if var ~= '_' then
                  push(var, val[i], env)
               end
            end
            self.env = env
            expr = fun.body
         end
      end
   end
end


function scheme.new()
   local obj = { env = se.empty }
   setmetatable(obj, {__index = scheme})
   return obj
end


return scheme
