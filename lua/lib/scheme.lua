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

macro['module'] = function(self, expr)
   local _, _, body = se.unpack(expr, { n = 2, tail = true })
   return {'begin', body}
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
   return se.cons({var = var, val = val}, env)
end



function scheme:eval(expr, env)

   -- This needs to run state machines, so will need proper tail call
   -- handling.  This means that eval cannot be called recursively in
   -- tail position.  Lua 5.1 doesn't have goto, so use a loop
   -- instead.
   while true do

      assert(env)
      assert(expr)

      -- Atoms
      if type(expr) ~= 'table' then
         if type(expr) == 'string' then
            -- Strings starting with '#' are primitives.
            -- e.g. '#<void>'
            if 35 == string.byte(expr,1) then
               return expr
            else
               return ref(expr, env)
            end
         else
            -- Constant
            return expr
         end
      end

      -- Expressions
      local form, tail = unpack(expr)
      assert(form)
      local macro_fn = self.macro[form]
      -- Speed is not a concern, so stick to the absolute minimum:
      -- 'if', 'lambda', macro expansion and function application, and
      -- primitives that would be more work to express as a macro
      -- ('begin', 'let*').
      local function lambda(args, body)
         return se.list({env = env, args = se.list_to_array(args), body = body })
      end

      if form == 'if' then
         local _, cond, iftrue, iffalse = se.unpack(expr, { n = 4 })
         expr = ifte(self:eval(cond, env), iftrue, iffalse)

      elseif form == 'begin' then
         local _, first, rest = se.unpack(expr, {n = 2, tail = true})
         if se.is_pair(rest) then
            self:eval(first, env)
            expr = {'begin', rest}
         else
            expr = rest
         end

      elseif form == 'lambda' then
         local _, args, body = se.unpack(expr, { n = 3 })
         return lambda(args, body)

      elseif form == 'let*' then
         local _, bindings, body = se.unpack(expr, { n = 2, tail = true })
         for binding in se.elements(bindings) do
            local var, vexpr = se.unpack(binding, {n = 2 }),
            env = push(var, self:eval(vexpr, env), env)
         end
         expr = {'begin', body}

      elseif form == 'define' then
         local _, spec, body = se.unpack(expr, { n = 2, tail = true })
         local name, args = se.unpack(spec, { n = 1, tail = true })
         assert(type(name) == 'string')
         env = push(name, lambda(args, body), env)
         return '#<void>'

      elseif macro_fn then
         expr = macro_fn(self, expr)

      else
         -- Application
         local fun = self:eval(se.car(expr), env)
         local arg_val = {}
         for el in se.elements(se.cdr(expr)) do
            table.insert(arg_val, self:eval(expr, env))
         end
         if type(fun) == 'function' then
            return fun(unpack(arg_val))
         else
            local new_env = fun.env
            for i = 1,#fun.args do
               local var = fun.args[i]
               if var ~= '_' then
                  new_env = push(var, arg_val[i], new_env)
               end
            end
            env = new_env
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
