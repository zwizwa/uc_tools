-- Minimal Scheme interpreter.  This is intended just for evaluating
-- test code.  Speed is not a concern.

-- Primitives are recognized by type 'function', bound in the initial
-- environment by the user.

local function ifte(c,t,f)
   if c then return t else return f end
end
local function log(str)
   -- io.stderr:write(str)
end


local se = require('lib.se')
local macro = {}
local scheme = {macro = macro}

macro['module'] = function(self, expr)
   local _, _, mod_body = se.unpack(expr, { n = 2, tail = true })
   return {'begin',mod_body}
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

      -- Abstract objects
      if expr.class then
         return expr
      end

      -- Expressions
      local form, tail = unpack(expr)
      assert(form)

      log('form = ' .. form .. '\n')

      local macro_fn = self.macro[form]

      if form == 'if' then
         local _, cond, iftrue, iffalse = se.unpack(expr, { n = 4 })
         expr = ifte(self:eval(cond, env), iftrue, iffalse)

      elseif form == 'begin' then
         local statements = se.cdr(expr)
         if se.is_empty(statements) then
            expr = '#<void>'
         else
            local first, rest = se.unpack(statements, {n = 1, tail = true})

            -- The 'define' form is only valid inside 'begin', where we
            -- need to extend the current environment as we iterate down
            -- the list of statements.
            if type(first) == 'table' and first[1] == 'define' then
               -- Only (define (fun ...) ...) is supported atm.
               local _, spec, fun_body = se.unpack(first, { n = 2, tail = true })
               local name, args = se.unpack(spec, { n = 1, tail = true })
               assert(type(name) == 'string')
               log('define ' .. name .. '\n')
               env = push(name, self:eval({'lambda',{args,fun_body}}, env), env)
               expr = {'begin', rest}
            else
               if se.is_empty(rest) then
                  expr = first
               else
                  self:eval(first, env)
                  expr = {'begin', rest}
               end
            end
         end

      elseif form == 'lambda' then
         local _, args, body = se.unpack(expr, { n = 2, tail = true })
         expr =
            {class = 'closure',
             env   = env,
             args  = se.list_to_array(args),
             body  = {'begin', body }}

      elseif form == 'let*' then
         local _, bindings, body = se.unpack(expr, { n = 2, tail = true })
         for binding in se.elements(bindings) do
            local var, vexpr = se.unpack(binding, {n = 2 })
            env = push(var, self:eval(vexpr, env), env)
         end
         expr = {'begin', body}

      elseif macro_fn then
         expr = macro_fn(self, expr)

      else
         -- Application
         local fun_expr, args_expr = se.unpack(expr, { n = 1, tail = true })
         local fun = self:eval(fun_expr, env)
         local arg_val = {}
         for arg_expr in se.elements(args_expr) do
            table.insert(arg_val, self:eval(arg_expr, env))
         end
         log('app: ' .. type(fun) .. '\n')
         if type(fun) == 'function' then
            expr = fun(unpack(arg_val))
         else
            assert(type(fun) == 'table')
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

function scheme.table_to_env(tab)
   local env = se.empty
   for k,v in pairs(tab) do
      env = push(k,v,env)
   end
   return env
end
local prim = {}
function prim.add(a, b) return a + b end

function scheme:eval_top(expr)
   return self:eval(expr, self.top)
end

function scheme.new()
   local obj = { env = se.empty, top = scheme.table_to_env(prim) }
   setmetatable(obj, {__index = scheme})
   return obj
end


return scheme
