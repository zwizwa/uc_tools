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

local scheme = {}

-- A special form _must_ reduce the current expression, and is allowd
-- to update the lexical environment.  Special forms can call
-- self:eval() in non-tail position.
local form = {}
scheme.form = form

form['lambda'] = function(self, s)
   local _, args, body = se.unpack(s.expr, { n = 2, tail = true })
   s.expr = {
      class = 'closure',
      env   = s.env,
      args  = se.list_to_array(args),
      body  = {'begin', body}
   }
end

form['if'] = function(self, s)
   local _, cond, iftrue, iffalse = se.unpack(s.expr, { n = 4 })
   s.expr = ifte(self:eval(cond, s.env), iftrue, iffalse)
end

form['begin'] = function(self, s)
   local statements = se.cdr(s.expr)
   if se.is_empty(statements) then
      s.expr = '#<void>'
   else
      local first, rest = se.unpack(statements, {n = 1, tail = true})

      -- The 'define' form is only valid inside 'begin', where we need
      -- to extend the current environment as we iterate down the list
      -- of statements.
      if type(first) == 'table' and first[1] == 'define' then
         -- Only (define (fun ...) ...) is supported atm.
         local _, spec, fun_body = se.unpack(first, { n = 2, tail = true })
         local name, args = se.unpack(spec, { n = 1, tail = true })
         assert(type(name) == 'string')
         log('define ' .. name .. '\n')
         s.env = push(name, self:eval({'lambda',{args,fun_body}}, s.env), s.env)
         s.expr = {'begin', rest}
      else
         if se.is_empty(rest) then
            s.expr = first
         else
            self:eval(first, s.env)
            s.expr = {'begin', rest}
         end
      end
   end
end

form['let*'] = function(self, s)
   local _, bindings, body = se.unpack(s.expr, { n = 2, tail = true })
   for binding in se.elements(bindings) do
      local var, vexpr = se.unpack(binding, {n = 2 })
      s.env = push(var, self:eval(vexpr, s.env), s.env)
   end
   s.expr = {'begin', body}
end


-- Macros are forms that do not modify s.env
function scheme.macro(fun)
   return function(self, s)
      s.expr = fun(s.expr)
   end
end
form['module'] = scheme.macro(function(expr)
   local _, _, mod_body = se.unpack(expr, { n = 2, tail = true })
   return {'begin',mod_body}
end)



function scheme:eval(expr, env)
   return self:eval_loop({
         expr = expr,
         env = env or self.mod_env
   })
end

function scheme:eval_loop(s)

   -- This needs to run state machines, so will need proper tail call
   -- handling.  This means that eval cannot be called recursively in
   -- tail position.  Lua 5.1 doesn't have goto, so use a loop
   -- instead.
   while true do

      assert(s.env)
      assert(s.expr)

      -- Atoms
      if type(s.expr) ~= 'table' then
         if type(s.expr) == 'string' then
            -- Strings starting with '#' are primitives.
            -- e.g. '#<void>'
            if 35 == string.byte(s.expr,1) then
               return s.expr
            else
               return ref(s.expr, s.env)
            end
         else
            -- Constant
            return s.expr
         end
      end

      -- Abstract objects
      if s.expr.class then
         return s.expr
      end

      -- Expressions
      local form, tail = unpack(s.expr)
      assert(form)

      log('form = ' .. form .. '\n')

      local form_fn = self.form[form]

      if form_fn then
         -- Special form or macro.
         form_fn(self, s)

      else
         -- Application
         local fun_expr, args_expr = se.unpack(s.expr, { n = 1, tail = true })
         local fun = self:eval(fun_expr, s.env)
         local arg_val = {}
         for arg_expr in se.elements(args_expr) do
            table.insert(arg_val, self:eval(arg_expr, s.env))
         end
         log('app: ' .. type(fun) .. '\n')
         if type(fun) == 'function' then
            s.expr = fun(unpack(arg_val))
            if s.expr == nil then s.expr = '#<void>' end
         else
            assert(type(fun) == 'table')
            local new_env = fun.env
            for i = 1,#fun.args do
               local var = fun.args[i]
               if var ~= '_' then
                  new_env = push(var, arg_val[i], new_env)
               end
            end
            s.env = new_env
            s.expr = fun.body
         end
      end
   end
end

function scheme:push_mod(tab)
   for k,v in pairs(tab) do
      self.mod_env = push(k,v,self.mod_env)
   end
end

local prim = {}
scheme.prim = prim
function prim.add(a, b) return a + b end

function scheme:eval_file(filename)
   local stream = io.open(filename,"r")
   local parser = se.new(stream)
   local exprs = parser:read_multi()
   stream:close()
   return self:eval({'begin',exprs})
end

function scheme.new(tables)
   local obj = {
      -- lexical environment
      env = se.empty,
      -- toplevel / module environment
      mod_env = se.empty,
   }
   setmetatable(obj, {__index = scheme})
   -- install module level bindings
   obj:push_mod(prim)
   for _,tab in ipairs(tables or {}) do
      obj:push_mod(tab)
   end
   return obj
end


return scheme
