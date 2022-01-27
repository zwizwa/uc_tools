-- Minimal Scheme interpreter.  Used in smc.lua for partial
-- evaluation.  Speed is not a concern.
--
-- Primitives are recognized by type 'function', bound in the initial
-- environment by the user.
--
-- Continuations are not supported: eval uses the Lua call stack to
-- recurse.  However tail calls are properly optimized to support
-- (mutual) tail recursion.

local function ifte(c,t,f)
   if c then return t else return f end
end
local function log(str)
   io.stderr:write(str)
end

require('lib.log')
--local log = log
--local log_desc = log_desc
--local log_hex = log_hex


local se = require('lib.se')
local function ref(var_name, env)
   assert(type(var_name) == 'string')
   for v in se.elements(env) do
      if v.var == var_name then
         return v.val
      end
   end
   log('current bindings:\n')
   for v in se.elements(env) do
      log(' ')
      log(v.var)
   end
   log('\n')
   error('undefined var ' .. var_name)
end
local function ivar_iolist(var)
   return {"<#ivar:",var.var,">"}
end
local function push(var, val, env)
   assert(val)
   return se.cons({var = var, val = val, class = 'ivar', iolist = ivar_iolist}, env)
end

local scheme = {}

-- A special form _must_ reduce the current expression, and is allowed
-- to update the lexical environment in which the next expression is
-- evaluated.  Special forms can call self:eval() in non-tail
-- position, but must leave the result of the evaluation in s.expr
local form = {}
scheme.form = form

form['lambda'] = function(self, s)
   local _, args, body = se.unpack(s.expr, { n = 2, tail = true })
   s.expr = {
      class = 'closure',
      env   = s.env,
      args  = se.list_to_array(args),
      body  = {'begin', body},
      name  = nil,
   }
   -- log_desc({lambda = s.expr})
end

form['if'] = function(self, s)
   local _, cond, iftrue, iffalse = se.unpack(s.expr, { n = 4 })
   s.expr = ifte(self:eval(cond, s.env), iftrue, iffalse)
end

form['set!'] = function(self, s)
   local _, name, exp = se.unpack(s.expr, { n = 2 })
   local var = ref(name, s.env)
   var.val = self:eval(exp, s.env)
   s.expr = '#<void>'
end


form['let*'] = function(self, s)
   local _, bindings, body = se.unpack(s.expr, { n = 2, tail = true })
   for binding in se.elements(bindings) do
      local var, vexpr = se.unpack(binding, {n = 2 })
      s.env = push(var, self:eval(vexpr, s.env), s.env)
   end
   local val = '#<void>'
   for expr in se.elements(body) do
      val = self:eval(expr, s.env)
   end
   s.expr = val
end


-- Macros are forms that do not modify s.env
-- These are kept in a separate file as they can probably be reused.
local macros = require('lib.scheme_macros')
function scheme.macro(fun)
   return function(self, s) s.expr = fun(s.expr) end
end
for k,v in pairs(macros) do
   form[k] = scheme.macro(v)
end


function scheme:eval(expr, env)
   return self:eval_loop({
         expr = expr,
         env = env or self.mod_env
   })
end

function scheme:eval_app(s)
   -- Application
   local fun_expr, args_expr = se.unpack(s.expr, { n = 1, tail = true })
   local fun = self:eval(fun_expr, s.env)
   local arg_val = {}
   for arg_expr in se.elements(args_expr) do
      assert(arg_expr)
      table.insert(arg_val, self:eval(arg_expr, s.env))
   end
   -- log('app: ' .. type(fun) .. '\n')
   if type(fun) == 'function' then
      -- Primitive
      s.expr = fun(unpack(arg_val))
      if s.expr == nil then s.expr = '#<void>' end
   else
      -- Closure
      assert(type(fun) == 'table')
      local new_env = fun.env
      assert(#fun.args == se.length(args_expr))
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


function scheme:is_value(s)
   -- Atoms
   if type(s.expr) ~= 'table' then
      if type(s.expr) == 'string' then
         if 35 == string.byte(s.expr,1) then
            -- Strings starting with '#' are primitives.
            -- e.g. '#<void>'
            return true
         else
            -- Variable reference
            return false
         end
      else
         -- Constant
         return true
      end
   end

   -- Abstract objects
   if s.expr.class then
      return true
   end

   -- Expression
   return false
end




-- Proper tail call handling means that eval cannot be called
-- recursively in tail position.  Lua 5.1 doesn't have goto, so use a
-- loop that updates the current interpreter state = current
-- expression + current environment.
function scheme:eval_loop(s)
   while not self:is_value(s) do
      self:eval_step(s)
   end
   assert(s.expr)
   return s.expr
end

function scheme:eval_step(s)
   assert(s.env)
   assert(s.expr)

   -- If we get past is_value(), all strings are variable
   -- references.
   if type(s.expr) == 'string' then
      -- Look up variable in environment.
      s.expr = ref(s.expr, s.env)
   else

      -- Expressions
      local form, tail = unpack(s.expr)
      assert(form)

      -- log('form = ' .. form .. '\n')
      local form_fn = self.form[form]
      if form_fn then
         -- Special form or macro.  These are in a separate table to
         -- make interpreter extension straightforward.
         form_fn(self, s)
      else
         -- Primitive or form application.
         self:eval_app(s)
      end
   end
end


function scheme:define(tab)
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
   obj:define(prim)
   for _,tab in ipairs(tables or {}) do
      obj:define(tab)
   end
   return obj
end


return scheme
