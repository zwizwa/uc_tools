-- FIXME: Just doodling.  Figuring out if this really needs
-- infrastructure or not (e.g. Haskell).  There are a lot of issues
-- with this.

-- Reduced scheme compiler to map onto sm.h style state machines.


-- Some ideas:

-- 1. Compiling closures to C functions is too much work.  Use
--    computed goto.  It's quite useful to jump into a control
--    structure.
--
-- 2. All higher order functions need to be implemented as macros.
--    Maybe some functionality can be defined for that?
--
-- 3. All non blocking primitives can just be C functions.
--
-- 4. Keep the environment simple: an array of words essentially
--    implementing a stack.  The compiler can guarantee the stack size
--    at compile time.
--

-- FIXME: Restructure such that eval (compile) is always parameterized
-- by a location where the value is stored, which is either a stack
-- location or a local variable in case the variable doesn't escape.
--
-- Really this is no different than the other compilers, just that
-- there are two kinds of variables: those that are not referenced
-- past a suspension point, and those that are.  It's not clear how to
-- separate them.

local scm = {}

local form = {}
scm.form = form

local function is_form(expr, form)
   return type(expr) == 'table' and expr[1] == form
end
local function tail(arr)
   local tab = {}
   assert(#arr > 0)
   for i=2,#arr do table.insert(tab, arr[i]) end
   return tab
end

function scm:alloc_cont()
   local n = self.nb_cont
   self.nb_cont = n + 1
   return n
end

local indent = "  "

-- Introduce a varible on the stack.  Note that it's probably possible
-- to mark the variables that do not escape.
function scm:push(var)
   table.insert(self.env, var)
   return #self.env
end
function scm:pop()
   local var = self.env[#self.env]
   table.remove(self.env)
end
function scm:ref(var)
   -- search backwards.  this implements shadowing
   for i=#self.env,1,-1 do
      if self.env[i] == var then return i end
   end
   return nil
end

-- Basic structuring form is 'let*' for a single variable binding,
-- which mostly resembles C's scoping rules.  Implemented in two
-- steps: single binding form let1 and nested let*.
form['let1'] = function(self, let_expr, hole)
   assert(#let_expr == 3)
   local _, bindings, inner = unpack(let_expr)
   assert(type(bindings) == 'table')
   -- Primitive form only supports one binding.
   assert(#bindings == 1)
   local var, expr = unpack(bindings[1])
   assert(type(var) == 'string')
   assert(expr)
   -- Reserve a location
   local n = self:push(var)
   self:compile(expr, n)
   self:compile(inner, hole)
   self:pop()
   -- This isn't quite right: it excapes out of the context, but then
   -- again the value is never used.
end

-- Macros are implemented by calling self:compile() directly.
form['let*'] = function(self, let_expr, hole)
   local _, bindings, inner = unpack(let_expr)
   assert(#bindings > 0)
   if #bindings == 1 then
      return self:compile({'let1', bindings, inner}, hole)
   else
      return self:compile({'let1', {bindings[1]}, {'let*', tail(bindings), inner}}, hole)
   end
end

-- Every machine is a loop.
form['loop'] = function(self, expr, hole)
   self:write("void loop(state_t *s) {\nbegin:\n");
   assert(#expr == 2)
   self:compile(expr[2], hole)
   self:write(indent .. "goto begin;\n}\n");
   -- Return value can never be read.
   return nil
end

-- Blocking form.  This is implemented in a C macro.
form['read'] = function(self, expr, hole)
   assert(#expr == 2)
   local chan = expr[2]
   self:write_binding(hole, "READ(" .. chan .. ")")
end

function scm:var(n)
   local ref = "s->e[" .. n-1 .. "]"
   local comment = "/*" .. self.env[n] .. "*/"
   return ref .. comment
end

function scm:write_binding(n, c_expr)
   self:write(indent)
   if nil ~= n then
      self:write(self:var(n) .. " = ")
   end
   self:write(c_expr)
   self:write(";\n");
end
function scm:write_statement(c_expr)
   self:write_binding(nil, c_expr)
end

function scm:apply(vals)
   return vals[1] .. "(" .. table.concat(tail(vals),",") .. ")"
end

function scm:compile(expr, hole)
   assert(expr)
   if type(expr) == 'number' then
      return self:write_binding(hole, expr)
   end
   if type(expr) == 'string' then
      local n = self:ref(expr)
      if n then
         return self:write_binding(hole, expr)
      else
         return self:write_binding(hole, expr)
      end
   end
   assert(type(expr) == 'table')
   local form = unpack(expr)
   assert(type(form) == 'string')
   local form_fn = self.form[form]
   if form_fn then
      return form_fn(self, expr, hole)
   else
      -- We expect everything to be in ANF
      local refs = {}
      for i=2,#expr do
         local n = self:ref(expr[i])
         if n then
            table.insert(refs, self:var(n))
         else
            table.insert(refs, "global:" .. expr[i])
         end
      end
      local c_expr = expr[1] .. "(" .. table.concat(refs, ",") .. ")"
      self:write_binding(hole, c_expr)
   end
end

function scm.new()
   local obj = { nb_cont = 0, env = {} }
   setmetatable(obj, {__index = scm})
   return obj
end

return scm
