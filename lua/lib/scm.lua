-- FIXME: Just doodling.  Figuring out if this really needs
-- infrastructure or not (e.g. Haskell).

-- FIXME: Figure out how to determine variable lifetime.

-- Compiler for small subset of Scheme to compile down to sm.h style
-- state machines.


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


local se = require('lib.se')

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
-- steps: single binding form let1 and let* that expands into nested
-- let1 forms.
form['let1'] = function(self, let_expr, hole)
   local bindings, inner = unpack(se.cdr(let_expr))
   assert(type(bindings) == 'table')
   -- Primitive form only supports one binding.
   assert(1 == se.length(bindings))
   local binding = se.car(bindings)
   assert(2 == se.length(binding))
   local var  = se.car(binding)
   local expr = se.car(se.cdr(binding))
   assert(type(var) == 'string')
   assert(expr)
   -- Reserve a location
   local n = self:push(var)
   self:compile(expr, n)
   -- Compile inner forms.  Only the result of the last one is stored.
   assert(se.length(inner) > 0)
   while true do
      local form = se.car(inner)
      assert(form)
      if 1 == se.length(inner) then
         self:compile(form, hole)
         self:pop()
         return
      else
         self:compile(form, nil)
      end
      inner = se.cdr(inner)
   end
end

-- Macros are implemented by calling self:compile() directly.
form['let*'] = function(self, let_expr, hole)
   local bindings, statements = unpack(se.cdr(let_expr))
   local n = se.length(bindings)
   assert(n>0)
   if 1 == n then
      return self:compile({'let1', {bindings, statements}}, hole)
   else
      local first = se.car(bindings)
      local rest  = se.cdr(bindings)
      return self:compile({'let1', {se.list(first), se.list({'let*', {rest, statements}})}}, hole)
   end
end

-- Every machine is a loop.
form['loop'] = function(self, expr, hole)
   self:write("void loop(state_t *s) {\nbegin:\n");
   local tail = se.cdr(expr)
   -- assert(nil == se.cdr(tail))
   self:compile(se.car(tail), hole)
   self:write(indent .. "goto begin;\n}\n");
   -- Return value can never be read.
   return nil
end

-- Blocking form.  This is implemented in a C macro.
form['read'] = function(self, expr, hole)
   assert(2 == se.length(expr))
   local chan = se.car(se.cdr(expr))
   self:write_binding(hole, "READ(" .. chan .. ") /*blocks*/")
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

function scm:atom_to_c_expr(atom, hole)
   if type(atom) == 'string' then
      local n = self:ref(atom)
      local thing = n or "global." .. atom
      return thing
   else
      -- number or other const
      return atom
   end
end

function scm:compile(expr, hole)
   if type(expr) ~= 'table' then
      return self:write_binding(hole, self:atom_to_c_expr(expr))
   end
   local form, tail = unpack(expr)
   assert(form)
   -- self:write('/*form: ' .. form .. "*/")
   assert(type(form) == 'string')
   local form_fn = self.form[form]
   if form_fn then
      return form_fn(self, expr, hole)
   else
      -- We expect everything to be in ANF, meaning arguments to
      -- function calls are only constants or variable references.
      -- FIXME: Insert let* form if this is not the case.
      local refs = {}
      for maybe_var in se.elements(tail) do
         table.insert(refs, self:atom_to_c_expr(maybe_var))
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
