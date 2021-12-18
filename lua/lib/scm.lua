-- FIXME: Just doodling.  Figuring out if this really needs
-- infrastructure or not (e.g. Haskell).

-- FIXME: Figure out how to encode variable lifetime.  Idea is to
-- transition from 'bound' to 'forgotten' when going through a
-- blocking point, then 'saved' when actually referenced again AND the
-- state was 'forgotten'.

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

function scm:indent_string()
   local strs = {}
   for i=1,self.indent do
      table.insert(strs,"  ")
   end
   return table.concat(strs,"")
end

-- Introduce a varible.
function scm:push(var)
   table.insert(self.env, {var=var,state='unbound'})
   return #self.env
end
function scm:pop()
   local var = self.env[#self.env]
   table.remove(self.env)
end
function scm:ref(var)
   -- search backwards.  this implements shadowing
   for i=#self.env,1,-1 do
      local v = self.env[i]
      if v.state ~= 'unbound' and v.var == var then
         -- If this is a variable that crossed a suspension border,
         -- mark it such that it gets stored in the state struct and
         -- not on the C stack.
         if v.state == 'forgotten' then
            v.state = 'saved'
         end
         return i
      end
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
   self:write(self:indent_string() .. "{\n")
   self.indent = self.indent + 1

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
         self.indent = self.indent - 1
         self:write(self:indent_string() .. "}\n")
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
   self:write(self:indent_string() .. "goto begin;\n}\n");
   -- Return value can never be read.
   return nil
end

-- Blocking form.  This is implemented in a C macro.
form['read'] = function(self, expr, hole)
   assert(2 == se.length(expr))
   local chan = se.car(se.cdr(expr))
   local bound = {}
   for n,var in ipairs(self.env) do
      if var.state == 'bound' then
         table.insert(bound, n)
         var.state = 'forgotten'
      end
   end

   self:write_binding(hole, "READ(" .. chan .. ") /*closure:" .. table.concat(bound,",") .. "*/")
   -- This is a blocking point.  Mark all visible variables.
end

function scm:var(n)
   -- local ref = "s->e[" .. n-1 .. "]"
   local ref = "R(" .. n .. ")"
   local state_comment = ""
   if self.env[n].state == 'saved' then
      state_comment = ":saved"
   end
   local comment = "/*" .. self.env[n].var .. state_comment .. "*/"
   return ref .. comment
end

-- FIXME: Write it symbolically first, then generate C in a second pass.
function scm:write_binding(n, c_expr)
   self:write(self:indent_string())
   if nil ~= n then
      assert(self.env[n].state == 'unbound')
      self.env[n].state = 'bound'
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
      if n then
         return self:var(n)
      else
         return "global." .. atom
      end
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
   local obj = { nb_cont = 0, env = {}, indent = 1 }
   setmetatable(obj, {__index = scm})
   return obj
end

return scm
