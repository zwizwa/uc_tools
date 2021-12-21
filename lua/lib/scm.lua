-- FIXME: Just doodling.  Figuring out if this really needs large
-- infrastructure or not (e.g. Haskell or Racket), or if Lua is enough
-- to compile it.


-- Compiler for small subset of Scheme to compile down to sm.h style
-- state machines.

-- Some ideas:

-- * Compiling continuations to individual C functions is too much
--   work.  Use computed goto just like sm.h does. Much easier to jump
--   straight into a control structure compared to separately
--   representing continuations.
--
-- * The main problem then becomes variable management, essentially
--   implementing variable storage: local for temporary variables that
--   do not need to survive yield points, and C struct for values that
--   survive yield points.
--
-- * All non blocking primitives can just be C functions.
--
-- * Keep the stored environment simple: an array of machine words
--   essentially implementing a stack.  The compiler can guarantee the
--   stack size at compile time.
--
-- * Basic program form is scheme's let* mapped to GCC statement
--   expressions.
--
-- * Second pass can distinguish between saved and local variables,
--   and perform the stack allocation.
--
-- * Lifetime is implemented by letting variables go through this cycle:
--   unbound -> local -> lost -> saved
--   with the last 2 transitions not happening for all.
--
-- * Blocking subroutines are not yet implemented.  Simplest is to
--   implement them as macros, but a function call mechanism inside a
--   machine isn't an impossible challenge.



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


function scm:indent_string()
   local strs = {}
   for i=1,self.indent do
      table.insert(strs,"  ")
   end
   return table.concat(strs,"")
end

-- Introduce a varible.
function scm:push(var)
   local id = #self.vars + 1
   local v = {var=var,id=id,state='unbound'}
   -- self.var is the list of all created variables
   table.insert(self.vars, v)
   -- self.env is the currently visible environment, which gets popped on exit.
   -- FIXME: How to not put unbound variables here? Maybe not an issue..
   table.insert(self.env, v)
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
         if v.state == 'lost' then
            v.state = 'saved'
         end
         return i
      end
   end
   return nil
end

-- FIXME: This might not be a good primitive form.  I'm leaning more
-- and more towards basic scheme, which would mean actual
-- continuations and function calls.
form['for'] = function(self, for_expr, hole)
   -- For doesn't return a value, so for now just assert there is
   -- nothing to bind.
   assert(not hole)
   local _, setup, inner = se.unpack(for_expr, { n = 2, tail = true })

   self:write(self:indent_string() .. "for(;;){\n")
   self.indent = self.indent + 1

   for form in se.elements(inner) do
      assert(form)
      self:compile(form, nil)
   end

   self.indent = self.indent - 1
   self:write(self:indent_string() .. "}\n")
end

form['if'] = function(self, if_expr, hole)
   assert(not hole)
   local _, setup, inner = se.unpack(if_expr, { n = 2, tail = true })

   self:write(self:indent_string() .. "for(;;){\n")
   self.indent = self.indent + 1

   for form in se.elements(inner) do
      assert(form)
      self:compile(form, nil)
   end

   self.indent = self.indent - 1
   self:write(self:indent_string() .. "}\n")
end


-- Core form is 'let*' which mostly resembles C's scoping rules.
form['let*'] = function(self, let_expr, hole)
   local _, bindings, inner = se.unpack(let_expr, { n = 2, tail = true })

   assert(type(bindings) == 'table')

   if hole then
      -- C statement expressions essentially let*
      self:write_assign(hole)
      self:write("({\n")
   else
      -- If there's no variable to bind then use a block.
      self:write(self:indent_string() .. "{\n")
   end
   self.indent = self.indent + 1

   local nb_bindings = se.length(bindings)

   for binding in se.elements(bindings) do
      local var, expr = se.unpack(binding, { n = 2 })
      assert(type(var) == 'string')
      assert(expr)

      -- Reserve a location
      local n = self:push(var)
      self:compile(expr, n)
   end

   -- Compile inner forms as statements.  C handles value passing of
   -- the last statement if this is compiled as statement expression.
   assert(se.length(inner) > 0)
   for form in se.elements(inner) do
      assert(form)
      self:compile(form, nil)
   end

   self.indent = self.indent - 1
   for i=1,nb_bindings do
      self:pop()
   end

   -- Only mark after it's actually bound.
   if hole then
      self:write(self:indent_string() .. "});\n")
      self:mark_bound(hole)
   else
      self:write(self:indent_string() .. "}\n")
   end

end

-- Functions with no arguments.
-- FIXME: How to specify this as only top-level form?
form['define'] = function(self, expr, hole)
   local _, spec, expr1 = se.unpack(expr, {n = 3})
   local fname = se.unpack(spec, {n = 1})
   self:write("void " .. fname .. "("
                 .. self.config.state_type
                 .. " *" .. self.config.state_name
                 .. ") {\n");
   assert(expr1)
   self:compile(expr1, hole)
   self:write(self:indent_string() .. "}\n");
end

-- Blocking form.  This is implemented in a C macro.
form['read'] = function(self, expr, hole)
   local _, chan = se.unpack(expr, {n = 2})
   local bound = {}
   for n,var in ipairs(self.env) do
      -- At the suspension point, all local variables are lost.
      -- This is used later when the variable is referenced to turn it
      -- into a 'saved' variable, so in the second pass it can be
      -- properly allocated.
      if var.state == 'local' then
         table.insert(bound, n)
         var.state = 'lost'
      end
   end
   self:write_binding(
      hole,
      "SM_READ("
         .. self.config.state_name .. ","
         .. self.config.state_name .. "->" .. chan .. ")")
end

function scm:stack_index(n)
   -- Only works in second pass.  In first pass we map the Scheme
   -- environment index directly to a stack index.
   local n1 = n
   if self.vars_last then
      n1 = 0
      for i=1,n do
         local id = self.env[i].id
         -- In the second pass we have the final word on all of the
         -- variables.
         if self.vars_last[id].state == 'saved' then
            n1 = n1 + 1
         end
      end
   end
   if n1 > self.stack_size then
      self.stack_size = n1
   end
   return n1
end

function scm:var_and_type(n)
   local v = self.env[n]
   local c_index = self:stack_index(n) - 1

   if not self.vars_last then
      -- First pass: allocate all bindings in the state's stack.
      local state_comment = ""
      if v.state == 'saved' then state_comment = ":saved" end
      local comment = "/*" .. v.var .. state_comment .. "*/"
      -- return "r" .. v.id .. comment, ""
      return "s->e[" .. c_index .. "]" .. comment, ""
   else
      -- Second pass: we have a lot more information now.  Saved and
      -- local variables can be distinguished and the stack allocation
      -- can be shrunk.
      local last = self.vars_last[v.id]
      assert(last)
      assert(last.state)
      local comment = "/*" .. v.var .. "*/"
      if last.state == 'saved' then
         return "s->e[" .. c_index .. "]" .. comment, ""
      else
         return "l" .. v.id .. comment, "T "
      end
   end
end

function scm:var(n)
   local c_expr, c_type = self:var_and_type(n)
   return c_expr
end

function scm:mark_bound(n)
   assert(self.env[n].state == 'unbound')
   self.env[n].state = 'local'
end
function scm:write_assign(n)
   local var, typ = self:var_and_type(n)
   self:write(self:indent_string() .. typ .. var .. " = ")
end
function scm:write_binding(n, c_expr)
   if n then
      self:write_assign(n)
      self:mark_bound(n)
   else
      self:write(self:indent_string())
   end
   self:write(c_expr)
   self:write(";\n");
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

function scm:gensym()
   local n = self.sym_n
   self.sym_n = n + 1
   -- FIXME: Generated symbols should not clash with any program text.
   return "g" .. n
end

-- Convert all arguments to A-Normal form.
-- https://en.wikipedia.org/wiki/A-normal_form
function scm:anf(expr, hole)
   local bindings = {}
   local prim_forms = {}

   assert(se.length(expr) > 0)
   -- FIXME: Functions are primitive for now.
   assert(type(se.car(expr)) == 'string')

   for subexpr in se.elements(expr) do
      if type(subexpr) == 'table' then
         -- non-primitive, insert form
         local sym = self:gensym()
         local binding = se.list(sym, subexpr)
         table.insert(bindings, binding)
         table.insert(prim_forms, sym)
      else
         -- primitive, just collect it
         table.insert(prim_forms, subexpr)
      end
   end
   if #bindings == 0 then
      -- Compile primitive call
      local args = {}
      for i=2,#prim_forms do
         table.insert(args, self:atom_to_c_expr(prim_forms[i]))
      end
      local c_expr = prim_forms[1] .. "(" .. table.concat(args, ",") .. ")"
      self:write_binding(hole, c_expr)

   else
      -- Generate let* and ecurse into compiler
      self:compile(
         se.list('let*',
                 se.array_to_list(bindings),
                 -- List expression containing function call
                 se.array_to_list(prim_forms)),
         hole)
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
      self:anf(expr, hole)
   end
end


-- Don't bother with building representations.  The two passes emit C
-- code directly, as the shape of the code resembles the Scheme code
-- fairly directly.  The second pass can use the information gathered
-- in the first pass to allocate variables in the state struct, or on
-- the C stack.
function scm:compile_passes(expr)

   self:reset()
   self:write("\n// first pass\n")
   self:write("#if 0\n")
   self:compile(expr)
   self:write("// state size: " .. self.stack_size .. "\n")
   self:write("#endif\n")

   -- Second pass uses vars_last: the information gathered about each
   -- variable in the first pass.
   self.vars_last = self.vars

   self:reset()
   self:write("\n// second pass\n")
   self:compile(expr)
   self:write("// state size: " .. self.stack_size .. "\n")
   self:write("\n")
end

-- Reset compiler state before executing a new pass.
function scm:reset()
   self.vars = {}
   self.stack_size = 0
   self.sym_n = 0
   assert(0 == #self.env)
   assert(1 == self.indent)
end


function scm.new()
   local config = { state_name = "s", state_type = "state_t" }
   local obj = { env = {}, indent = 1, config = config }
   setmetatable(obj, {__index = scm})
   return obj
end

return scm
