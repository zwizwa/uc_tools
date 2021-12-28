-- Compiles a subset of Scheme to sm.h style state machines.

-- FIXME: Just doodling at this poing.  Figuring out if this really
-- needs large infrastructure or not (e.g. Haskell or Racket), or if
-- Lua is enough to compile it.


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
--
-- * To support function calls we take an iterative approach:
--   1) a single Scheme file (module) maps to a single C function.
--   2) Scheme functions map to goto labels inside the C function.



local se = require('lib.se')

local smc = {}

local form = {}
smc.form = form

local function is_form(expr, form)
   return type(expr) == 'table' and expr[1] == form
end
local function tail(arr)
   local tab = {}
   assert(#arr > 0)
   for i=2,#arr do table.insert(tab, arr[i]) end
   return tab
end


function smc:indent_string()
   local strs = {}
   for i=1,self.indent do
      table.insert(strs,"  ")
   end
   return table.concat(strs,"")
end

-- FIXME: later, with renaming, it is possible that variables get
-- shuffled so this might need to compute a maximum instead of first
-- match?
local function next_cstack_index(stack)
   for v in se.elements(stack) do
      if v.index then return v.index + 1 end
   end
   return 0 -- This is a C index, starts at 0.
end

-- Introduce a varible.
function smc:new_var(var_name)
   local id = #self.vars + 1
   local v = {var=var_name,id=id,state='unbound'}
   if (not self.vars_last) or (self.vars_last[id].state == 'saved') then
      -- In first pass (no vars_last), assume this variable needs to
      -- be saved on the state stack.  In second pass we have analysis
      -- information to distinguish between emphemeral (local) and
      -- saved.
      v.cstack_index = next_cstack_index(self.stack)
   end
   -- self.var is the list of all created variables
   table.insert(self.vars, v)
   return v
end

function smc:push(var_name)
   local v = self:new_var(var_name)
   -- self.stack is the currently visible environment, which gets popped on exit.
   -- FIXME: How to not put unbound variables here? Maybe not an issue..
   self.stack = se.cons(v, self.stack)
   return v
end

function smc:ref(var)
   -- search backwards.  this implements shadowing
   for v in se.elements(self.stack) do
      if v.state ~= 'unbound' and v.var == var then
         -- If this is a variable that crossed a suspension border,
         -- mark it such that it gets stored in the state struct and
         -- not on the C stack.
         if v.state == 'lost' then
            v.state = 'saved'
         end
         return v
      end
   end
   return nil
end

-- FIXME: This might not be a good primitive form.  I'm leaning more
-- and more towards basic scheme, which would mean actual
-- continuations and function calls.
-- EDIT: Tail recursion is now supported.
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

-- TODO
-- form['if'] = function(self, if_expr, hole)
--    assert(not hole)
--    local _, setup, inner = se.unpack(if_expr, { n = 2, tail = true })

--    self:write(self:indent_string() .. "for(;;){\n")
--    self.indent = self.indent + 1

--    for form in se.elements(inner) do
--       assert(form)
--       self:compile(form, nil)
--    end

--    self.indent = self.indent - 1
--    self:write(self:indent_string() .. "}\n")
-- end


function smc:save(keys, fun)
   local saved = {}
   for i,key in ipairs(keys) do
      saved[key] = self[key]
   end
   local rv = fun()
   for i,key in ipairs(keys) do
      self[key] = saved[key]
   end
   return rv
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

   self:save(
      {'indent','stack'},
      function()
         self.indent = self.indent + 1

         local nb_bindings = se.length(bindings)
         for binding in se.elements(bindings) do
            local var, expr = se.unpack(binding, { n = 2 })
            assert(type(var) == 'string')
            assert(expr)

            -- Reserve a location
            local v = self:push(var)
            self:compile(expr, v, false)
         end

         -- Compile inner forms as statements.  C handles value passing of
         -- the last statement if this is compiled as statement expression.
         local n_inner = se.length(inner)
         assert(n_inner > 0)


         for form, rest_expr in se.elements(inner) do
            assert(form)
            self:compile(form, nil, not se.is_pair(rest_expr))
         end
   end)

   -- Only mark after it's actually bound.
   if hole then
      self:write(self:indent_string() .. "});\n")
      self:mark_bound(hole)
   else
      self:write(self:indent_string() .. "}\n")
   end

end

-- Functions with no arguments compile to goto lables.  The module
-- form compiles to a C function.

form['module'] = function(self, expr, hole)
   assert(hole == nil)
   local _, mod_spec, define_exprs = se.unpack(expr, {n = 2, tail = true})
   local modname = se.unpack(mod_spec, {n = 1})
   self:write("void " .. modname .. "("
                 .. self.config.state_type
                 .. " *" .. self.config.state_name
                 .. ") {\n");

   local function for_defines(f)
      for define_expr in se.elements(define_exprs) do
         local define, fun_spec, body_expr = se.unpack(define_expr, {n = 3})
         assert(define == 'define')
         local fname, args = se.unpack(fun_spec, {n = 1, tail = true})
         assert(body_expr)
         f(fname, args, body_expr)
      end
   end

   -- 'define' has is only defined inside a 'module' form.  we need
   -- to perform two passes to allow for backreferences.
   for_defines(
      function(fname, args, body_expr)
         assert(type(fname == 'string'))
         assert(body_expr)
         -- Keeping track of these saves two purposes: 1) it allows to
         -- distinguish between primitives and composite functions,
         -- and 2) it allows inlining of non-tail calls.
         self.funs[fname] = {args = args, body = body_expr}
      end)
   for_defines(
      function(fname, args, body_expr)
         self:write(fname .. ":\n");
         -- Every function starts and ends with an empty environment.
         -- We do not (yet) support closures.

         -- FIXME: In first pass, keep track of which functions are
         -- called in tail position.  Those will need to be emitted.
         -- Functions that are only inlined do not need to be
         -- generated.
         --assert(#self.stack == 0)
         self:compile(body_expr, nil, true)
         --assert(#self.stack == 0)
      end)

   self:write("}\n");
end


-- Blocking form.  This is implemented in a C macro.
form['read'] = function(self, expr, hole)
   local _, chan = se.unpack(expr, {n = 2})
   local bound = {}
   for var in se.elements(self.stack) do
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


function smc:var_and_type(v)
   local comment = "/*" .. v.var .. "*/"
   if v.cstack_index then
      return "s->e[" .. v.cstack_index .. "]" .. comment, ""
   else
      return "l" .. v.id .. comment, "T "
   end
end

function smc:var(n)
   local c_expr, c_type = self:var_and_type(n)
   return c_expr
end

function smc:mark_bound(v)
   assert(v.state == 'unbound')
   v.state = 'local'
end
function smc:write_assign(n)
   local var, typ = self:var_and_type(n)
   self:write(self:indent_string() .. typ .. var .. " = ")
end
function smc:write_binding(n, c_expr)
   if n then
      self:write_assign(n)
      self:mark_bound(n)
   else
      self:write(self:indent_string())
   end
   self:write(c_expr)
   self:write(";\n");
end

function smc:apply(vals)
   return vals[1] .. "(" .. table.concat(tail(vals),",") .. ")"
end

function smc:atom_to_c_expr(atom, hole)
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

function smc:gensym()
   local n = self.sym_n
   self.sym_n = n + 1
   -- FIXME: Generated symbols should not clash with any program text.
   return "g" .. n
end

-- Convert all arguments to A-Normal form.
-- https://en.wikipedia.org/wiki/A-normal_form
function smc:anf(expr, hole, tail_position)
   local bindings = {}
   local app_form = {}

   assert(se.length(expr) > 0)
   -- FIXME: Functions are primitive for now.
   assert(type(se.car(expr)) == 'string')

   for subexpr in se.elements(expr) do
      if type(subexpr) == 'table' then
         -- non-primitive, insert form
         local sym = self:gensym()
         local binding = se.list(sym, subexpr)
         table.insert(bindings, binding)
         table.insert(app_form, sym)
      else
         -- primitive, just collect it
         table.insert(app_form, subexpr)
      end
   end
   if #bindings == 0 then
      -- Compile function call
      local fun_def = self.funs[app_form[1]]
      if fun_def then
         -- Composite
         if (tail_position) then
            -- FIXME: only 0-arg tail calls for now!
            assert(#app_form == 1)
            -- local cmt = { [true] = "/*tail*/", [false] = "/*no-tail*/" }
            local c_expr = "goto " .. app_form[1] --  .. cmt[tail_position]
            self:write_binding(hole, c_expr)
         else
            -- Non-tail calls are inlined.  We do not support "real"
            -- function calls which would need a stack.
            local nb_bindings = #app_form - 1
            local saved_env = self.stack
            -- Inlining boils down to creating aliases for variables..
            for i=1,nb_bindings do
               self:push(FIXME)
            end
            -- Then compiling the body expression.
            self:compile(fun_def.body, hole, false)
            self.stack = saved_env
         end
      else
         -- Primitive
         local args = {}
         for i=2,#app_form do
            table.insert(args, self:atom_to_c_expr(app_form[i]))
         end
         local c_expr = app_form[1] .. "(" .. table.concat(args, ",") .. ")"
         self:write_binding(hole, c_expr)
      end

   else
      -- Generate let* and ecurse into compiler
      self:compile(
         se.list('let*',
                 se.array_to_list(bindings),
                 -- List expression containing function call
                 se.array_to_list(app_form)),
         hole)
   end
end

function smc:compile(expr, hole, tail_position)
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
      self:anf(expr, hole, tail_position)
   end
end


-- Don't bother with building representations.  The two passes emit C
-- code directly, as the shape of the code resembles the Scheme code
-- fairly directly.  The second pass can use the information gathered
-- in the first pass to allocate variables in the state struct, or on
-- the C stack.
function smc:compile_passes(expr)
   local w = self.write
   -- Override to suppress printing of first pass C output, which is
   -- only neccessary for debugging variable allocation.
   self.write = function() end

   self:reset()
   self:write("\n// first pass\n")
   self:write("#if 0\n")
   self:compile(expr)
   self:write("// state size: " .. self.stack_size .. "\n")
   self:write("#endif\n")


   self.write = w

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
function smc:reset()
   self.vars = {}
   self.stack_size = 0
   self.sym_n = 0
   self.funs = {}
   assert(0 == se.length(self.stack))
   assert(1 == self.indent)
end


function smc.new()
   local config = { state_name = "s", state_type = "state_t" }
   local obj = { stack = {}, indent = 1, config = config }
   setmetatable(obj, {__index = smc})
   return obj
end

return smc
