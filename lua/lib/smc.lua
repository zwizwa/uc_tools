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

local prompt = require('prompt')
local function log(str)
   io.stderr:write(str)
end
local function log_desc(thing)
   log(prompt.describe(thing))
   log("\n")
end

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

-- Return C index for C struct storage (first index is 0).
-- This depends on the current stack.
-- Also we have no guarantee that the variables are ordered, so scan the whole stack.
-- FIXME: That introduces quadratic behavior
function smc:next_c_index()
   local c_index = 0
   for v in se.elements(self.stack) do
      if v.cell.c_index and v.cell.c_index >= c_index then
         c_index = v.cell.c_index + 1
      end
   end
   if c_index >= self.c_size then
      self.c_size = c_index + 1
   end
   return c_index
end

-- Introduce a varible.
function smc:new_cell()
   local id = #self.cells + 1
   local cell = {id=id,bind='unbound'}
   if (not self.cells_last) or (self.cells_last[id].bind == 'saved') then
      -- In first pass (no cells_last), assume this variable needs to
      -- be saved on the state stack.  In second pass we have analysis
      -- information to distinguish between emphemeral (local) and
      -- saved.
      cell.c_index = self:next_c_index()
   end
   -- self.var is the list of all created variables
   table.insert(self.cells, cell)
   return cell
end

function smc:push_var(var_name, cell)
   if not cell then
      cell = self:new_cell()
   end
   local v = {var=var_name,cell=cell}
   -- self.stack is the currently visible environment, which gets popped on exit.
   -- FIXME: How to not put unbound variables here? Maybe not an issue..
   self.stack = se.cons(v, self.stack)
   return v
end

-- Aliases are used to implement substitution for function inlining.
function smc:push_alias(alias_name, v)
   assert(v and v.cell)
   local v_alias = {var=alias_name, cell=v.cell}
   self.stack = se.cons(v_alias, self.stack)
   return v_alias
end

function smc:ref(var, stack)
   if not stack then stack = self.stack end

   -- search starts at last pushed variable.  this implements shadowing
   for v in se.elements(stack) do
      if v.var == var then
         -- note that if the cell is not yet bound to a value, we
         -- can't see it yet.  this is a hack: FIXME
         if v.cell.bind ~= 'unbound' then
            -- If this is a variable that crossed a suspension border,
            -- mark it such that it gets stored in the state struct and
            -- not on the C stack.
            if v.cell.bind == 'lost' then
               v.cell.bind = 'saved'
            end
            return v
         end
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

-- Save/restore dynamic environment.
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
form['let*'] = function(self, let_expr, hole, tail_position)
   local _, bindings, inner = se.unpack(let_expr, { n = 2, tail = true })

   assert(type(bindings) == 'table')

   if hole then
      -- C statement expressions are essentially let*
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
            local v = self:push_var(var)
            self:compile(expr, v, false)
         end

         -- Compile inner forms as statements.  C handles value passing of
         -- the last statement if this is compiled as statement expression.
         local n_inner = se.length(inner)
         assert(n_inner > 0)


         for form, rest_expr in se.elements(inner) do
            assert(form)
            local sub_tail_position =
               tail_position and
               (not se.is_pair(rest_expr))
            self:compile(form, nil, sub_tail_position)
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

         assert(0 == se.length(self.stack))
         self:compile(body_expr, nil, true)
         assert(0 == se.length(self.stack))
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
      if var.cell.bind == 'local' then
         table.insert(bound, n)
         var.cell.bind = 'lost'
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
   if v.cell.c_index then
      return "s->e[" .. v.cell.c_index .. "]" .. comment, ""
   else
      return "l" .. v.cell.id .. comment, "T "
   end
end

function smc:var(n)
   local c_expr, c_type = self:var_and_type(n)
   return c_expr
end

function smc:mark_bound(v)
   assert(v.cell.bind == 'unbound')
   v.cell.bind = 'local'
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

-- Apply function to arguments, converting all arguments to A-Normal
-- form if necessary.
--
-- https://en.wikipedia.org/wiki/A-normal_form
--
-- To simplify representation, we also bind constants to variables.
-- The C compiler can later optimize those.
--
function smc:apply(expr, hole, tail_position)
   local bindings = {}
   local app_form = {}

   assert(se.length(expr) > 0)
   assert(type(se.car(expr)) == 'string')

   for subexpr in se.elements(expr) do
      if type(subexpr) == 'string' then
         -- variable reference, just collect
         table.insert(app_form, subexpr)
      else
         -- sub-expression or constant. insert form
         local sym = self:gensym()
         local binding = se.list(sym, subexpr)
         table.insert(bindings, binding)
         table.insert(app_form, sym)
      end
   end

   -- If any new bindings were generated, insert a let* for and
   -- recurse into compiler.
   if #bindings > 0 then
      self:compile(
         se.list('let*',
                 se.array_to_list(bindings),
                 -- List expression containing function call
                 se.array_to_list(app_form)),
         hole,
         tail_position)
      return
   end


   -- The expression is in A-Normal form.
   local fun_name = app_form[1]
   local fun_def = self.funs[fun_name]

   -- If there is no function definition under this name, we assume
   -- this is a C primitive.  Emit code.
   if not fun_def then
      local args = {}
      for i=2,#app_form do
         table.insert(args, self:atom_to_c_expr(app_form[i]))
      end
      local c_expr = fun_name .. "(" .. table.concat(args, ",") .. ")"
      self:write_binding(hole, c_expr)
      return
   end

   -- Compile composite function call.
   if (tail_position) then
      -- FIXME: only 0-arg tail calls for now!
      -- log("tail: fun_name=" .. fun_name .. "\n")
      assert(#app_form == 1)
      -- local cmt = { [true] = "/*tail*/", [false] = "/*no-tail*/" }
      local c_expr = "goto " .. app_form[1] --  .. cmt[tail_position]
      self:write_binding(hole, c_expr)
   else
      log("no_tail: fun_name=" .. fun_name .. "\n")
      -- Non-tail calls are inlined as we do not support the call
      -- stack necessary for "real" calls.
      self:save(
         {'stack','depth'},
         function()
            self.depth = self.depth + 1
            assert(self.depth < 10)

            -- Inlining boils down to creating a new environment where
            -- variables from the call site are aliased into a fresh
            -- scope.
            local callsite_stack = self.stack
            self.stack = {}
            for i=2, #app_form do
               local var_name = app_form[i]
               assert(type(var_name) == 'string')
               local callsite_var = self:ref(var_name, callsite_stack)
               assert(callsite_var)
               self:push_alias(var_name, callsite_var)
            end
            -- Then compiling the body expression.
            log_desc(fun_def.body)
            self:compile(fun_def.body, hole, false)
         end)
   end

end

local function ifte(c,t,f)
   if c then return t else return f end
end

function smc:compile(expr, hole, tail_position)
   if type(expr) ~= 'table' then
      -- variable or constant
      return self:write_binding(hole, self:atom_to_c_expr(expr))
   end
   local form, tail = unpack(expr)
   assert(form)
   -- self:write('/*form: ' .. form .. "*/")
   assert(type(form) == 'string')
   -- log("compile:" .. form .. ",tail:" .. ifte(tail_position,"yes","no") .. "\n")

   local form_fn = self.form[form]
   if form_fn then
      return form_fn(self, expr, hole, tail_position)
   else
      self:apply(expr, hole, tail_position)
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
   self:write("// state size: " .. self.c_size .. "\n")
   self:write("#endif\n")


   self.write = w

   -- Second pass uses cells_last: the information gathered about each
   -- storage cell in the first pass.
   self.cells_last = self.cells

   self:reset()
   self:write("\n// second pass\n")
   self:compile(expr)
   self:write("// state size: " .. self.c_size .. "\n")
   self:write("\n")
end

-- Reset compiler state before executing a new pass.
function smc:reset()
   self.cells = {}
   self.c_size = 0
   self.sym_n = 0
   self.funs = {}
   self.depth = 0
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
