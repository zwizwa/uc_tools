-- Compiles a subset of Scheme to computed goto state machines.

-- Some ideas:
--
-- * General language structure:
--   1) a Scheme module maps to a C function.
--   2) Scheme functions map to goto labels inside the C function.
--
-- * Compiling continuations to individual C functions is too much
--   work.  Use computed goto instead: much easier to jump straight
--   into a control structure compared to separately representing
--   continuations.
--
-- * The main problem then becomes variable management, essentially
--   implementing variable storage: local for temporary variables that
--   do not need to survive past yield points, and state struct for
--   values that survive yield points.
--
-- * All non blocking primitives can just be C functions.
--
-- * Keep the stored environment simple: an array of machine words
--   essentially implementing a stack.  The compiler can guarantee the
--   stack size at compile time.
--
-- * Basic program form is scheme's let* / begin mapped to C99 blocks
--   (compound statements)
--
-- * Second pass can distinguish between variables that needs to be
--   saved, and variables that can be implemented as C local
--   variables.  This is the main added value of this compiler: to
--   allow the C compiler to optimize those local variables.
--
-- * Variable lifetime is implemented by letting variables go through
--   this cycle: unbound -> local -> lost -> saved.
--
-- * Blocking subroutines are not implemented.  Any non-tail function
--   call is inlined.
--
-- * The for(;;) C form is implemented explicitly, modeled after a
--   stripped-down Racket for form.
--
-- * Lua string concatenation is avoided by resorting to Erlang style
--   IOLists.
--
--

-- WTF why Lua?
--
-- Originally, just to see if I can add a small special-purpose state
-- machine compiler to an existing project without introducing "scary"
-- dependencies like Racket or Haskell, and as an incentive to keep it
-- simple.  It seems like that is possible, and this project gradually
-- turned into a sandbox to develop a schemisch Lua programming style.
-- Looks like I'm going to be stuck with Lua for a while it seems so
-- might as well make me feel at home...
--
-- A Racket version is planned, but currently the Lua approach
-- actually works quite well so I'm sticking with it until semantics
-- is stable.


local se     = require('lure.se')
local scheme = require('lure.scheme')
local comp   = require('lure.comp')
local iolist = require('lure.iolist')


-- Tools
require('lure.log')
local function log_w(...) iolist.write(log, {...}) end

local l = se.list
local function is_form(expr, form)
   return type(expr) == 'table' and expr[1] == form
end
local function ifte(c,t,f)
   if c then return t else return f end
end


-- Module
local smc = {}
local form = {}

-- Note that variables and cells are separate.  A variable is a Lua
-- string that refers to a cell.  Cells can be aliased, e.g. inside an
-- inlined function context, variables are typically renamed.
--
-- Cells are allocated on the C stack (if it is known that there is no
-- suspend boundary between definition and reference), or in the
-- persistent store otherwise.

-- Introduce a varible.
function smc:new_cell()
   local id = #self.cells + 1
   local cell = {id = id, bind = 'unbound', const = true}
   if (not self.cells_last) or (self.cells_last[id].bind == 'saved') then
      -- In second pass (cells_last defined) we can distinguish
      -- between variables that need to be saved in the state struct,
      -- or those that can be represented as C variables.  This
      -- optimization is the main purpose of this mini language.
      --
      -- In the first pass this information is not known, so all
      -- variables are allocated in the state struct.
      cell.c_index = self:inc('stack_ptr')
      -- Note that self.current_task is 0-based
      self:track_max_indexed('stack_size', self.current_task + 1, cell.c_index+1)
   end
   -- self.var is the list of all created variables
   table.insert(self.cells, cell)
   return cell
end

local function cvar_iolist(var)
   return {"<#cvar:",var.var,">"}
end
function smc:new_var(var_name)
   local cell = self:new_cell()
   cell.orig_var = var_name
   -- log("new_var:" .. cell.id .. ":" .. var_name .. "\n")
   return {var = var_name, cell = cell, class = 'cvar', iolist = cvar_iolist}
end

-- Introduce the variable in the current lexical scope.
function smc:push_var(v)
   self.env = se.cons(v, self.env)
   return v
end


-- Aliases are used to implement substitution for function inlining.
function smc:push_alias(alias_name, v)
   assert(v and v.cell)
   local v_alias = {var = alias_name, cell = v.cell}
   self.env = se.cons(v_alias, self.env)
   return v_alias
end

local function cell_bind(cell, bind)
   cell.bind = bind
   -- if cell.id == 6 then log_desc(cell) end
end


-- Map a variable name to variable slot in the environment.
function smc:ref(var_name, env)
   if not env then env = self.env end

   -- search starts at last pushed variable.  this implements shadowing
   for v in se.elements(env) do
      if v.var == var_name then

         if v.cell then
            -- We can only get references from the lexical
            -- environment, and that should never contain unbound
            -- variables.
            assert(v.cell.bind ~= 'unbound')

            -- If this is a variable that crossed a suspension border,
            -- mark it such that it gets stored in the state struct and
            -- not on the C stack.
            if v.cell.bind == 'lost' then
               -- self:w("/*saved:",v.var,"*/")
               cell_bind(v.cell, 'saved')
            end
         else
            -- Emphemeral, compile-time bindings created with partial
            -- evaluation.  We don't need to do any marking here.
         end
         return v
      end
   end
   -- This should probably be an error.
   log_w("env: ", se.iolist(env), "\n")
   error('unbound variable ' .. var_name)

   return nil
end

-- This is a stripped-down version of the racket 'for' form supporting
-- a single sequence.
form['for'] = function(self, for_expr)
   -- For doesn't return a value, so for now just assert there is
   -- nothing to bind.
   assert(not self.var)
   local _, bindings, inner = se.unpack(for_expr, { n = 2, tail = true })

   -- Only supports a small subset.  The iterators are compile-time
   -- constructs, not like the Racket case.
   local binding = se.unpack(bindings, { n = 1 })
   local var_name, iter_form = se.unpack(binding, { n = 2 })
   local iter_name, iter_arg = se.unpack(iter_form, { n = 2 })
   assert(iter_name == 'in-range')
   assert(type(iter_arg) == 'number')
   assert(type(var_name) == 'string')

   self:save_context(
      {'env'},
      function()
         local v = self:new_var(var_name)

         self:w(self:tab(),"for(",self:var_def(v),"0 ; ")

         self:push_var(v)
         self:mark_bound(v)
         local cv = self:atom_to_c_expr(var_name)
         self:w(cv, " < ", iter_arg, " ; ")
         self:w(cv, "++) {\n")
         self:save_context(
            {'indent'},
            function()
               self:inc('indent')
               for form in se.elements(inner) do
                  assert(form)
                  self:compile(form)
               end
            end)

         self:w(self:tab(), "}\n")
      end)
end


form['define'] = function(self, expr)
   error("'define' form not supported in first order section")
end

local function se_comment(expr)
   return {"/*",se.iolist(expr),"*/"}
end

form['if'] = function(self, if_expr)
   local _, condition, expr_true, expr_false = se.unpack(if_expr, { n = 4 })

   -- Perform let insertion if necessary.
   local li = self:let_insert()
   condition = li:maybe_insert_var(condition)
   if li:compile_inserts(l('if', condition, expr_true, expr_false)) then
      return
   end

   -- See also implementation of 'let*'.
   self:w(self:tab(), self:var_def_assign_later(self.var))

   local function compile_branch(form)
      self:save_context(
         {'env','stack_ptr','indent','var'},
         function()
            self:w("{\n") ;
            self:inc('indent')
            self:compile(form)
            self:dec('indent')
            self:w(self:tab(), "}")
         end)
   end

   local ccond = self:atom_to_c_expr(condition)

   self:w("if (",ccond,") ")
   compile_branch(expr_true);
   self:w(" else ")
   compile_branch(expr_false);
   self:w("\n")

   self:mark_bound(self.var)
end


form['let*'] = function(self, expr)
   local _, bindings, sequence = se.unpack(expr, { n = 2, tail = true })
   self:compile_letstar(bindings, sequence)
end
form['begin'] = function(self, expr)
   local _, sequence = se.unpack(expr, { n = 1, tail = true })
   self:compile_letstar(se.empty, sequence)
end
form['let'] = function(self, expr)
   -- This is here as a guard to not trigger other obscure errors.
   error("ERROR: Use 'let*' instead of 'let'\n")
   --local let = form['let*']
   --let(self,expr)
end


-- Core form is 'let*' which resembles C's scoping rules.
-- Abstracted out since we use it in a couple of places.
function smc:compile_letstar(bindings, sequence)

   self:w(self:tab(), self:var_def_assign_later(self.var), "{\n")

   self:save_context(
      {'env','stack_ptr','indent','var','tail_position'},
      function()
         self:inc('indent')

         local tail_position = self.tail_position
         local var = self.var

         -- Compile binding forms as expressions assigned to variables
         -- (self.var ~= nil).
         local nb_bindings = se.length(bindings)
         for binding in se.elements(bindings) do
            local var_name, expr = self.unpack_binding(binding, '#<void>')
            local v = self:new_var(var_name)
            self.var = v
            self.tail_position = false
            self:compile(expr, v, false)
            self:push_var(v)
         end

         -- Compile inner forms as statements (self.var == nil).
         local n_inner = se.length(sequence)
         assert(n_inner > 0)

         self.var = nil

         for form, rest_expr in se.elements(sequence) do
            assert(form)
            local last_expr = se.is_empty(rest_expr)
            self.tail_position = tail_position and last_expr
            self.var = ifte(last_expr, var, nil)
            self:compile(form)
         end
   end)

   self:w(self:tab(), "}\n")

   -- Only mark after it's actually bound in the C text.
   self:mark_bound(self.var)

end

local function is_closure(thing)
   return thing and type(thing) == 'table' and thing.class == 'closure'
end

-- current tasks's next pointer containing resume point
function smc:next(t)
   local s  = self.config.state_name
   local tc = self.current_task
   return {s, "->t", t or tc}
end

function smc:state_type()
   assert(self.module_name)
   return {"struct ", self.module_name, "_state"}
end

-- The module form compiles to a C function.  Functions with no
-- arguments compile to goto lables inside such a function.

function smc:compile_module_closure_inner(module_closure)
   local c = self.config
   local modname = self.module_name
   if self.mod_prefix then modname = { self.mod_prefix, modname } end
   assert(modname)
   assert(not self.var)

   -- Compile the bulk of the C code.  This produces some information
   -- such as the number of tasks that is necessary to generate entry
   -- code.
   local c_bulk = self:collect_output(
      function() self:compile_tasks(module_closure) end)

   -- The argument passed to the 'co' call in 'start'.
   self:w("#define ", modname, "_init ", self.init_arg, "\n")

   -- Emit main C function
   local s  = self.config.state_name
   local args = { { self:state_type(), " *", s }, {"T ", self:arg(0)} }
   self:w("T ", modname, "(",comp.clist(args),") {\n");

   -- Allocate 'registers' used for function/coroutine argument
   -- passing.  Size is defined during c_bulk compilation.
   assert(self.args_size_app)
   for i=1,self.args_size_app-1 do
      local a = self:arg(i)
      self:w(self:tab(), "T ",a , ";\n")
   end

   -- Emit code to jump to the current resume point
   local resume = {c.state_name, "->next"}
   self:w(self:tab(), "if(", resume, ") goto *", resume, ";\n")

   -- Emit init code, executed on first entry to the function.
   -- Unfortunately we do not have access to the labels outside of the
   -- function so cannot generate this ahead of time.  The alternative
   -- is to use static variables, but it seems best to keep state
   -- explicit, such that reset can be performed externally by zeroing
   -- the state.struct.
   local nb_tasks = #self.stack_size
   for i=1,nb_tasks do
      local entry = self.registries.task[i].entry
      assert(entry)
      -- Each task has a ->t member that contains the address of the
      -- resume label.  The entry points are collected during c_bulk
      -- compilation.
      self:w(self:tab(), s, "->t",i-1,"=&&t",i-1,"_",entry,";\n");
   end
   local init_task = self.init_task
   assert(init_task)
   self:w(self:tab(), "goto t",init_task.id,"_",init_task.entry,";\n")

   -- With preamble emitted, emit bulk C code.
   self:w(c_bulk)

   self:w("}\n");

end


-- Obtain the name of the closure from the closure's environment,
-- assuming it will be there because it contains mutually recursive
-- functions created by letrec.
function smc:closure_name(closure)
   assert(closure and closure.class == 'closure')
   local entry
   for var in se.elements(closure.env) do
      local cl = var.val
      if (cl.class == 'closure') then
         if cl == closure then
            entry = var.var
            assert(type(entry) == 'string')
         end
      end
   end
   assert(entry)
   return entry
end

function smc:compile_tasks(module_closure)

   -- Emit code for all functions implemented as goto labels followed
   -- by statements.  This code will be inserted in the module's
   -- function body, after the init preamble.

   -- The compilation can be summarized as follows:
   --
   -- . Interpret the function 'start' in an environment that has
   --   primitives for creating a task network.
   --
   local start = scheme.ref('start',module_closure.env)
   assert(start and start.class == 'closure')

   --
   -- . The last function called in 'start' will invoke the compiler
   --   for each task's closure.

   local function compile_task(task)
      -- This gets the task closure, e.g. the result of evaluating
      -- '(task_program)' with a definition like:
      --
      -- (define (task_program)
      --    (define (main a) (yield a) (main a))
      --    main)
      --
      -- Here '(task_program)' evaluates to a closure.  We're
      -- interested in the environment of that closure: it will
      -- contain the mutually recursive local definitions of
      -- task_program that we will be compiling to C.
      --
      -- By evaluating the task closure, we've lost the name.  We want
      -- to "re-reference" that closure to obtain the identifier for
      -- the entry function for that task.  Save it for generating
      -- bootstrap code later.
      task.entry = self:closure_name(task.closure)

      -- Compiler will generate code for functions on demand for all
      -- functions that have been referenced during compilation of
      -- other functions, starting with the entry function.  We track
      -- references with fun_def.
      local fun_defs = {}
      local function fun_def(_, fname)
         assert(fname)
         local fun = scheme.ref(fname, task.closure.env, true)
         if fun and not fun_defs[fname] then
            fun_defs[fname] = { compiled = false, closure = fun }
         end
         return fun
      end
      local function compile_function(fname)
         self:compile_fundef(fname, fun_def(self, fname))
         fun_defs[fname].compiled = true
      end
      self:parameterize({
            fun_def = fun_def,
            current_task = task.id,
         },
         function()
            -- One stack per task.  Size is known after compilation.
            self.stack_size[task.id + 1] = 0

            -- Compile the entry point.  This will start filling up
            -- fun_defs with functions that are referenced but not
            -- compiled.
            compile_function(task.entry)

            -- Keep compiling as the list grows until all are
            -- compiled.
            local did_compile
            repeat
               did_compile = false
               for fname, status in pairs(fun_defs) do
                  if not status.compiled then
                     compile_function(fname)
                     did_compile = true
                  end
               end
            until (not did_compile)
         end)
   end

   -- Primitives available during the evaluation of 'start'.  Note
   -- that thesee are not available, or not the same, as what is
   -- available during compilation of the C code.
   local prim = {}

   -- Some data constructors.
   self.registries = {
      task = {},
      channel = {},
   }
   local function new(typ)
      local registry = self.registries[typ]
      assert(registry)
      local obj = {class = typ, id = #registry}
      table.insert(registry, obj)
      self:w("// new ", typ, ": ", obj.id, "\n")
      return obj
   end
   prim['make-channel'] = function(fun)
      return new('channel')
   end
   prim['make-task'] = function(fun)
      return new('task')
   end

   -- Each task is associated to a closure that we will compile to C.
   prim['load-task!'] = function(task, closure)
      assert(task and task.class == 'task')
      assert(is_closure(closure))
      task.closure = closure
   end
   -- The 'start' function needs to end in a coroutine call into the
   -- network.  This primitive triggers the compilation to C,
   -- effecitively dumping the continuation as C code.
   prim['co'] = function(init_task, value)
      assert(type(value) == 'number')
      assert(init_task and init_task.class == 'task')
      -- Compile all the tasks that were created during 'start'.
      for _,task in ipairs(self.registries.task) do
         compile_task(task)
      end
      -- Save the coroutine that starts up the network.
      self.init_task = init_task
      -- Save the argument value.
      self.init_arg = value
   end

   -- To glue it all together, create an environment to and evaluate
   -- 'start' to put everything in motion.
   local env = scheme.push_primitives(module_closure.env, prim)
   assert(#start.args == 0)
   scheme.new():eval(start.body, env)

end



function smc:compile_fundef(fname, fclosure)

   local args = se.array_to_list(fclosure.args)
   local body_expr = fclosure.body
   local env = fclosure.env


   -- self:w("/*compile_fundef, env:",se.iolist(env),"*/")

   -- Only emit body if it is actually used.  We only have usage
   -- information in the second pass when labels_last is defined.

   local label = self:mangle_label(fname)
   -- log_desc(self.labels_last)

   local c_code =
      self:collect_output(
         function()
            -- No closure support: make sure lex env is empty.
            assert(0 == se.length(self.env))
            self:w(label, ":", se_comment(args), "\n");
            self:save_context(
               {'var','tail_position','env'},
               function()
                  self.env = env
                  self.tail_position = true
                  self.var = nil
                  local nb_args = se.length(args)
                  if nb_args == 0 then
                     self:compile(body_expr)
                  else
                     local bindings = se.empty
                     for i,var in ipairs(se.list_to_array(args)) do
                        bindings = {se.list(var, self:arg(i-1)), bindings}
                     end
                     self:compile_letstar(se.reverse(bindings), se.list(body_expr))
                  end
            end)
         end)

   if (not self.labels_last) or self.labels_last[label] then
      self:w(c_code)
   else
      -- If code is never called, don't emit it.  Note that we DO need
      -- to go through the compilation step above to ensure the
      -- iteration pattern doesn't change, so we generate the code and
      -- then drop it.
      self:w("/* ", fname, " inline only */\n")
   end
end


-- Ignore Racket forms.
form['require'] = function(self, expr) end
form['provide'] = function(self, expr) end


function smc:statement(name, ...)
   return {name, "(", comp.clist({...}), ");\n"}
end


-- Blocking forms.  See csp.h for the definition of the macros.  The
-- task we have to do here is variable storage management.

-- See also smc_cspc.lua

function smc:local_lost()
   local bound = {}
   for var in se.elements(self.env) do
      -- All visible C local variables are uninitialized when we jump
      -- into the body of a function as their.  Mark them 'lost' here.
      -- This is used later when the variable is referenced to
      -- (lazily) turn it into a 'saved' variable, so in the second
      -- pass it can be allocated in the state struct.
      if var.cell then
         if var.cell.bind == 'local' then
            table.insert(bound, n)
            cell_bind(var.cell, 'lost')
         end
      else
         -- Ephemeral variables don't need marking.
      end
   end
   -- self:w("/*lost:",se.iolist(self.env),"*/")
   return bound
end



-- C representation of variable (lvalue/rvalue), and its type.
function smc:cvar_and_ctype(v)
   -- It's not useful to print generated symbol names.
   local comment = {"/*", v.var, "*/"}
   -- if v.var:byte(1) == 59 then comment = "" end


   -- Only concrete variables are supported here.
   -- If this fails on an ephemeral variable, handle it one layer up
   if not v.cell then
      error("variable '" .. v.var .. "' is not available at run time")
   elseif v.cell.c_index then
      local s = self.config.state_name
      local t = self.current_task
      local i = v.cell.c_index
      -- return {s,"->e[",i,"]", comment}, false
      return {s,"->e",t,"[",i,"]", comment}, false
   else
      return {"r",v.cell.id,comment}, "T"
   end
end


-- Called after C code is emitted that assigns a value to the
-- variable.
function smc:mark_bound(v)
   if not v then
      -- It's simplest to just handle this case, since binding
      -- vs. ignoring is handled implicitly at most places.
      return
   end
   if v.cell.bind == 'unbound' or v.cell.assign_later then
      cell_bind(v.cell, 'local')
   end
end


-- Emit C code for variable definition.
function smc:var_def(v)
   if not v then
      -- This means: if there is no variable to be defined, don't emit
      -- any defining code.  It's simpler to push this all the way to
      -- the bottom.
      return ""
   end
   local var, typ = self:cvar_and_ctype(v)
   if v.cell.assign_later then
      -- Assume variable definition has already been written out
      -- without value.
      return {var," = "}
   else
      -- Insert the definition.
      assert(v.cell.bind == 'unbound')
      return {ifte(typ,{typ, " "}, ""), var, " = "}
   end
end

-- For variables that are defined without value and assigned later,
-- the definition is only necessary for C local variables.  If they
-- are on the stack this does not emit any C code.
--
-- Note: this has side-effect, so only call when it is actually
-- written out!
--
-- FIXME: Is there a better way to deal with this?
--
function smc:var_def_assign_later(v)
   if not v then return "" end
   if not v.cell.assign_later then
      v.cell.assign_later = true
      if not v.cell.c_index then
         local var, typ = self:cvar_and_ctype(v)
         return {ifte(typ,{typ, " "}, ""), var, "; "}
      else
         return ""
      end
   else
      -- don't re-emit definition
      return ""
   end
end


-- Write expression, and if there is a current variable 'hole', emit
-- variable definition as well.
function smc:binding(c_expr)
   local vardef = self:var_def(self.var)
   assert(c_expr)
   return {self:tab(), vardef, c_expr, ";\n"}
end

-- Map Scheme atom (const or variable) to its C representation.
function smc:atom_to_c_expr(atom)
   if type(atom) == 'string' then
      if atom:byte(1) == 95 then
         -- Underscore is reserved for references to a C variables
         -- that are not known by Scheme.  This is used e.g. to
         -- implement argument passing.
         return atom
      else
         local v = self:ref(atom)
         if v then
            local c_expr = self:cvar_and_ctype(v)
            return c_expr
         else
            -- Free variables are assumed to be struct members.
            self.free[atom] = true
            return {self.config.state_name,"->",atom,"/*free*/"}
         end
      end
   else
      -- number or other const
      return atom
   end
end

-- Type checking shorthand
local function check(typ,val)
   if type(val) == typ then return val end
   log_w(se.iolist(val))
   log("\n")
   error('not type ' .. typ)
   return val
end

-- Function arguments do seem to need some intermediate storage, since
-- there are always "two sides to the story".  I currently do not see
-- how to insert function arguments into the context without causing
-- permanent storage on the saved stack, so they are copied to
-- intermediate variables instead, giving the C compiler some room to
-- optimize.
function smc:arg(arg_nb)
   return '_' .. arg_nb
end

function smc:mangle_label(name)
   -- This creates a string instead of an iolist, because it's used to
   -- index a table.
   assert(self.current_task)
   return "t" .. self.current_task .. "_" .. name
   -- local label_prefix = {"t",self.current_task,"_"}
   -- return {label_prefix, name}
end

-- Apply function to arguments, converting all arguments to A-Normal
-- form if necessary.
--
-- https://en.wikipedia.org/wiki/A-normal_form
--
-- To simplify representation, we also bind constants to variables.
-- The C compiler can later optimize those.
--

function smc:apply(expr)
   assert(se.length(expr) > 0)
   assert(type(se.car(expr)) == 'string')

   local li = self:let_insert()
   local app_form = {}

   for subexpr in se.elements(expr) do
      table.insert(app_form, li:maybe_insert_var(subexpr))
   end

   -- If any new bindings were generated, insert a let* form and
   -- recurse into compiler.
   if li:compile_inserts(se.array_to_list(app_form)) then
      return
   end

   -- The expression is in A-Normal form.
   local fun_name = app_form[1]
   local fun_def = self:fun_def(fun_name)

   -- If there is no function definition under this name, we assume
   -- this is a C primitive.  Emit code.
   if not fun_def then
      local args = {}
      for i=2,#app_form do
         table.insert(args, self:atom_to_c_expr(app_form[i]))
      end
      local c_expr = {fun_name, "(", comp.clist(args), ")"}
      self:w(self:binding(c_expr))
      self:mark_bound(self.var)
      return
   end

   -- Compile composite function call.
   if (self.tail_position) then
      local nb_args = #app_form - 1
      local fun_args = fun_def.args
      assert(nb_args == #fun_args)
      self:track_max('args_size_app', nb_args)
      for i,arg in ipairs(fun_args) do
         self:w(self:tab(), self:arg(i-1), " = ", self:atom_to_c_expr(app_form[i+1]), ";\n")
         --self:w(self:tab(),"/*assign ", i-1, " ", arg, "*/\n")
      end
      local label = self:mangle_label(app_form[1])
      local c_expr = {"goto ", label}
      self:w(self:binding(c_expr))
      self:mark_bound(self.var)
      self.labels[label] = true
   else
      -- Non-tail calls are inlined as we do not support the call
      -- stack necessary for "real" calls.
      self:save_context(
         {'env','stack_ptr','recinl'},
         function()
            -- Recursion guard is implemented using a list.  If the
            -- function is already in the list, we error out.
            local new_recinl = {fun_name, self.recinl}
            for fun in se.elements(self.recinl) do
               if fun == fun_name then
                  local arr = se.list_to_array(se.reverse(new_recinl))
                  error(fun_name .. ": inlining loop: " .. table.concat(arr," -> "))
               end
            end
            self.recinl = new_recinl

            -- Inlining links the environment inside the function body
            -- to cells accessible through the callsite environment.
            local callsite_env = self.env
            self.env = fun_def.env
            local dbg = {fun_name}
            -- log_desc(fun_def)
            for i=1, #app_form-1 do
               local var_name = check('string',app_form[i+1])
               local arg_name = check('string',fun_def.args[i])
               local callsite_var = check('table',self:ref(var_name, callsite_env))
               self:push_alias(arg_name, callsite_var)
               table.insert(dbg, {arg_name,"=",var_name})
            end
            -- The body can then be compiled in this local environment.
            self:w(self:tab(), "/*inline:",comp.clist(dbg),
                   -- ", env:",se.iolist(fun_def.env),
                   "*/\n")
            self:compile(fun_def.body)
         end)
   end

end

-- Compilation dispatch based on expr.  If var is non-nil, it refers
-- to the variable that takes the value of the expression.  If
-- tail_position is true, this expression resides in the tail position
-- of a function definition ( where goto statements are valid. )
function smc:compile(expr)
   if type(expr) ~= 'table' then
      -- variable or constant
      self:w(self:binding(self:atom_to_c_expr(expr)))
      self:mark_bound(self.var)
      return
   end
   local form, tail = unpack(expr)
   assert(form)
   assert(type(form) == 'string')
   local form_fn = self.form[form]
   if form_fn then
      return form_fn(self, expr)
   else
      self:apply(expr)
   end
end

function smc:collect_output(thunk)
   local c_code = {}
   self:parameterize(
      { write = function(_, str) table.insert(c_code, str) end },
      thunk)
   return c_code
end

function smc:compile_module_closure_pass(closure)
   return
      self:collect_output(
         function()
            self:reset()
            self:compile_module_closure_inner(closure)
         end)
end

function smc:cell_table()
   local tab = {}
   for i,cell in ipairs(self.cells) do
      table.insert(tab, {"// ", cell.id, " ", cell.bind, " ", cell.orig_var or "?", "\n"})
   end
   return tab
end

function smc:compile_module_closure_2pass(closure)
   -- First pass
   local c_code_1 = self:compile_module_closure_pass(closure)

   -- Code output is not used. Might no longer be valid C.  But it is
   -- useful to dump this + stats for debugging.
   -- self:w_if0({c_code_1, self:cell_table()}, "first pass")
   -- Second pass uses some information from the previous pass: the
   -- information gathered about each storage cell and information
   -- about the goto labels.
   self.cells_last  = self.cells   ; self.cells = {}
   self.labels_last = self.labels  ; self.lables = {}
   -- Second pass
   local c_code_2 = self:compile_module_closure_pass(closure)

   -- If iteration pattern is the same, the tables should be the same.
   -- We just check size here.
   assert(#self.cells == #self.cells_last)

   return c_code_2
end

-- Don't bother with building intermediate representations.  The two
-- passes emit C code directly.  The the shape of the C code is almost
-- identical to the Scheme code apart from let-insertion and inlining.
-- The first pass should be valid C.  The second pass can use the
-- information gathered in the first pass to allocate variables in the
-- state struct, or on the C stack, and to omit unused function
-- definitions.
function smc:compile_module_closure(closure, config)
   assert(config)
   assert(config.module_name)
   self.module_name = config.module_name

   local c_code = self:compile_module_closure_2pass(closure)

   self:w(self:state_type(), " {\n")

   -- This is for resuming on C function entry.  Since all tasks are
   -- inlined anyway, it seems simplest to collaps task+pointer into a
   -- single pointer, and set this on function return (yield).
   self:w(self:tab(), "void *next;\n")

   -- Tasks: header + storage for stack.
   for i,size in ipairs(self.stack_size) do
      self:w(self:tab(), {"void *t", i-1, ";"})
      if size > 0 then
         self:w(" T e",i-1,"[", size, "];")
      end
      self:w("\n")
   end

   -- Globals or task local?
   for v in se.elements(self.free) do
      self:w(self:tab(), "T ", v, ";\n")
   end

   self:w("};\n")



   -- Generate the struct definition, then append the C code.
   -- self:w("struct ", self.config.state_struct, " {\n")

   -- local task = 1 -- FIXME: One per task!
   -- if self.evt_size[task] and self.evt_size[task] > 0 then
   --    -- evt_size == 0 can be used as a proxy for there not being any
   --    -- CSP calls.  FIXME: this is specific to csp.c so should
   --    -- probably be moved.
   --    self:w(self:tab(), "struct csp_task task; // ends in evt[]\n");
   --    self:w(self:tab(), "struct csp_evt evt[", self.evt_size[task], "]; // nb events used\n");

   --    self:w(self:tab(), "T e[", self.stack_size[self.current_task], "];\n")
   --    for v in pairs(self.free) do
   --       self:w(self:tab(), "T ", v, ";\n")
   --    end
   -- end

   --self:w(self:tab(), "void *next;\n")
   --self:w("};\n")

   self:w(c_code)

end


-- Reset compiler state before executing a new pass.
function smc:reset()
   -- Consistency checks: these need to come back to their reset
   -- position after a compilation pass.
   assert(0 == se.length(self.env))
   assert(0 == self.stack_ptr)
   assert(1 == self.indent)
   -- Lists
   self.cells = {}
   self.labels = {}
   self.free = {}
   -- Sizes
   self.stack_size = {}   -- one per task
   self.evt_size = {}     -- one per task
   self.args_size_app = 0
   -- Counters
   self.nb_sym = 0
   self.recinl = se.empty
   -- Current variable box
   self.var = nil
   -- Is current expression in tail position?
   self.tail_position = false
end


function smc:compile_module_file(filename)
   local basename = string.gsub(filename, "(.*/)*(.*)", "%2")
   local modname = string.gsub(basename, "(.*).sm", "%1")
   assert(modname)
   local exprs = se.read_file_multi(filename)
   local expr = {'module-begin',exprs}

   -- Evaluate using the interpreter in an empty environment.  This is
   -- ok since the file should contain only definitions.  Insert a
   -- sentinel lambda so this will evaluate to a closure that contains
   -- all the definitions in the environment.
   local scm = scheme.new()
   local cl = scm:eval({'begin',se.append(exprs,l(l('lambda',l())))})
   -- for el in se.elements(cl.env) do log_se(el) ; log("\n") end

   self:compile_module_closure(cl, { module_name = modname })
end


-- This file sms.lua implements the state machine / computed goto
-- functionality.  The scheduling primitives are implemented
-- separately, to allow future implementation of different primitives.

-- FIXME: forms should be mixin.

-- FIXME: Simpler to directly import.
function smc:import_forms(forms)
   for name,impl in pairs(forms) do
      -- log("form: " .. name .. "\n")
      self.form[name] = impl
   end
end

function smc.new(cfg)
   local config = { state_name = "s", forms = {} }
   for k,v in pairs(cfg or {}) do config[k] = v end
   local obj = { stack_ptr = 0, env = se.empty, indent = 1, config = config, form = {} }
   local function index(_,k)
      for _,tab in ipairs({obj, smc, comp}) do
         local mem = rawget(tab, k)
         if mem then return mem end
      end
   end
   setmetatable(obj, {__index = index})
   obj:import_forms(form) -- Basic forms
   for _,f in ipairs(config.forms) do
      obj:import_forms(f) -- Extension forms
   end
   return obj
end


return smc
