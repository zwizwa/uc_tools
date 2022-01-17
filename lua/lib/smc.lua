-- Compiles a subset of Scheme to sm.h style state machines.

-- Some ideas:
--
-- * General language structure:
--   1) a Scheme module maps to a C function.
--   2) Scheme functions map to goto labels inside the C function.
--
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
--   which can be used to optimize the stack allocation.
--
-- * Lifetime is implemented by letting variables go through this cycle:
--   unbound -> local -> lost -> saved
--   with the last 2 transitions not happening for all.
--
-- * Blocking subroutines are not (yet) implemented.  Any non-tail
--   function call is inlined.
--
-- * The for(;;) C form is implemented explicitly, modeled after a
--   stripped-down Racket for form.
--
-- * Split string construction (Erlang style IOLists) and writing as
--   much as possible.
--
--

-- WTF why Lua?
--
-- Originally, just to see if I can add a small special-purpose state
-- machine compiler to an existing project without introducing "scary"
-- dependencies like Racket or Haskell, and as an incentive to build
-- something simple first.  It seems like that is possible, and this
-- project gradually turned into a sandbox to develop a schemisch Lua
-- programming style.  Looks like I'm going to be stuck with Lua for a
-- while it seems so might as well make me feel at home...
--
-- After adding the CSP forms and the case statement generation, I do
-- think this is getting a bit too complex and hard to read.
-- Especially hard to refactor.  Conclusion: yes possible in Lua, but
-- there is a point where the stateful nature starts to get in the
-- way.  Also there is a limit to 2-pass compilation.  At some point
-- the tracking needed becomes too spread out.  I started
-- transliteration to Racket.  That also informs some cleanup here.


local se     = require('lib.se')
local scheme = require('lib.scheme')
local comp   = require('lib.comp')


-- Tools
require('lib.log')

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
      self:track_max('stack_size', cell.c_index+1)
   end
   -- self.var is the list of all created variables
   table.insert(self.cells, cell)
   return cell
end

function smc:new_var(var_name)
   return {var = var_name, cell = self:new_cell()}
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

-- Map a variable name to variable slot in the environment.
function smc:ref(var_name, env)
   if not env then env = self.env end

   -- search starts at last pushed variable.  this implements shadowing
   for v in se.elements(env) do
      if v.var == var_name then
         -- We can only get references from the lexical environment,
         -- and that should never contain unbound variables.
         assert(v.cell.bind ~= 'unbound')

         -- If this is a variable that crossed a suspension border,
         -- mark it such that it gets stored in the state struct and
         -- not on the C stack.
         if v.cell.bind == 'lost' then
            v.cell.bind = 'saved'
         end
         return v
      end
   end
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

local function se_comment(expr)
   return {"/*",se.iolist(expr),"*/"}
end
function smc:se_comment_i_n(expr)
   return {self:tab(),se_comment(expr),"\n"}
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
   -- We use statement expressions here as well, so write var def here
   -- and propagate var=nil since value of last expression in
   -- statement expression eventually ends up in this variable.

   self:w(self:tab(), self:var_def(self.var))

   local function compile_branch(form)
      self:save_context(
         {'env','stack_ptr','indent','var'},
         function()
            self:inc('indent')
            self.var = nil
            self:w("({\n") ;
            self:compile(form)
            self:w(self:tab(), "})")
         end)
   end

   local ccond = self:atom_to_c_expr(condition)

   self:w(ccond, " ? ")
   compile_branch(expr_true);
   self:w(" : ")
   compile_branch(expr_false);
   self:w(";\n")

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


-- Core form is 'let*' which resembles C's scoping rules.
-- Abstracted out since we use it in a couple of places.
function smc:compile_letstar(bindings, sequence)

   self:w(self:tab(),self:var_def(self.var),"({\n")

   self:save_context(
      {'env','stack_ptr','indent','var','tail_position'},
      function()
         self:inc('indent')

         local tail_position = self.tail_position

         -- Compile binding forms as expressions assigned to variables
         -- (self.var ~= nil).
         local nb_bindings = se.length(bindings)
         for binding in se.elements(bindings) do
            local var_name, expr = se.unpack(binding, { n = 2 })
            assert(type(var_name) == 'string')
            assert(expr)
            local v = self:new_var(var_name)
            self.var = v
            self.tail_position = false
            self:compile(expr, v, false)
            self:push_var(v)
         end

         -- Compile inner forms as statements (self.var == nil).
         -- C handles value passing of the last statement since we're
         -- inside a statement expression.
         local n_inner = se.length(sequence)
         assert(n_inner > 0)

         self.var = nil

         for form, rest_expr in se.elements(sequence) do
            assert(form)
            self.tail_position = tail_position and se.is_empty(rest_expr)
            -- Var is nil because we use statement expressions.
            self.var = nil
            self:compile(form)
         end
   end)

   self:w(self:tab(), "});\n")

   -- Only mark after it's actually bound in the C text.
   self:mark_bound(self.var)

end


local function for_begin(begin_expr, def, other)
   local _begin, exprs = se.unpack(begin_expr, {n = 1, tail = true})
   assert(_begin == 'begin' or _begin == 'module-begin')
   for expr in se.elements(exprs) do
      -- FIXME: This only supports (define (name . args) ...)
      -- Maybe generalize to proper Scheme later?
      if ('define' == se.car(expr)) then
         local define, fun_spec, body_exprs =
            se.unpack(expr, {n = 2, tail = true})
         local body_expr = {'begin', body_exprs}
         assert(define == 'define')
         local fname, args = se.unpack(fun_spec, {n = 1, tail = true})
         assert(body_expr)
         def(fname, args, body_expr)
      else
         if other then
            other(expr)
         end
      end
   end
end

local function collect_defs(self, tab, begin_expr)
   for_begin(
      begin_expr,
      function(fname, args, body_expr)
         assert(nil == tab[fname])
         assert(type(fname == 'string'))
         assert(body_expr)
         -- Keep track of storage needed for function calls
         -- FIXME: Add a separate pass for this, then remove 'self' arg.
         self:track_max('args_size_def', se.length(args))
         -- Keep track of syntax for later inlining.  This is the same
         -- representation as scheme.lua closures.
         tab[fname] = {
            class = 'closure',
            name = fname,
            args = se.list_to_array(args),
            body = body_expr,
            env = se.empty,
         }
   end)
end


local function is_closure(thing)
   return thing and type(thing) == 'table' and thing.class == 'closure'
end

-- The module form compiles to a C function.  Functions with no
-- arguments compile to goto lables inside such a function.

form['module-begin'] = function(self, expr)
   local c = self.config
   assert(not self.var)
   local _, define_exprs = se.unpack(expr, {n = 1, tail = true})
   local modname = "testmod" -- FIXME


   -- We iterate twice over the definitions.  first iteration builds
   -- the name to syntax map to allow for back-references and function
   -- inlining and to collect information necessary for allocation.
   -- also just collect the data expressions.

   -- The toplevel bindings are unique, so collect them in a table.
   self.funs = {}

   collect_defs(self, self.funs, expr)

   -- Emit main C function
   if self.mod_prefix then modname = { self.mod_prefix, modname } end
   local args = { { "struct ", c.state_struct, " *", c.state_name } }
   self:w("T ", modname, "(",comp.clist(args),") {\n");

   -- Allocate 'registers' usef for function/coroutine argument
   -- passing.
   --
   -- In the second compiler pass, the max number of arguments
   -- actually used in function application is known.  in first pass
   -- it is not, and we bound it by max arguments of definitions.
   local nxt = {c.state_name, "->next"};
   local max_nb_args = self.args_size_app_last or self.args_size_def
   for i=1,max_nb_args do
      self:w(self:tab(), "T ", self:arg(i-1), ";\n")
   end

   -- Emit code to jump to the current resume point
   self:w(self:tab(), "if(", nxt, ") goto *", nxt, ";\n")

   -- The C cursor is now at a point where we can start emitting
   -- functions implemented as goto labels followed by statements.

   -- We don't do this directly from source code.  Instead we start
   -- interpreting a Scheme function 'start', that will call 'spawn!'
   -- to start tasks.

   -- The border between evaluation and compilation is that function
   -- 'spawn!' which is passed a closure.
   local prim = {}
   prim['spawn!'] = function(closure)
      assert(is_closure(closure))
      self:w("// spawn1: ", se.iolist(closure.body), "\n")

      -- Conceptually, spawn! evaluates the closure in a new task
      -- context.  This is implemented by generating the code for the
      -- task at current C cursor.

      -- But there is a complication: the expression might be wrapped
      -- in a number of lambdas and apps.  In order to find the proper
      -- point, we single-step an interpreter.
      local s = {env = closure.env, expr = closure.body}
      local scm = scheme.new({})
      local function step() scm:eval_step(s) end
      -- Now we don't have a good stop criterion yet.  For now just
      -- stop when the first expression is a definition.
      step(s)
      step(s)
      self:w("// spawn2: ", se.iolist(s.expr), "\n")

      -- Collect the definitions
      local defs = {}
      collect_defs(self, defs, s.expr)
      log_desc({defs = defs})

      -- And compile.
      -- The lexical context consists of:
      -- 1. Top level defines (can still be used as inline functions)
      -- 2. Defines in the program's body (compiled as goto labels)
      -- So we need to build it from scratch
      local ctx = {}
      for k,v in pairs(self.funs) do ctx[k]=v end
      for k,v in pairs(defs)      do ctx[k]=v end
      self:parameterize(
         { funs = ctx },
         function()
            local entry = {}
            for_begin(
               s.expr,
               function(n,a,b)
                  -- All definitions are compiled
                  self:w("// fun ", n, "\n")
                  self:compile_fundef(n,a,b)
               end,
               function(other)
                  -- This handles (define ...) ... <init-expr> ...
                  -- But does re-ordering so will accept incorrect
                  -- interleaving of definitions and statements.
                  self:w("// other: ", se.iolist(other), "\n")
                  table.insert(entry, other)
            end)
            -- Collect the other forms as entry code.
            -- FIXME: generate the label name?
            self:compile_fundef('entry',se.empty,{'begin',se.array_to_list(entry)})
         end)


      -- log_desc({compile_closure = closure})
      -- log_desc({spawn_closure_env = closure.env})

      -- each closure is treated as an entry into the compiled space.
      -- the 'begin' form it contains will contain definitions.

      --local begin, exprs = se.unpack(closure.body, { n = 1, tail = true })
      --assert(begin == 'begin')


      --local defs = {}
      --collect_defs(self, defs, exprs)
      -- log_se(exprs)
      -- log_desc({defs = defs})
   end

   local task_nb = 0
   prim['make-task'] = function(fun)
      self:w("// make-task\n")
      local nb = task_nb
      task_nb = task_nb + 1
      return nb
   end

   local start = self.funs.start
   assert(start)
   scheme.new({self.funs, prim}):eval(start.body)


   -- FIXME: OLD
   -- for_begin_forms(define_exprs, function(n,a,b) self:compile_fundef(n,a,b) end)

   self:w("}\n");

end


function smc:compile_fundef(fname, args, body_expr)
   -- Only emit body if it is actually used.  We only have usage
   -- information in the second pass when labels_last is defined.
   if (not self.labels_last) or self.labels_last[fname] then
      -- No closure support: make sure lex env is empty.
      assert(0 == se.length(self.env))
      self:w(fname, ":", se_comment(args), "\n");
      self:save_context(
         {'var','tail_position'},
         function()
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
   else
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
      if var.cell.bind == 'local' then
         table.insert(bound, n)
         var.cell.bind = 'lost'
      end
   end
   return bound
end



-- C representation of variable (lvalue/rvalue), and its type.
function smc:cvar_and_ctype(v)
   local comment = {"/*", v.var, "*/"}
   if v.cell.c_index then
      return {self.config.state_name,"->e[",v.cell.c_index,"]", comment}
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
      v.cell.bind = 'local'
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
function smc:var_def_assign_later(v)
   if not v then return "" end
   v.cell.assign_later = true
   if not v.cell.c_index then
      local var, typ = self:cvar_and_ctype(v)
      return {ifte(typ,{typ, " "}, ""), var, "; "}
   else
      return ""
   end
end


-- Write expression, and if there is a current variable 'hole', emit
-- variable definition as well.
function smc:binding(c_expr)
   return {self:tab(), self:var_def(self.var), c_expr, ";\n"};
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
   local fun_def = self.funs[fun_name]

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
      local c_expr = {"goto ",app_form[1]}
      self:w(self:binding(c_expr))
      self:mark_bound(self.var)
      self.labels[fun_name] = true
   else
      -- Non-tail calls are inlined as we do not support the call
      -- stack necessary for "real" calls.
      self:save_context(
         {'env','stack_ptr','depth'},
         function()
            -- FIXME: ad-hoc infinite loop guard
            assert(self:inc('depth') < 10)

            -- Inlining links the environment inside the function body
            -- to cells accessible through the callsite environment.
            local callsite_env = self.env
            self.env = se.empty
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
            self:w(self:tab(), "/*inline:",comp.clist(dbg),"*/\n")
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


-- Don't bother with building intermediate representations.  The two
-- passes emit C code directly.  The the shape of the C code is almost
-- identical to the Scheme code apart from let-insertion and inlining.
-- The first pass should be valid C.  The second pass can use the
-- information gathered in the first pass to allocate variables in the
-- state struct, or on the C stack, and to omit unused function
-- definitions.
function smc:compile_passes(expr)
   self:save_context(
      {'write','mod_prefix'},
      function()
         -- Override to suppress printing of first pass C output, which is
         -- only neccessary for debugging variable allocation.
         -- self.config.first_pass_prefix = "pass1_"  -- FIXME

         if not self.config.first_pass_prefix then
            self.write = function() end
         else
            self.mod_prefix = self.config.first_pass_prefix
         end

         self:reset()
         self:w("\n// first pass\n")
         -- FIXME: First pass does not generate valid C at this point.
         self:w("#if 0\n")
         self:compile(expr)
         self:w("// stack_size: ",self.stack_size,"\n")
         self:w("#endif\n")
      end)

   -- Second pass uses some information from the previous pass: the
   -- information gathered about each storage cell, information about
   -- the goto labels, and nb vars necessary for argument passing.
   self.cells_last         = self.cells
   -- self.labels_last        = self.labels
   self.args_size_app_last = self.args_size_app

   local c_code = {}
   self:save_context(
      {'write'},
      function()
         self.write = function(_, str)
            table.insert(c_code, str)
         end
         self:reset()
         self:w("\n// second pass\n")
         self:compile(expr)
         self:w("// stack_size: ",self.stack_size,"\n")
         self:w("\n")
      end)

   -- Generate the struct definition, then append the C code.
   self:w("struct ", self.config.state_struct, " {\n")
   if self.evt_size > 0 then
      -- evt_size == 0 can be used as a proxy for there not being any
      -- CSP calls.  FIXME: this is specific to csp.c so should
      -- probably be moved.
      self:w(self:tab(), "struct csp_task task; // ends in evt[]\n");
      self:w(self:tab(), "struct csp_evt evt[", self.evt_size, "]; // nb events used\n");
   end
   self:w(self:tab(), "void *next;\n")

   self:w(self:tab(), "T e[", self.stack_size, "];\n")
   for v in pairs(self.free) do
      self:w(self:tab(), "T ", v, ";\n")
   end
   self:w("};\n")

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
   self.funs = {}
   self.free = {}
   -- Sizes
   self.stack_size = 0
   self.evt_size = 0
   self.args_size_app = 0
   self.args_size_def = 0
   -- Counters
   self.nb_sym = 0
   self.depth = 0
   -- Current variable box
   self.var = nil
   -- Is current expression in tail position?
   self.tail_position = false
end


function smc:compile_module_file(filename)
   local stream = io.open(filename,"r") or error("Can't open '" .. filename .. "'")
   local parser = se.new(stream)
   parser.log = function(self, str) io.stderr:write(str) end
   local exprs = parser:read_multi()
   local expr = {'module-begin',exprs}
   self:se_comment_i_n(expr)
   stream:close()
   self:compile_passes(expr)
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
   local config = { state_name = "s", state_struct = "state", forms = {} }
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
