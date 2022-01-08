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
local prompt = require('prompt')
local function log(str)
   io.stderr:write(str)
end
local function log_desc(thing)
   log(prompt.describe(thing))
   log("\n")
end
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
smc.form = form


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

-- Let insertion happens often enough, so make an abstraction.
local let_insert = {}
function smc:let_insert()
   local obj = {comp = self, bindings_list = se.empty}
   setmetatable(obj, {__index = let_insert})
   return obj
end
function let_insert:maybe_insert_var(expr)
   if type(expr) == 'string' then return expr end
   local var = self.comp:gensym()
   self.bindings_list = {l(var, expr), self.bindings_list}
   return var
end
function let_insert:compile(inner)
   self.comp:compile_letstar(self.bindings_list, l(inner))
end
function let_insert:compile_inserts(inner)
   if not self:bindings() then return false end
   self:compile(inner)
   return true
end
function let_insert:bindings()
   return not se.is_empty(self.bindings_list)
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
            self:compile(form)
         end)
   end

   local ccond = self:atom_to_c_expr(condition)

   self:w(ccond, " ? ({\n")
   compile_branch(expr_true)
   self:w(self:tab(), "}) : ({\n")
   compile_branch(expr_false)
   self:w(self:tab(), "});\n")

   self:mark_bound(self.var)
end




form['let*'] = function(self, expr)
   local _, bindings, sequence = se.unpack(expr, { n = 2, tail = true })
   assert(type(bindings) == 'table')
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


-- The module form compiles to a C function.  Functions with no
-- arguments compile to goto lables inside such a function.

form['module-begin'] = function(self, expr)
   local c = self.config
   assert(not self.var)
   local _, define_exprs = se.unpack(expr, {n = 1, tail = true})
   local modname = "testmod" -- FIXME

   if self.mod_prefix then modname = { self.mod_prefix, modname } end
   local args = { { "struct ", c.state_struct, " *", c.state_name } }
   self:w("T ", modname, "(",comp.clist(args),") {\n");

   local nxt = {c.state_name, "->next"};
   self:w(self:tab(), "if(", nxt, ") goto *", nxt, ";\n")

   -- 'define' is only defined inside a 'module' form.
   local function for_defines(f)
      for define_expr in se.elements(define_exprs) do
         local define, fun_spec, body_exprs =
            se.unpack(define_expr, {n = 2, tail = true})
         local body_expr = {'begin', body_exprs}
         assert(define == 'define')
         local fname, args = se.unpack(fun_spec, {n = 1, tail = true})
         assert(body_expr)
         f(fname, args, body_expr)
      end
   end

   -- perform two passes to allow for backreferences.
   for_defines(
      function(fname, args, body_expr)
         assert(type(fname == 'string'))
         assert(body_expr)
         -- Keep track of syntax for later inlining.
         self.funs[fname] = {
            class = 'function', name = fname,
            args = args, body = body_expr,
         }
      end)

   for_defines(
      function(fname, args, body_expr)
         -- Only emit body if it is actually used.  We only have usage
         -- information in the second pass when labels_last is defined.
         if (not self.labels_last) or self.labels_last[fname] then
            -- No closure support: make sure lex env is empty.
            assert(0 == se.length(self.env))
            self:w(fname, ":\n");
            self:save_context(
               {'var','tail_position'},
               function()
                  self.tail_position = true
                  self.var = nil
                  self:compile(body_expr)
               end)
         else
            self:w("/* ", fname, " inline only */\n")
         end
      end)

   self:w("}\n");

end

-- Ignore Racket forms.
form['require'] = function(self, expr) end
form['provide'] = function(self, expr) end


function smc:statement(name, ...)
   return {name, "(", comp.clist({...}), ");\n"}
end


-- Blocking forms.  See csp.h for the definition of the macros.  The
-- task we have to do here is variable storage management.

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

form['read'] = function(self, expr)
   local _, chan = se.unpack(expr, {n = 2})
   self:local_lost()
   local s = self.config.state_name
   local t = {"&(", s, "->task)"}
   self:w(self:tab(),
          self:var_def(self.var),
          self:statement("CSP_RCV_W", t, s, chan))
   self:track_max('nb_evt', 1)
   self:mark_bound(self.var)
end

form['write'] = function(self, expr)
   local _, chan, data_expr = se.unpack(expr, {n = 3})

   local li = self:let_insert()
   data_expr = li:maybe_insert_var(data_expr)
   if li:compile_inserts(l('write', chan, data_expr)) then
      return
   end

   -- Perform the reference _before_ marking the context as lost.  Our
   -- reference here is used to initialize the evt msg before
   -- executing return.
   local cvar = self:atom_to_c_expr(data_expr)

   self:local_lost()
   local s = self.config.state_name
   local t = {"&(", s, "->task)"}
   self:w(self:tab(),self:statement("CSP_SND_W", t, s, chan, cvar))
   self:track_max('nb_evt', 1)
end

-- This is what it is all about.
--
-- Generate csp.h CSP_EVT and CSP_SEL macro invocations + dispatch on
-- ->selected, starting from a multi-clause Scheme form:
--
-- (select
--   ((read  0 <var>) (... ref <var> ...))
--   ((write 1 <val>) (...))
--
-- This call blocks execution and resumes exactly one of the clauses.


-- FIXME
form['select'] = function(self, expr)
   local _, clauses_expr = se.unpack(expr, {n = 1, tail = true})
   -- Collect read and write clauses separately.  They need to be
   -- sorted before invoking CSP_SEL.
   local clauses = { read = {}, write = {} }
   -- Also rebuild the expression in case let insertion is necessary
   local clauses_expr1 = se.empty
   -- Keep track of bindings to perform ANF transformation if necessary
   local li = self:let_insert()
   for clause_expr in se.elements(clauses_expr) do
      self:w(self:se_comment_i_n(clause_expr))
      local head_expr, handle_expr = se.unpack(clause_expr, {n = 2})
      local kind, chan, data_expr  = se.unpack(head_expr,   {n = 3})
      if kind == 'write' then
         -- Possibly needs let insertion.
         data_expr = li:maybe_insert_var(data_expr)
      else
         assert(kind == 'read')
      end
      assert(type(data_expr) == 'string')
      table.insert(clauses[kind],
                   {chan = chan,
                    var  = data_expr,
                    expr = handle_expr})
      clauses_expr1 = {l(l(kind, chan, data_expr), handle_expr),
                       clauses_expr1}
   end

   -- Insert let if there were any non-variable forms.
   if li:compile_inserts({'select', se.reverse(clauses_expr1)}) then
      return
   end

   local s = self.config.state_name
   local t = {"&(",s,"->task)"}

   local function w(...)
      self:w(self:tab(),{...})
   end

   -- The C case statement needs separate variable definition and
   -- assignment.  For this the variable is marked as assign_later, such
   -- that subsequent binding operations ignore that the variable has
   -- already been bound and emit an assignment insted of a
   -- definition.
   self:w(self:tab(), self:var_def_assign_later(self.var), "{\n")
   self:save_context(
      {'indent','env','stack_ptr'},
      function()
         self:inc('indent')

         -- Note: the CSP scheduler is used in zero-copy mode by
         -- setting rcv buffer pointer to NULL.  Further we only use
         -- the msg_buf in unboxed mode (unitptr_t machine word
         -- .msg.buf.w).
         local function stmt(...)
            self:w(self:tab(),self:statement(unpack({...})))
         end

         local n_w = #(clauses.write)
         local n_r = #(clauses.read)
         self:track_max('nb_evt', n_w + n_r)

         for i,c in ipairs(clauses.write) do
            local cvar = self:atom_to_c_expr(c.var)
            stmt("CSP_EVT_BUF",t,i-1,c.chan,cvar,0)
         end
         for i,c in ipairs(clauses.read) do
            stmt("CSP_EVT_BUF",t,n_w+i-1,c.chan,"NULL",0)
         end

         stmt("CSP_SEL",t,s,n_w,n_r)
         self:local_lost()

         local function w_case(evt,c,bind_var)
            w("case ", evt, ": {\n")
            self:save_context(
               {'indent','env','stack_ptr'},
               function()
                  self:inc('indent')
                  if bind_var then
                     local v = self:new_var(bind_var)
                     self:w(self:tab(),self:var_def(v),
                            s,"->evt[",evt,"].msg.w;\n")
                     self:mark_bound(v)
                     self:push_var(v)
                  end
                  self:compile(c.expr)
                  w("} break;\n");
               end)
         end

         w("switch((",t,")->selected) {\n")
         for i,c in ipairs(clauses.write) do w_case(i-1,c) end
         for i,c in ipairs(clauses.read)  do w_case(n_w+i-1,c,c.var) end
         w("}\n")
   end)
   w("};\n")


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
      local v = self:ref(atom)
      if v then
         local c_expr = self:cvar_and_ctype(v)
         return c_expr
      else
         -- Keep track of free variables.
         self.free[atom] = true
         return {self.config.state_name,"->",atom,"/*free*/"}
      end
   else
      -- number or other const
      return atom
   end
end

-- Type checking shorthand
local function check(typ,val)
   assert(type(val) == typ)
   return val
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
      assert(#app_form == 1)
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
   -- information gathered about each storage cell, and information
   -- about the goto labels.
   self.cells_last  = self.cells
   self.labels_last = self.labels

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
   self:w(self:tab(), "struct csp_task task; // ends in evt[]\n");
   self:w(self:tab(), "struct csp_evt evt[", self.nb_evt, "]; // nb events used\n");
   self:w(self:tab(), "void *next;\n")

   self:w(self:tab(), "T e[", self.stack_size, "];\n")
   for v in pairs(self.free) do
      self:w(self:tab(), "T ", v, ";\n")
   end
   self:w("};\n")

   self:w(c_code)


   -- The start function is executed at compile time.
   local start = self.funs.start
   local prim = {}
   function prim.spawn(fun, arg)
      self:w("// spawn: ", fun.name, " ", arg, "\n")
   end
   if start then
      scheme.new({self.funs, prim}):eval(start.body)
   end


end

-- Reset compiler state before executing a new pass.
function smc:reset()
   self.cells = {}
   self.labels = {}
   self.stack_size = 0
   self.sym_n = 0
   self.funs = {}
   self.depth = 0
   self.free = {}
   self.nb_evt = 0
   self.var = nil
   self.tail_position = false
   assert(0 == se.length(self.env))
   assert(0 == self.stack_ptr)
   assert(1 == self.indent)
end


function smc:compile_module_file(filename)
   local stream = io.open(filename,"r")
   local parser = se.new(stream)
   parser.log = function(self, str) io.stderr:write(str) end
   local exprs = parser:read_multi()
   local expr = {'module-begin',exprs}
   self:se_comment_i_n(expr)
   stream:close()
   self:compile_passes(expr)
end




function smc.new()
   local config = { state_name = "s", state_struct = "state" }
   local obj = { stack_ptr = 0, env = se.empty, indent = 1, config = config }
   local function index(_,k)
      for _,tab in ipairs({obj, smc, comp}) do
         local mem = rawget(tab, k)
         if mem then return mem end
      end
   end
   setmetatable(obj, {__index = index})
   return obj
end


return smc
