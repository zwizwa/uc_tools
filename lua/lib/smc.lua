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

function smc:tab()
   local strs = {}
   for i=1,self.indent do
      table.insert(strs,"  ")
   end
   return table.concat(strs,"")
end

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
   local cell = {id = id, bind = 'unbound'}
   if (not self.cells_last) or (self.cells_last[id].bind == 'saved') then
      -- In second pass (cells_last defined) we can distinguish
      -- between variables that need to be saved in the state struct,
      -- or those that can be represented as C variables.  This
      -- optimization is the main purpose of this mini language.
      --
      -- In the first pass this information is not known, so all
      -- variables are allocated in the state struct.
      cell.c_index = self.stack_ptr
      self.stack_ptr = self.stack_ptr + 1
      if self.stack_ptr > self.stack_size then
         self.stack_size = self.stack_ptr
      end
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
         -- and those should never contain unbound variables.
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
form['for'] = function(self, for_expr, hole)
   -- For doesn't return a value, so for now just assert there is
   -- nothing to bind.
   assert(not hole)
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

         self:write(self:tab())
         self:write("for(")
         self:write_var_def(v)
         self:write("0 ; ")

         self:push_var(v)
         self:mark_bound(v)
         local cv = self:atom_to_c_expr(var_name)
         self:write(cv .. " < " .. iter_arg .. " ; ")
         self:write(cv .. "++) {\n")
         self.indent = self.indent + 1
         for form in se.elements(inner) do
            assert(form)
            self:compile(form, nil)
         end
         self.indent = self.indent - 1

         self:write(self:tab() .. "}\n")
      end)
end

function smc:write_se(expr)
   if type(expr) ~= 'table' then
      self:write(expr)
   else
      self:write("(")
      for el, rest in se.elements(expr) do
         self:write_se(el)
         if se.is_pair(rest) then
            self:write(" ")
         end
      end
      self:write(")")
   end
end
function smc:write_se_comment(expr)
   self:write("/*")
   self:write_se(expr)
   self:write("*/")
end

local l = se.list


form['if'] = function(self, if_expr, hole, tail_position)
   local _, condition, expr_true, expr_false = se.unpack(if_expr, { n = 4 })

   -- Perform let insertion when the condition is not a variable.
   if type(condition) ~= 'string' then
      local cond_var = self:gensym()
      self:compile(
         l('let*', l(l(cond_var, condition)),
           l('if', cond_var, expr_true, expr_false)),
         hole,
         tail_position)
      return
   end


   -- See also implementation of 'let*'.
   -- We use statement expressions here as well, so write var def here
   -- and propagate hole=nil since value of last expression in
   -- statement expression eventually ends up in this variable.
   self:write(self:tab())
   if hole then
      -- FIXME: In tail position this doesn't make any sense.
      self:write_var_def(hole)
   end

   local function compile_branch(form)
      self:save_context(
         {'env','stack_ptr','indent'},
         function()
            self.indent = self.indent + 1
            self:compile(form, nil, tail_position)
         end)
   end

   local ccond = self:atom_to_c_expr(condition)

   self:write(ccond .. " ? ({\n")
   compile_branch(expr_true)
   self:write(self:tab() .. "}) : ({\n")
   compile_branch(expr_false)
   self:write(self:tab() .. "});\n")

   if hole then
      self:mark_bound(hole)
   end
end


-- Tracking the language's lexical scope can be implemented using
-- dynamic scope in the compiler, as it recurses into the syntax.
-- This function saves and restores a list of compiler keys
-- (e.g. 'env', 'stack_ptr', 'indent').  Note that 'env' is
-- implemented using a cons list instead of a hash table to facilitate
-- sharing.
function smc:save_context(keys, inner_fun)
   local saved = {}
   for i,key in ipairs(keys) do saved[key] = self[key]  end
   local rv = inner_fun()
   for i,key in ipairs(keys) do self[key]  = saved[key] end
   return rv
end


-- Core form is 'let*' which mostly resembles C's scoping rules.
-- Compile the form to C statement expressions.
form['let*'] = function(self, let_expr, hole, tail_position)
   local _, bindings, inner = se.unpack(let_expr, { n = 2, tail = true })

   assert(type(bindings) == 'table')

   self:write(self:tab())
   if hole then
      self:write_var_def(hole)
   end
   self:write("({\n")

   self:save_context(
      {'env','stack_ptr','indent'},
      function()
         self.indent = self.indent + 1

         local nb_bindings = se.length(bindings)
         for binding in se.elements(bindings) do
            local var_name, expr = se.unpack(binding, { n = 2 })
            assert(type(var_name) == 'string')
            assert(expr)
            local v = self:new_var(var_name)
            self:compile(expr, v, false)
            self:push_var(v)
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

   self:write(self:tab() .. "});\n")

   -- Only mark after it's actually bound in the C text.
   if hole then
      self:mark_bound(hole)
   end

end

-- The module form compiles to a C function.  Functions with no
-- arguments compile to goto lables inside such a function.

form['module'] = function(self, expr, hole)
   assert(hole == nil)
   local _, mod_spec, define_exprs = se.unpack(expr, {n = 2, tail = true})
   local modname = se.unpack(mod_spec, {n = 1})
   if self.mod_prefix then
      modname = self.mod_prefix .. modname
   end
   local c = self.config
   local args = {
      "struct " .. c.state_struct .. " *" .. c.state_name,
      -- Note that since the main purpose of this ended up compiling
      -- CSP tasks, the next pointer and communication data is best
      -- kept in the state struct.  No point in trying to optimize it
      -- into registers.
      --
      -- "void *next",
      -- "T arg",
   }

   self:write("T " .. modname .. "(")
   self:write(table.concat(args,", "))
   self:write(") {\n");
   local nxt = c.state_name .. "->next";
   -- local nxt = "next";
   self:write(self:tab() .. "if(" .. nxt .. ") goto *" .. nxt .. ";\n")

   -- 'define' is only defined inside a 'module' form.
   local function for_defines(f)
      for define_expr in se.elements(define_exprs) do
         local define, fun_spec, body_expr = se.unpack(define_expr, {n = 3})
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
         self.funs[fname] = {args = args, body = body_expr}
      end)
   for_defines(
      function(fname, args, body_expr)
         -- Only emit body if it is actually used.  We only have usage
         -- information in the second pass when labels_last is defined.
         if (not self.labels_last) or self.labels_last[fname] then
            -- No closure support: make sure lex env is empty.
            assert(0 == se.length(self.env))
            self:write(fname .. ":\n");
            self:compile(body_expr, nil, true)
         else
            self:write("/* " .. fname .. " inline only */\n")
         end
      end)

   self:write("}\n");
end

function smc:write_statement(name, ...)
   self:write(self:tab() .. name .. "(")
   self:write(table.concat({...},","))
   self:write(");\n")
end


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


-- Blocking form.  The control structure itself is implemented
-- separately in a C macro, specialized to the state machine
-- scheduler.  It is not of concern here.  We only need to manage the
-- lexical varariables.
form['read'] = function(self, expr, hole)
   local _, chan = se.unpack(expr, {n = 2})
   -- self:assert(type(chan) == 'string')

   self:local_lost()

   local s = self.config.state_name
   -- Might as well generate the jump label here.
   local label = self:gensym("l");

   -- Emit a csp.h style rendezvous setup sequence.
   local t = "&(" .. s .. "->task)"

   self:write(self:tab())
   if hole then
      self:write_var_def(hole)
   end
   self:write("({\n")

   self:save_context(
      {'indent','env','stack_ptr'},
      function()
         self.indent = self.indent + 1

         -- The rendez-vous buffer is a saved variable.
         --local rv_name = self:gensym()
         --local rv_var = self:new_var(rv_name)
         --rv_var.cell.bind = 'saved'
         --self:push_var(rv_var)
         --local rv_cvar = self:atom_to_c_expr(rv_name)

         -- Note: the CSP scheduler is used in shared memory
         -- configuration by setting rcv buffer pointer to NULL.
         -- Further we only use the msg_buf in unboxed mode (unitptr_t
         -- machine word .msg.buf.w).

         -- Note that this can just be a C macro.  No need to make
         -- this verbose in the generated code.

         local nb_evt = 1
         for evt=1,nb_evt do
            self:write_statement("CSP_EVT_BUF",t,evt-1,chan,"NULL",0)
         end
         self:write_statement("CSP_SEL",t,s,0,nb_evt)
         -- This is only valid up to the next blocking point so we
         -- need to copy the value.

         self:write(self:tab() .. s .. "->evt[0].msg.w;\n")
   end)

   self:write(self:tab() .. "});\n")
   self:mark_bound(hole)
end


-- C representation of variable (lvalue/rvalue), and its type.
function smc:var_and_type(v)
   local comment = "/*" .. v.var .. "*/"
   if v.cell.c_index then
      return self.config.state_name .. "->e[" .. v.cell.c_index .. "]" .. comment, ""
   else
      return "l" .. v.cell.id .. comment, "T "
   end
end

-- Just the lvalue/rvalue.
function smc:var(n)
   local c_expr, c_type = self:var_and_type(n)
   return c_expr
end

-- Called after C code is emitted that assigns a value to the
-- variable.
function smc:mark_bound(v)
   assert(v.cell.bind == 'unbound')
   v.cell.bind = 'local'
end

-- Emit C code for variable definition.
function smc:write_var_def(v)
   local var, typ = self:var_and_type(v)
   self:write(typ .. var)
   self:write(" = ")
end
function smc:write_binding(v, c_expr)
   self:write(self:tab())
   if v then
      self:write_var_def(v)
      self:mark_bound(v)
   end
   self:write(c_expr)
   self:write(";\n");
end

-- Map Scheme atom (const or variable) to its C representation.
function smc:atom_to_c_expr(atom)
   if type(atom) == 'string' then
      local n = self:ref(atom)
      if n then
         return self:var(n)
      else
         -- Keep track of free variables.
         self.free[atom] = true
         return self.config.state_name .. "->" .. atom .. "/*free*/"
      end
   else
      -- number or other const
      return atom
   end
end

-- Type checking shorthand
local function tc(typ,val)
   assert(type(val) == typ)
   return val
end

-- Generate symbols for let-insertion.  These use a prefix that is not
-- legal in the code so they never clash with source variables.
function smc:gensym(prefix)
   -- Generated symbols should not clash with any program text, which
   -- is why we use the comment character here.
   if not prefix then prefix = ";" end;
   local n = self.sym_n
   self.sym_n = n + 1
   return prefix .. n
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
         local binding = l(sym, subexpr)
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
      self.labels[fun_name] = true
   else
      -- log("no_tail: fun_name=" .. fun_name .. "\n")
      -- Non-tail calls are inlined as we do not support the call
      -- stack necessary for "real" calls.
      self:save_context(
         {'env','stack_ptr','depth'},
         function()
            self.depth = self.depth + 1
            assert(self.depth < 10) -- FIXME: ad-hoc infinite loop guard

            -- Inlining links the environment inside the function body
            -- to cells accessible through the callsite environment.
            local callsite_env = self.env
            self.env = nil
            local dbg = {fun_name}
            for i=1, #app_form-1 do
               local var_name = tc('string',app_form[i+1])
               local arg_name = tc('string',fun_def.args[i])
               local callsite_var = tc('table',self:ref(var_name, callsite_env))
               self:push_alias(arg_name, callsite_var)
               table.insert(dbg, arg_name .. "=" .. var_name)
            end
            -- The body can then be compiled in this local environment.
            self:write(self:tab() .. "/*inline:" .. table.concat(dbg,",") .. "*/\n")
            self:compile(fun_def.body, hole, false)
         end)
   end

end

local function ifte(c,t,f)
   if c then return t else return f end
end

-- Compilation dispatch based on expr.  If hole is non-nil, it refers
-- to the variable that takes the value of the expression.  If
-- tail_position is true, this expression resides in the tail position
-- of a function definition ( where goto statements are valid. )
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
         if not self.config.first_pass_prefix then
            self.write = function() end
         else
            self.mod_prefix = self.config.first_pass_prefix
         end

         self:reset()
         self:write("\n// first pass\n")
         -- FIXME: First pass does not generate valid C at this point.
         self:write("#if 0\n")
         self:compile(expr)
         self:write("// stack_size: " .. self.stack_size .. "\n")
         self:write("#endif\n")
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
         self:write("\n// second pass\n")
         self:compile(expr)
         self:write("// stack_size: " .. self.stack_size .. "\n")
         self:write("\n")
      end)

   -- Generate the struct definition, then append the C code.
   self:write("struct " .. self.config.state_struct .. " {\n")
   self:write(self:tab() .. "struct csp_task task; // ends in evt[]\n");
   self:write(self:tab() .. "struct csp_evt evt[" .. self.nb_evt .. "]; // nb events used\n");
   self:write(self:tab() .. "void *next;\n")

   self:write(self:tab() .. "T e[" .. self.stack_size .. "];\n")
   for v in pairs(self.free) do
      self:write(self:tab() .. "T " .. v .. ";\n")
   end
   self:write("};\n")

   self:write(table.concat(c_code,""))
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
   self.nb_evt = 0;
   assert(0 == se.length(self.env))
   assert(0 == self.stack_ptr)
   assert(1 == self.indent)
end


function smc.new()
   local config = { state_name = "s", state_struct = "state" }
   local obj = { stack_ptr = 0, env = nil, indent = 1, config = config }
   setmetatable(obj, {__index = smc})
   return obj
end

return smc
