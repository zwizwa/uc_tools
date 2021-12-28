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

-- Introduce a variable in the lexical scope associated to a new
-- storage cell.
function smc:push_new_var(var_name)
   local cell = self:new_cell()
   local v = {var = var_name, cell = cell}
   -- self.env is the currently visible environment, which gets popped on exit.
   -- FIXME: How to not put unbound variables here? Maybe not an issue..
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

   self:save(
      {'env'},
      function()
         local v = self:push_new_var(var_name)
         self:write(self:tab())
         self:write("for(")
         self:write_var_def(v)
         self:write("0 ; ")
         self:mark_bound(v)
         local cv = self:atom_to_c_expr(var_name)
         self:write(cv .. " < " .. iter_arg .. " ; ")
         self:write(cv .. " = " .. cv .. " + 1) {\n")
         self.indent = self.indent + 1
         for form in se.elements(inner) do
            assert(form)
            self:compile(form, nil)
         end
         self.indent = self.indent - 1

         self:write(self:tab() .. "}\n")
      end)
end

-- TODO
-- form['if'] = function(self, if_expr, hole)
--    assert(not hole)
--    local _, setup, inner = se.unpack(if_expr, { n = 2, tail = true })

--    self:write(self:tab() .. "for(;;){\n")
--    self.indent = self.indent + 1

--    for form in se.elements(inner) do
--       assert(form)
--       self:compile(form, nil)
--    end

--    self.indent = self.indent - 1
--    self:write(self:tab() .. "}\n")
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
-- Compile the form to C statement expressions.
form['let*'] = function(self, let_expr, hole, tail_position)
   local _, bindings, inner = se.unpack(let_expr, { n = 2, tail = true })

   assert(type(bindings) == 'table')

   self:write(self:tab())
   if hole then
      self:write_var_def(hole)
   end
   self:write("({\n")

   self:save(
      {'env','stack_ptr','indent'},
      function()
         self.indent = self.indent + 1

         local nb_bindings = se.length(bindings)
         for binding in se.elements(bindings) do
            local var, expr = se.unpack(binding, { n = 2 })
            assert(type(var) == 'string')
            assert(expr)
            local v = self:push_new_var(var)
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
   self:write("void " .. modname .. "("
                 .. self.config.state_type
                 .. " *" .. self.config.state_name
                 .. ") {\n");

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
         end
      end)

   self:write("}\n");
end


-- Blocking form.  The control structure itself is implemented
-- separately in a C macro, specialized to the state machine
-- scheduler.  It is not of concern here.  We only need to manage the
-- lexical varariables.
form['read'] = function(self, expr, hole)
   local _, chan = se.unpack(expr, {n = 2})
   local bound = {}
   for var in se.elements(self.env) do
      -- All visible C local variables are undefined when we jump into
      -- the body of a function.  Mark them 'lost' here.  This is used
      -- later when the variable is referenced to turn it into a
      -- 'saved' variable, so in the second pass it can be allocated
      -- in the state struct.
      if var.cell.bind == 'local' then
         table.insert(bound, n)
         var.cell.bind = 'lost'
      end
   end
   local s = self.config.state_name
   self:write_binding(
      hole,
      "SM_READ("
         .. s .. ","
         .. s .. "->" .. chan .. ")")
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
   self:write( typ .. var .. " = ")
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
function smc:gensym()
   local n = self.sym_n
   self.sym_n = n + 1
   -- FIXME: Generated symbols should not clash with any program text.
   return ";" .. n
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
      self.labels[fun_name] = true
   else
      -- log("no_tail: fun_name=" .. fun_name .. "\n")
      -- Non-tail calls are inlined as we do not support the call
      -- stack necessary for "real" calls.
      self:save(
         {'env','stack_ptr','depth'},
         function()
            self.depth = self.depth + 1
            assert(self.depth < 10) -- FIXME: ad-hoc infinite loop guard

            -- Inlining links the environment inside the function body
            -- to cells accessible through the callsite environment.
            local callsite_env = self.env
            self.env = {}
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


-- Don't bother with building representations.  The two passes emit C
-- code directly, as the shape of the code resembles the Scheme code
-- fairly directly.  The second pass can use the information gathered
-- in the first pass to allocate variables in the state struct, or on
-- the C stack, and to omit unused function definitions.
function smc:compile_passes(expr)
   local w = self.write
   -- Override to suppress printing of first pass C output, which is
   -- only neccessary for debugging variable allocation.
   -- self.write = function() end

   self:reset()
   self:write("\n// first pass\n")
   self:write("#if 0\n")
   self:compile(expr)
   self:write("// stack_size: " .. self.stack_size .. "\n")
   self:write("#endif\n")

   self.write = w

   -- Second pass uses some information from the previous pass: the
   -- information gathered about each storage cell, and information
   -- about the goto labels.
   self.cells_last  = self.cells
   self.labels_last = self.labels

   self:reset()
   self:write("\n// second pass\n")
   self:compile(expr)
   self:write("// stack_size: " .. self.stack_size .. "\n")
   self:write("\n")
end

-- Reset compiler state before executing a new pass.
function smc:reset()
   self.cells = {}
   self.labels = {}
   self.stack_size = 0
   self.sym_n = 0
   self.funs = {}
   self.depth = 0
   assert(0 == se.length(self.env))
   assert(0 == self.stack_ptr)
   assert(1 == self.indent)
end


function smc.new()
   local config = { state_name = "s", state_type = "state_t" }
   local obj = { stack_ptr = 0, env = {}, indent = 1, config = config }
   setmetatable(obj, {__index = smc})
   return obj
end

return smc
