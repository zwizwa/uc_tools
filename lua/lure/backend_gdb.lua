-- Compile to gdb command sequence.
--
-- Note that there are some holes in the implementation.  Not all
-- Scheme programs can be represented.
--
-- The intended purpose is for running test and to explore the
-- "gradual untethering" design method, where code is written in
-- Scheme and originally runs mostly in GDB.  Then pieces are
-- translated to a subset of Scheme that can compile to C, gradually
-- building up a C program.

-- Some notes on the GDB command language and the Scheme mapping:
--
-- . There is no concept of return value, but GDB uses call-by-name,
--   so it is possible to parameterize a return value parameter:
--   https://stackoverflow.com/questions/12572631/return-a-value-via-a-gdb-user-defined-command
--
-- . Top level structure is expected to be a labels form.  Mutual
--   recursion is supported, but is not stack-safe.
--
-- . There is support for nested self-tail recursion, pattern-matched
--   from labels form.  It seems to work but there are likely corner
--   cases that are not implemented correctly.
--
-- . Functions are not re-entrant due to global variables only.  There
--   doesn't seem to be a simple way around this, besides creating a
--   large array and running an interpreter inside of it, or using
--   target memory.
--
-- . Since all variables are global and uniquely named, closures just
--   work (definition control-dominates use) with the caveat that they
--   are still not re-entrant, so things like 'map' won't work when
--   nested.  To be able to store functions in variables, they are
--   encoded as integers, and named such that they can be referenced
--   via eval "lambda%d".
--
-- . The trampoline heuristic excludes a degenerate subset of code
--   patterns where a letrec function is used for actual recursion.
--   To fix this it is likely necessary to implement backtracking.
--   Doesn't seem worth it at the moment.
--
-- . Arrays of word-size integers can be represented locally, but
--   cannot have zero size.


local se        = require('lure.se')
local se_match  = require('lure.se_match')
local iolist    = require('lure.iolist')
local lure_comp = require('lure.comp')
local backend   = require('lure.backend')
local scheme_frontend = require('lure.scheme_frontend')
local scheme_pretty   = require('lure.scheme_pretty')
local l = se.list
local l2a = se.list_to_array
local ins = table.insert
local pprint = scheme_pretty.new()

local class = {}

-- Cherry-pick some methods (micro-mixin?)
class.parameterize = lure_comp.parameterize
class.indented     = lure_comp.indented
class.tab          = lure_comp.tab

class.def          = lure_comp.def
class.ref          = lure_comp.ref
class.set          = lure_comp.set
class.find_cell    = lure_comp.find_cell

-- Convention for newline/tab:

-- Expressions are always compiled in binding position, are already at
-- indented position and should not print newline.

local function _trace(tag,expr)
   log_se_n(expr, tag .. ":")
end
local function trace(tag,expr)
   -- _trace(tag,expr)
end

class.prim = {
   ["print"] = 1
}

function id(x) return x end

function class.mangle_fun(s, var)
   return s:mangle(var, id)
end

function class.mangle(s, var, prefix)

   local ref = s:ref(var, true)
   if ref then
      assert(ref.class == 'subst')
      assert(ref.var)
      return class.mangle(s, ref.var, prefix)
   end

   prefix = prefix or function(v) return {'$',v} end

   assert(var and var.unique)
   local name = var.var
   -- name = nil -- FIXME: temporary, to make output form easier to read
   if not name then return prefix(var.unique) end

   -- Do some exact matches first
   local alias = {
      ['+'] = 'add',
      ['-'] = 'min',
   }
   if alias[name] then name = alias[name] end

   -- Then replace illegal chars
   local subst = {
      ["next"] = "nxt",
      ["-"] = "_", -- "_dash_",
      ["+"] = "_", -- "_dash_",
      ["!"] = "",  -- "_bang_",
      ["?"] = "p", -- "_pred_",
      [">"] = "_gt_",
      ["/"] = "_div_",
      ["="] = "_is_",
   }
   for from,to in pairs(subst) do
      name = string.gsub(name,from,to)
   end
   return prefix({var.unique,"_",name})
end

local function iol_cons(a,d)
   return {"{",a,",",d,"}"}
end

function class.iol_atom(s, a)
   if type(a) == 'table' and a.class == 'var' then
      return s:mangle(a)
   elseif type(a) == 'number' then
      return a
   elseif type(a) == 'string' then
      return {"'",a,"'"} -- FIXME: string quoting
   elseif a == scheme_frontend.void then
      return 'nil'
   elseif type(a) == 'table' then
      -- All tables are abstract types wrapped in the style of se.lua
      local et = se.expr_type(a)
      if et == 'expr' then
         return s:iol_atom(a.expr)
      elseif et == 'pair' then
         return iol_cons(s:iol_atom(a[1]), s:iol_atom(a[2]))
      else
         error('bad expr_type = ' .. et)
      end
   else
      log_desc({bad_atom = var})
      log_se_n(a,"BAD_ATOM: ")
      error("syntax error")
      -- return "<BADATOM>"
   end
end

function class.arglist(s,lst,sep)
   sep = sep or " "
   local iol = {}
   for el, last in se.elements(lst) do
      ins(iol, s:iol_atom(el))
      if not se.is_empty(last) then
         ins(iol, sep)
      end
   end
   return iol
end

local infix = {
   ['+']  = '+',
   ['-']  = '-',
   ['*']  = '*',
   ['/']  = '/',
   ['>']  = '>',
   ['<']  = '<',
}
local form = {}
for scm,op in pairs(infix) do
   form[scm] = function(s, args)
      local a, b = se.unpack(args, {n=2})
      s:w(s:iol_atom(a)," ",op," ",s:iol_atom(b))
   end
end
form['vector'] = function(s, args)
   s:w("{")
   s:w(s:arglist(args,", "))
   s:w("}")
end


function class.w_assign(s, var)
   s:w("set ", s:iol_atom(var), " = ")
end


function class.w_maybe_assign(s, var)
   if s.tail and s.rv then
      s:w("set ", s:iol_atom(s.rv), " = ")
   elseif var ~= '_' then
      s:w_assign(var)
   end
end

function class.pass_rv(s, var)
   return ((var == '_') and s.rv) or var
end

function class.find_trampoline(s, name)
   for binding in se.elements(s.trampoline) do
      if name == se.car(binding) then
         _trace("LOOP_VARS", binding)
         return se.cdr(binding)
      end
   end
   return nil
end

function class.w_bindings(s, bindings)
   -- trace("BINDINGS", bindings)

   function match_binding(binding, rest)
      local nop = false
      s.match(
         binding,
         {
            {"(,var (if ,cond ,etrue, efalse))", function(m)  -- FIXME var
                trace("IF",binding)
                s:parameterize(
                   { rv = s:pass_rv(m.var),
                     indent = s.indent + 1 },
                   function()
                      s:w("if ", s:iol_atom(m.cond), "\n",s:tab())
                      s:comp(m.etrue)
                      s:w("\n", s:tab(-1), "else\n",s:tab())
                      s:comp(m.efalse)
                      s:w("\n",s:tab(-1), "end")
                   end)
            end},
            {"(,rvar (set! ,var ,expr))", function(m)
                s:w_maybe_assign(m.var)
                s:comp(m.expr)
            end},
            -- Special case for recognizing while loops.  Note that
            -- this is a heuristic: once the loop function is
            -- converted to a trampoline, it can no longer be used as
            -- a regular function.  Only tail calls are allowed.
            {"(,var (labels ((,loop (lambda ,state ,expr))) (app ,loop0 . ,state_init)))",
             function(m)
                -- Guard
                return m.loop == m.loop0
             end,
             function(m)
                -- Reuse the name of the loop function as a boolean
                -- variable that is used as the argument to 'while'.
                s:w_assign(m.loop)
                s:w("1\n",s:tab())
                se.zip(
                   function(var, vexpr)
                      s:w_assign(var)
                      s:w(s:iol_atom(vexpr),"\n",s:tab())
                   end,
                   m.state, m.state_init)
                s:w("while ",s:iol_atom(m.loop),"\n",s:tab(1))
                s:parameterize(
                   { rv = s:pass_rv(m.var),
                     indent = s.indent + 1,
                     trampoline = {{m.loop, m.state}, s.trampoline} },
                   function()
                      -- Once the loop is entered, the default
                      -- behavior is to not loop next time. This makes
                      -- it so that only the (app loop ...) form needs
                      -- to be modified downstream, i.e. it sets the
                      -- loop variable to true.
                      s:w_assign(m.loop) ; s:w("0\n",s:tab())
                      s:comp(m.expr)
                   end)
                s:w("\n",s:tab(),"end")
            end},
            -- Other labels forms are only supported at top level.
            {"(,var (labels ,bindings ,inner))", function(m)
                _trace("LABELS", binding)
                error('labels_toplevel_only')
            end},
            {"(,var (app ,fun . ,args))", function(m)
                trace("APP",binding)
                local loop_vars = s:find_trampoline(m.fun)
                if (loop_vars) then
                   -- Recursive calls that are converted to
                   -- trampolines can only be used in tail position.
                   se.zip(
                      function(var, vexpr)
                         s:w_assign(var)
                         s:w(s:iol_atom(vexpr),"\n",s:tab())
                      end,
                      loop_vars, m.args)
                   s:w_assign(m.fun)
                   s:w("1")
                --elseif m.fun == s.lib_ref then
                --   s:def(m.var, se.car(m.args))
                else
                   local fun = s:ref(m.fun, true)
                   local fun_name = (fun and fun.expr) or s:mangle_fun(m.fun)
                   -- FIXME: use fun instead.
                   local w_f = m.fun.var and form[m.fun.var]
                   if w_f then
                      -- Result of primitive can be assigned
                      s:w_maybe_assign(m.var)
                      w_f(s, m.args)
                   else
                      if s.prim[fun_name] then
                         -- Primitives do not take a return value argument.
                         k_arg = l({class='var',unique='ign'})
                         s:w(fun_name," ",s:arglist(m.args))
                      else
                         -- Return variable is the last argument.
                         local k_arg = l({class='var',unique='ign'})
                         if s.tail then
                            if s.rv then k_arg = l(s.rv) end
                         elseif m.var ~= '_' then
                            k_arg = l(m.var)
                         end
                         local args = se.append(m.args, k_arg)
                         if s.fun[m.fun] then
                            -- Toplevel function referred by original name.
                            s:w(fun_name," ",s:arglist(args))
                         else
                            -- Anonymous function represented by integer.
                            s:w('eval "lambda%d ',s:arglist(args),'", ', s:iol_atom(m.fun))
                         end
                      end
                   end
                end
            end},
            {"(,var (lambda ,args ,expr))", function(m)
                -- All variables are global and unique, and since
                -- definition always control-dominates use, closures
                -- will just work as top level functions. However, we
                -- can't store references to functions in variables,
                -- so calls are done indirectly with each function
                -- assigned an index.
                local n = se.length(s.lambda)
                local var = {class='var', unique='lambda'..n}
                s.lambda = {l(var, se.cadr(binding)), s.lambda}
                s:w_maybe_assign(m.var)
                s:w(s:iol_atom(n))
            end},
            {"(,var (hint ,tag . ,args))", function(m)
                trace("HINT",binding)
                -- Ignore hints
            end},
            {"(,var (,form . ,args))", function(m)
                _trace("BAD",binding)
                error('bad_binding_form')
            end},
            {"(,var ,atom)", function(m)
                assert(se.expr_type(m.atom) ~= 'expr')
                s:w_maybe_assign(m.var)
                s:w(s:iol_atom(m.atom))
            end},
            {",other", function(m)
                trace("MISMATCH", m.other)
                error('mismatch')
            end}
         }
      )
      if not se.is_empty(rest) and not nop then
         s:w("\n",s:tab())
      end
   end

   s:parameterize(
      { tail = s.tail },
      function()
         for binding, rest in se.elements(bindings) do
            trace("BINDING", binding)
            s.tail = se.is_empty(rest)
            match_binding(binding, rest)
         end
      end)

end


-- Bounce it back to w_bindings.
function class.comp(s,expr)
   s.match(
      expr,
      {
         {"(block . ,bindings)", function(m)
             s:w_bindings(m.bindings)
         end},
         {",other", function(m)
             s:w_bindings(l(l('_',m.other)))
         end}
      })
end


function class.w_indented_bindings(s, bindings)
   s:indented(function() s:w_bindings(bindings) end)
end


function class.w(s, ...)
   local out = s.out
   assert(out)
   ins(out, {...})
end

local function var(unique)
   return { class = 'var', unique = unique }
end
local function arg(i)
   return var("arg" .. i)
end


-- Prefix this with name or 'fun', and postfix with '.' or 'end'.
function class.w_lambda(s,args,body)
   s:indented(
      function()
         local args_a = l2a(args)
         for i, a in ipairs(args_a) do
            s:def(a, { class = 'subst', var = arg(i-1) })
         end
         s:w("\n",s:tab())
         s:parameterize(
            { rv = arg(#args_a) },
            function()
               s:comp(body)
            end)
      end)
end

-- Top level entry point
function class.compile(s,expr)
   s.env = se.empty
   s.rv = var("rv")
   s.out = {}
   s.trampoline = se.empty
   s.lambda = se.empty
   s.fun = {}
   s.lib_ref = {}

   function comp_lambda_binding(binding)
      s.match(
         binding,
         {
            {"(,var (lambda ,args ,expr))", function(m)
                trace("LAMBDA",binding)
                s:w("define ", s:mangle_fun(m.var, id))
                s:w_lambda(m.args, m.expr)
                s:w("\n",s:tab(),"end\n")
            end},
      })
   end

   function compile_labels(bindings, main)
      -- Mark toplevel fuctions.
      for binding in se.elements(bindings) do
         local var, _ = se.unpack(binding, {n=2})
         s.fun[var] = true
      end

      -- Compile toplevel functions.  This will collect anonymous
      -- functions in s.lambda list.
      for binding in se.elements(bindings) do
         comp_lambda_binding(binding)
      end

      -- Compile all anonymous fuctions as top level functions.
      for binding in se.elements(s.lambda) do
         comp_lambda_binding(binding)
      end

      -- Compile the in-line code.
      s:comp(main)
   end

   backend.match_module_form(
      s, expr,
      function(var, sym) s:def(var, sym) end,
      compile_labels)

   local mod = { s.out }
   return { class = "iolist", iolist = {mod,"\n"} }

end

function class.i_comp(s, expr)
   s:indented(function() s:comp(expr) end)
end

function class.w_atom(s, a)
   s:w(s:iol_atom(a))
end

function class.map(s,method,lst)
   return se.map(function(e) return s[method](s,e) end, lst)
end

local function new(config)
   local obj = { match = se_match.new(), indent = 0, config = config or {} }
   setmetatable(obj, { __index = class })
   return obj
end
class.new = new
return class

