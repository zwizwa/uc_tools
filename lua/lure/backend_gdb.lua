-- Compile to gdb command sequence.
--
-- FIXME: Just a stub.  Not sure if this is actually useful.  So why?
-- I need a little nudge, a little excursion to see if I can use plain
-- GDB to be a better uc_tools SMOS host.  And I need some exercise in
-- compiling to weird substrates.

-- Note the the GDB language is quite limited, so focus is on inlining
-- + mapping single-clause letrec to while loops.
--
-- Top level structure is a labels form.  Beyond that all other
-- functions are inlined.

-- Some notes on the GDB command language:

-- . There is no concept of return value, but GDB uses call-by-name,
--   so it is possible to parameterize a return value parameter:
--   https://stackoverflow.com/questions/12572631/return-a-value-via-a-gdb-user-defined-command

local se        = require('lure.se')
local se_match  = require('lure.se_match')
local iolist    = require('lure.iolist')
local lure_comp = require('lure.comp')
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

function class.arglist(s,lst)
   local iol = {}
   for el, last in se.elements(lst) do
      ins(iol, s:iol_atom(el))
      if not se.is_empty(last) then
         ins(iol, " ")
      end
   end
   return iol
end

local infix = {
   ['+']  = '+',
   ['-']  = '-',
   ['*']  = '/',
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


function class.w_maybe_assign(s, var)
   if s.tail and s.rv then
      s:w("set ", s:iol_atom(s.rv), " = ")
   elseif var ~= '_' then
      s:w("set ", s:iol_atom(var), " = ")
   end
end


function class.w_bindings(s, bindings)
   -- trace("BINDINGS", bindings)

   function match_binding(binding, rest)
      s.match(
         binding,
         {
            {"(,var (if ,cond ,etrue, efalse))", function(m)  -- FIXME var
                trace("IF",binding)
                local rv = ((m.var == '_') and s.rv) or m.var
                s:parameterize(
                   {rv = rv, indent = s.indent + 1},
                   function()
                      s:w("if ", s:iol_atom(m.cond), "\n",s:tab())
                      s:comp(m.etrue)
                      s:w("\n", s:tab(-1), "else\n",s:tab())
                      s:comp(m.efalse)
                      s:w("\n",s:tab(-1), "end")
                   end,
                   2)
            end},
            {"(,rvar (set! ,var ,expr))", function(m)
                s:maybe_assign(m.var)
                s:comp(m.expr)
            end},
            {"(,var (app ,fun . ,args))", function(m)
                trace("APP",binding)
                if m.fun == s.lib_ref then
                   s:def(m.var, se.car(m.args))
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
                      -- Return variable is the last argument
                      local maybe_k_arg = (s.rv and (l(s.rv))) or l()
                      s:w(fun_name," ",s:arglist(se.append(m.args,maybe_k_arg)))
                   end
                end
            end},
            {"(,var (lambda ,args ,expr))", function(m)
                error('lambda_toplevel_only')
            end},
            {"(,var (labels ,bindings ,inner))", function(m)
                error('labels_toplevel_only')
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
      if not se.is_empty(rest) then
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
   s.match(
      expr,
      {{"(lambda (,lib_ref) (block . ,bindings))", function(m)
           s.lib_ref = m.lib_ref
           -- Last expression contains the labels form.  The rest are
           -- assumed to be top level lib_ref bindings.
           local rbindings = se.reverse(m.bindings)
           local lib_refs = se.reverse(se.cdr(rbindings))
           local labels = se.car(rbindings)
           s:w_bindings(lib_refs)
           s.match(
              labels,
              {{"(_ (labels ,bindings ,main))))", function(m)
                   for binding in se.elements(m.bindings) do
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
                   s:comp(m.main)
              end}})
      end}})
   local mod = { s.out }
   return { class = "iolist", iolist = mod }

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

