-- Erlang backend.
-- Restrictions:
-- . Top level form needs to be a labels (letrec) form, mapping to modules.
-- . The inner expression maps to the 'main' function, e.g. for scripts.

local se        = require('lure.se')
local se_match  = require('lure.se_match')
local iolist    = require('lure.iolist')
local lure_comp = require('lure.comp')
local backend   = require('lure.backend')
local scheme_frontend = require('lure.scheme_frontend')
local scheme_pretty   = require('lure.scheme_pretty')
local l = se.list
local ins = table.insert
local pprint = scheme_pretty.new()

local class = {}

-- Cherry-pick some methods (micro-mixin?)
class.parameterize = lure_comp.parameterize
class.indented     = lure_comp.indented
class.tab          = lure_comp.tab


-- Convention for newline/tab:

-- Expressions are always compiled in binding position, are already at
-- indented position and should not print newline.

local function _trace(tag,expr)
   log_se_n(expr, tag .. ":")
end
local function trace(tag,expr)
   -- _trace(tag,expr)
end

function capitalize(iol)
   local str = iolist.to_string(iol)
   local byte = str:byte(1) - 32 -- FIXME: sloppy
   return {string.char(byte), str:sub(2)}
end

function class.mangle(s, var)
   local prefix = capitalize
   if s.mod_fun[var] then prefix = function(x) return x end end

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

function class.commalist(s,lst)
   local iol = {}
   for el, last in se.elements(lst) do
      ins(iol, s:iol_atom(el))
      if not se.is_empty(last) then
         ins(iol, ", ")
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
   if var ~= '_' then
      s:w(s:iol_atom(var), " = ")
   end
end


function class.w_bindings(s, bindings)
   -- trace("BINDINGS", bindings)
   for binding, rest in se.elements(bindings) do
      trace("BINDING", binding)

      s.match(
         binding,
         {
            {"(,var (lambda ,args ,expr))", function(m)
                trace("LAMBDA",binding)
                s:w_maybe_assign(m.var)
                s:w("fun")
                s:w_lambda(m.args, m.expr)
                s:w("\n",s:tab(),"end")
            end},
            {"(,var (if ,cond ,etrue, efalse))", function(m)  -- FIXME var
                trace("IF",binding)
                s:w_maybe_assign(m.var)
                s:indented(
                   function()
                      s:w("case ", s:iol_atom(m.cond), " of\n")
                      s:w(s:tab(-1), "true ->\n", s:tab())
                      s:comp(m.etrue)
                      s:w(";\n", s:tab(-1), "false ->\n",s:tab())
                      s:comp(m.efalse)
                      s:w("\n",s:tab(-2), "end")
                   end,
                   2)
            end},
            {"(,rvar (set! ,var ,expr))", function(m)
                _trace("SET",binding)
                error("assingnment_not_supported")
            end},
            {"(,var (app ,fun . ,args))", function(m)
                trace("APP",binding)
                s:w_maybe_assign(m.var)
                -- FIXME: use s.lib_ref instead.
                local w_f = m.fun.var and form[m.fun.var]
                if w_f then
                   w_f(s, m.args)
                else
                   s:w(s:iol_atom(m.fun),"(",s:commalist(m.args),")")
                end
            end},
            {"(,var (labels . ,bindings))", function(m)
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
         s:w(",\n",s:tab())
      end
   end
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

-- Prefix this with name or 'fun', and postfix with '.' or 'end'.
function class.w_lambda(s,args,body)
   s:indented(
      function()
         s:w("(", s:commalist(args),")"," ->\n",s:tab())
         s:comp(body)
      end)
end

-- Top level entry point
function class.compile(s,expr)

   -- The output is a module.
   -- The input expression has a specific form:
   -- 1. Outer lambda with lib_ref
   -- 2. A block form with lib_ref dereferences
   -- 3. Inner labels form with module bindings.

   -- The inner labels fallthrough case is named 'main'.
   local main = { class='var', unique='main' }
   s.mod_fun = { [main] = true }

   s.out = {}
   s.lib_ref = {}

   function compile_labels(bindings, inner)
      assert(bindings)
      -- First pass: collect module-level function names so they can
      -- be distinguished from local variables.
      for binding in se.elements(bindings) do
         local var = se.car(binding)
         s.mod_fun[var] = true
      end
      -- Second pass: emit definitions.
      local function top_fun(b)
         local var = (b.var == '_' and main) or b.var
         s:w(s:iol_atom(var))
         s:w_lambda(b.args, b.body)
         s:w(".\n")
      end
      for binding in se.elements(bindings) do
         s.match(
            binding,
            {{"(,var (lambda ,args ,body))", top_fun}})
      end
      -- The inner expression is wrapped in a function as Erlang does
      -- not have naked expressions.
      top_fun({var = main, args=l(), body=inner})
   end

   -- Match the canonical top level module form.
   backend.match_module_form(
      s, expr,
      function(var, sym) s.lib_ref[var] = sym end,
      compile_labels)

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

