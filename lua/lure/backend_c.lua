-- C code pretty-printer

-- Currently this only does goto version, and a wrapper is needed.
-- Later, do this:
--
-- The input block IR is interpreted as follows:
-- . The outer labels form is converted to named C functions
-- . Inside that, all labels are converted to goto
--
-- This allows representation of both use cases (plain C, state
-- machine C) in one backend, as it is probably going to be useful to
-- mix.
--

local se        = require('lure.se')
local se_match  = require('lure.se_match')
local iolist    = require('lure.iolist')
local lure_comp = require('lure.comp')
local backend   = require('lure.backend')
local scheme_blockval = require('lure.scheme_blockval')
local scheme_frontend = require('lure.scheme_frontend')
local scheme_pretty   = require('lure.scheme_pretty')
local scheme_sm       = require('lure.scheme_sm')
local l = se.list
local ins = table.insert
local pprint = scheme_pretty.new()


local function _trace(tag, expr)
   log_se_n(expr, tag .. ": ")
end

local class = {}

-- Cherry-pick some methods (micro-mixin?)
class.parameterize = lure_comp.parameterize
class.indented     = lure_comp.indented
class.tab          = lure_comp.tab


local lib = require('lure.slc_runtime')

local function mangle(var)
   assert(var and var.unique)
   local name = var.var
   if not name then return var.unique end

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
   return {var.unique,"_",name}
end

local function iol_cons(a,d)
   return {"{",a,",",d,"}"}
end

local function iol_atom(a)
   assert(a ~= nil)
   if type(a) == 'table' and a.class == 'var' then
      return mangle(a)
   elseif type(a) == 'number' then
      return a
   elseif type(a) == 'string' then
      return {"'",a,"'"} -- FIXME: string quoting
   elseif a == scheme_frontend.void then
      return {"0","/*void*/"}
   elseif type(a) == 'table' then
      -- All tables are abstract types wrapped in the style of se.lua
      local et = se.expr_type(a)
      if et == 'expr' then
         return iol_atom(a.expr)
      elseif et == 'pair' then
         return iol_cons(iol_atom(a[1]), iol_atom(a[2]))
      else
         error('bad expr_type = ' .. et)
      end
   else
      log_desc({bad_atom = a})
      log_se_n(a,"BAD_ATOM: ")
      error("syntax error")
      -- return "<BADATOM>"
   end
end

function class.commalist(s,lst,typ)
   typ = typ or ""
   local iol = {}
   for el, last in se.elements(lst) do
      ins(iol, {typ, iol_atom(el)})
      if not se.is_empty(last) then
         ins(iol, ", ")
      end
   end
   return iol
end


-- Special syntax is implemented using another layer of special forms.
local form = {}
form['vector-set!'] = function(s, args)
   s.match(
      args,
      {{"(,vec ,idx ,val)", function(m)
           s:w(iol_atom(m.vec), "[",
               iol_atom(m.idx),"] = ",
               iol_atom(m.val), ";")
        end}})
end
form['vector-ref'] = function(s, args)
   s.match(
      args,
      {{"(,vec ,idx)", function(m)
           s:w(iol_atom(m.vec), "[", iol_atom(m.idx),"];")
        end}})
end

-- Lua infix functions
local infix = {
   ['+']  = '+',
   ['-']  = '-',
   ['*']  = '*',
   ['/']  = '/',
   ['>']  = '>',
   ['<']  = '<',
}
for scm,op in pairs(infix) do
   form[scm] = function(s, args)
      local a, b = se.unpack(args, {n=2})
      s:w(iol_atom(a)," ",op," ",iol_atom(b),";")
   end
end


-- Single expression forms inside if, lambda etc are converted to
-- block with a single return binding.
function class.w_expr(s, expr, indent)
   s.match(
      expr,
      {
         {"(block . ,bindings)", function(m)
             s:w_bindings(m.bindings, indent)
         end},
         {",body", function(m)
             s:w_bindings(l(l('_', m.body)), indent)
         end}
   })
end

function class.w_bindings(s, bindings, indent)
   indent = indent or 1
   s:parameterize(
      {indent = s.indent + indent},
      function() s:w_bindings_inner(bindings) end)
end
function class.w_bindings_inner(s, bindings)
   local function maybe_assign(var)
      if var ~= '_' then
         s:w("T ", iol_atom(var), " = ")
      end
   end

   local function w_prim_eval(expr)
      s.match(
         expr,
         {
            {"(app ,fun . ,args)",
             function(m)
                -- _trace("APP_FUN", expr)

                -- Note that in C, we require all function
                -- applications to be top level functions, represented
                -- by the 'prim' data type.  Any flattening needs to
                -- happen in a previous pass.
                if m.fun.class == 'prim' then
                   -- produced by scheme_frontend -> scheme_sm
                   name = m.fun.name
                elseif m.fun.class == 'var' then
                   local prim = s.prim[m.fun]
                   if prim then
                      name = prim.name
                   else
                      -- FIXME: Use mangled name
                      name = iol_atom(m.fun)
                   end
                else
                   error("unknown function class '" .. m.fun.class .. "'")
                end
                local w_f = form[name]
                if w_f then
                   w_f(s, m.args)
                else
                   s:w(name,"(",s:commalist(m.args),");")
                end
            end},
            {"(,form . ,args)", function(m)
                _trace("EXPR", binding)
                error("bad expression form")
            end},
            {",atom", function(m)
                s:w(iol_atom(m.atom))
                s:w(";")
            end}
         })
   end

   for binding, rest in se.elements(bindings) do
      local last = se.is_empty(rest)
      s.match(
         binding,
         {
            -- These are only allowed in the top level bindings form,
            -- or in a labels form.
            {"(,var (lambda ,args ,expr))", function(b)
                error('lambda_not_allowed')
            end},
            {"(_ (block . ,bindings))", function(m)
                s:w(s:tab(),"{\n")
                s:w_bindings(m.bindings)
                s:w(s:tab(),"}")
            end},
            -- Nested labels are supported, but are always implemented
            -- using goto.  IR needs to produce lambda expressions
            -- without arguments.  I.e. argument passing is already
            -- solved by other means.
            {"(_ (labels ,bindings ,entry))", function(m)
                s:w("/* labels: entry */\n",s:tab(),"{\n",s:tab(1))
                s:w_expr(m.entry, 1)
                s:w("\n",s:tab(), "}\n", s:tab())
                s:w("/* labels: clauses */\n",s:tab())
                for binding, rest in se.elements(m.bindings) do
                   s.match(
                      binding,
                      {{"(,name (lambda () ,expr))", function(b)
                           s:w(iol_atom(b.name),": {\n",s:tab(1))
                           s:w_expr(b.expr,1)
                           s:w("\n",s:tab(),"}")
                           if not se.is_empty(rest) then
                              s:w("\n",s:tab())
                           end
                   end}})
                end
            end},
            {"(_ (if ,cond ,etrue, efalse))", function(m)
                s:w("if (", iol_atom(m.cond), ") {\n", s:tab(1))
                s:w_expr(m.etrue, 1)
                s:w("\n", s:tab(), "} else {\n", s:tab(1))
                s:w_expr(m.efalse, 1)
                s:w("\n",s:tab(),"}")
            end},
            {"(_ (goto ,label))", function(m)
                s:w("goto ", iol_atom(m.label), ";")
            end},
            {"(_ (hint ,tag . ,args))", function(m)
            end},
            {"(_ (return ,expr))", function(m)
                s:w("return ")
                w_prim_eval(m.expr)
            end},
            {"(_ (set! ,var ,expr))", function(m)
                s:w(iol_atom(m.var), " = ")
                w_prim_eval(m.expr)
            end},
            {"(,var (app ,fun . ,args))", function(m)
                if (m.fun.name == 'make-vector') then
                   assert(m.var ~= '_')
                   assert(se.length(m.args) == 1)
                   s:w("T ", iol_atom(m.var), "[", iol_atom(se.car(m.args)), "] = {};")
                else
                   maybe_assign(m.var)
                   w_prim_eval({'app',{m.fun,m.args}})
                end
            end},
            {"(,var ,prim_eval)", function(m)
                maybe_assign(m.var)
                w_prim_eval(m.prim_eval)
            end}
      })
      if not last then
         s:w("\n", s:tab())
      end
   end
end

function class.w(s, ...)
   local out = s.out
   assert(out)
   ins(out, {...})
end

-- Currrently the compiler output only generates the inside of a
-- single function.  This can be used to wrap that code for testing.
function class.wrap_main(mod_body)
   return {
      "T module(void) {\n",
      mod_body,
      "}\n",
      "int main(int argc, char **argv) { return module(); }\n"
   }
end

-- Top level entry point
function class.compile(s,top_expr)

   -- Do blockval pass after rewriting toplevel expression.
   local c_blockval = scheme_blockval.new()

   local mod_body = {}
   local main = { class='var', unique='main' }

   function top_fun(name, args, expr1)
      local l_expr = c_blockval:compile(l('lambda',args,expr1))
      local _, _, expr = se.unpack(l_expr, {n=3})
      s:w("T ", iol_atom(name), "(", s:commalist(args, "T "),") {","\n",s:tab(1))
      s:w_expr(expr)
      s:w("\n",s:tab(),"}")
      if not se.is_empty(rest) then
         s:w("\n",s:tab())
      end
   end
   function compile_labels(bindings, inner)
      for binding in se.elements(bindings) do
         s.match(
            binding,
            {{"(,name (lambda ,args ,expr))", function(m)
                 top_fun(m.name, m.args, m.expr)
             end}})
      end
      top_fun(main, l(), inner)
   end

   s:parameterize(
      {out = mod_body},
      function()
         s.indent = 0
         s.prim = {}
         s:w(s:tab())
         s.match(
            top_expr,
            {
               {"(block . ,_)", function(m)
                   -- scheme_sm output: compile as state machine
                   -- without function wrapper (old style)
                   s.indent = 1
                   s:w_expr(top_expr, 0)
                   s:w("\n")
               end},
               {"(lambda . ,_)", function(m)
                   -- scheme_frontend output: compile as set of top
                   -- level functions.
                   backend.match_module_form(
                      s, top_expr,
                      function(var, sym)
                         -- Same behavor as scheme_sm
                         s.prim[var] = scheme_sm.make_prim(sym.expr)
                      end,
                      compile_labels)
               end},
            })
      end)

   if s.config.debug_lua_output then
      iolist.write_to_file(s.config.debug_lua_output, mod)
   end
   return { class = "iolist", iolist = mod_body }
end


-- FIXME: Also do 'quote' or 'lit'

-- Undo the form tags.  Those have been added to make matching easier.
-- The values themselves are also tagged via the 'class' attribute,
-- which is what we use for dispatch.
--function class.unref(s,e)
--   local a = function(m) return m.a end
--   return s.match({{"(ref ,a)", a}, {",a", a}})
--end
function class.map(s,method,lst)
   return se.map(function(e) return s[method](s,e) end, lst)
end




local function new(config)
   local obj = { match = se_match.new(), config = config or {} }
   setmetatable(obj, { __index = class })
   return obj
end
class.new = new
return class

