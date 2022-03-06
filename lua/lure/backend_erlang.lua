-- FIXME: Early WIP.  ISSUES
-- . Erlang doesn't have assignment
-- . The IF construct is special

-- Lua code pretty-printer from lambda, block, if, set!
-- Based on se_match

local se        = require('lure.se')
local se_match  = require('lure.se_match')
local iolist    = require('lure.iolist')
local lure_comp = require('lure.comp')
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

-- FIXME: There's a double :tab() somewhere.

local lib = require('lure.slc_runtime')

local function _trace(tag,expr)
   log_se_n(expr, tag .. ":")
end


local function var_prefix(name)
   return {'V',name}
end

local function mangle(var)
   assert(var and var.unique)
   local name = var.var
   name = nil -- FIXME: temporary, to make output form easier to read
   if not name then return var_prefix(var.unique) end

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
   return var_prefix({var.unique,"_",name})
end

-- FIXME: Go over tree once to collect the reverse mappings:
--  (r22:r21 (r34:base-ref 'cons))
-- FIXME: Create a library with generic iterators.
-- FIXME: Actually it's simpler: compile the quotation into cons in the frontend.
local function iol_cons(a,d)
   return {"{",a,",",d,"}"}
end

local function iol_atom(a)
   if type(a) == 'table' and a.class == 'var' then
      return mangle(a)
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
         return iol_atom(a.expr)
      elseif et == 'pair' then
         return iol_cons(iol_atom(a[1]), iol_atom(a[2]))
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
      ins(iol, iol_atom(el))
      if not se.is_empty(last) then
         ins(iol, ", ")
      end
   end
   return iol
end


-- Special syntax is implemented using another layer of special forms.
local form = {}
form['table-set!'] = function(s, args)
   s.match(
      args,
      {{"(,tab ,key, ,val)", function(m)
           s:w(iol_atom(m.tab), "[", iol_atom(m.key),"] = ",iol_atom(m.val))
        end}})
end
form['table-ref'] = function(s, args)
   s.match(
      args,
      {{"(,tab ,key)", function(m)
           s:w(iol_atom(m.tab), "[", iol_atom(m.key),"]")
        end}})
end


-- Lua infix functions
local infix = {
   ['+']  = '+',
   ['-']  = '-',
   ['*']  = '/',
   ['/']  = '/',
   ['>']  = '>',
   ['<']  = '<',
}
for scm,op in pairs(infix) do
   form[scm] = function(s, args)
      local a, b = se.unpack(args, {n=2})
      s:w(iol_atom(a)," ",op," ",iol_atom(b))
   end
end


function class.w_maybe_assign(s, var)
   if var ~= '_' then
      s:w(iol_atom(var), " = ")
   end
end


function class.w_bindings(s, bindings)
   -- _trace("BINDINGS", bindings)
   for binding in se.elements(bindings) do
      _trace("BINDING", binding)

      s:w(s:tab())
      s.match(
         binding,
         {
            -- Special case the function definitions
            {"(,var (lambda ,args ,expr))", function(m)
                _trace("LAMBDA",binding)
                s:indented(
                   function()
                      s:w_maybe_assign(m.var)
                      s:w("fun(", s:commalist(m.args),")"," ->\n")
                      s:comp(m.expr)
                      s:w(s:tab(),".")
                   end)
            end},
            {"(,var (if ,cond ,etrue, efalse))", function(m)  -- FIXME var
                _trace("IF",binding)
                s:w_maybe_assign(m.var)
                s:w("case ", iol_atom(m.cond), " of true -> ")
                s:indented(
                   function()
                      s:comp(m.etrue)
                      s:w(s:tab(), "; false -> ")
                      s:comp(m.efalse)
                      s:w(s:tab(), "end")
                   end,
                      -1)
            end},
            -- FIXME: Erlang doesn't have mutation.  This is going to be an issue.
            {"(,rvar (set! ,var ,expr))", function(m) 
                -- assert(m.rvar == '_') -- FIXME: why does this give a var in the example?
                _trace("SET",binding)
                s:w_maybe_assign(m.var)
                s:i_comp(m.expr)
            end},
            {"(,var (app ,fun . ,args))", function(m)
                _trace("APP",binding)
                s:w_maybe_assign(m.var)
                local w_f = m.fun.var and form[m.fun.var]
                if w_f then
                   w_f(s, m.args)
                else
                   s:w(iol_atom(m.fun),"(",s:commalist(m.args),")")
                end
                s:w(",")
            end},
            {"(,var (hint ,tag . ,args))", function(m)
                _trace("HINT",binding)
                -- Ignore hints
            end},
            {"(,var (,form . ,args))", function(m)
                _trace("BAD",binding)
                error('bad_binding_form')
            end},
            {"(,var ,atom)", function(m)
                assert(se.expr_type(m.atom) ~= 'expr')
                s:w_maybe_assign(m.var)
                s:w(iol_atom(m.atom))
            end},
            {",other", function(m)
                _trace("MISMATCH", m.other)
                error('mismatch')
            end}
         }
      )
      s:w("\n")
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

-- Top level entry point
function class.compile(s,expr)

   -- The IR expr is a single lambda expression, parameterized by
   -- 'lib_ref', a function that performs library symbol lookup for
   -- all free variables that were found by scheme_frontend.
   local mod_body = {}
   s:parameterize(
      {out = mod_body},
      function()
         s:comp(expr)
         s:w("\n")
      end)
   local mod = {
      "mod_body() -> ",mod_body,"\n",

      -- Emit code that binds this function to the library.  The
      -- library 'module-register!' will record definitions in the
      -- 'mod' table.
      "local mod = {}\n",
      "local lib_ref = require('lure.slc_runtime').new(mod)\n",
      "mod_body(lib_ref)\n",
      "return mod\n"
   }
   if s.config.debug_lua_output then
      iolist.write_to_file(s.config.debug_lua_output, mod)
   end
   return { class = "iolist", iolist = mod }
end

function class.i_comp(s, expr)
   s:indented(function() s:comp(expr) end)
end

function class.w_atom(s, a)
   s:w(iol_atom(a))
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
   local obj = { match = se_match.new(), indent = 0, config = config or {} }
   setmetatable(obj, { __index = class })
   return obj
end
class.new = new
return class

