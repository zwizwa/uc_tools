local se        = require('lure.se')
local se_match  = require('lure.se_match')
local iolist    = require('lure.iolist')
local lure_comp = require('lure.comp')
local scheme_frontend = require('lure.scheme_frontend')
local scheme_pretty   = require('lure.scheme_pretty')
local l = se.list
local ins = table.insert
local pprint = scheme_pretty.new()

local function _trace(tag,expr)
   log_se_n(expr, tag .. ":")
end
local function trace(tag,expr)
   -- _trace(tag,expr)
end

local class = {}


-- Cherry-pick some methods (micro-mixin?)
class.parameterize = lure_comp.parameterize
class.indented     = lure_comp.indented
class.tab          = lure_comp.tab


-- Convention for newline/tab:

-- Expressions are always compiled in binding position, are already at
-- indented position and should not print newline.

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


-- infix functions
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




function class.w_bindings(s, bindings)
   for binding in se.elements(bindings) do
      s:w(s:tab())
      s.match(
         binding,
         {
            -- Statements
            {"(_ ,expr)", function(b)
                if se.expr_type(b.expr) ~= 'pair' then
                   s:w("-- ")
                end
                s:i_comp(b.expr)
            end},
            -- Special case the function definitions
            {"(,var (lambda ,args ,expr))", function(b)
                s:indented(
                   function()
                      s:w("local function ", iol_atom(b.var), "(", s:commalist(b.args),")","\n")
                      s:w_body(b.expr)
                      s:w(s:tab(),"end")
                   end)
            end},
            -- Other variable definitions
            {"(,var ,expr)", function(b)
                -- Lua does not allow naked values to appear in a
                -- statement position.  Replace them with a comment.
                s:w("local ", iol_atom(b.var), " = ")
                s:i_comp(b.expr)
                -- FIXME: print orig var name in comment
            end},
      })
      s:w("\n")
   end
end

function class.w_indented_bindings(s, bindings)
   s:indented(function() s:w_bindings(bindings) end)
end

function class.w_body(s, expr)
   s.match(
      expr,
      -- The do .. end block can be omitted in a function body.
      {{"(block . ,bindings)", function(m)
           s:w_indented_bindings(m.bindings)
       end},
       {",body", function(m)
           s:i_comp(m.body)
      end}})
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
   local mod = { mod_body }
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

function class.w_maybe_assign(s, var)
   if var ~= '_' then
      s:w("var ", iol_atom(var), " = ")
   end
end

function class.w_lambda(s,args,body)
   s:indented(
      function()
         s:w("(", s:commalist(args),")"," {\n",s:tab())
         s:comp(body)
         s:w("\n",s:tab(-1),"}")
      end)
end

-- Compiler is centered around 'block', compiling a sequence of binding forms.
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
                s:w("function")
                s:w_lambda(m.args, m.expr)
            end},
            {"(,var (if ,cond ,etrue, efalse))", function(m)  -- FIXME var
                trace("IF",binding)
                s:w_maybe_assign(m.var)
                s:indented(
                   function()
                      s:w("case ", iol_atom(m.cond), " of\n")
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
                   s:w(iol_atom(m.fun),"(",s:commalist(m.args),")")
                end
            end},
            {"(,var (labels . ,bindings))", function(m)
                error('labels_toplevel_only')
            end},
            -- requires scheme_blockval pass on input IR
            {"(_ (return ,expr))", function(m)
                s:w("return ")
                s:comp(m.expr)
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
                s:w(iol_atom(m.atom))
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

local function new(config)
   local obj = { match = se_match.new(), indent = 0, config = config or {} }
   setmetatable(obj, { __index = class })
   return obj
end
class.new = new
return class

