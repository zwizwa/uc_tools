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

local lib = require('lure.slc_runtime')

local function mangle(var)
   assert(var and var.unique)
   local name = var.var
   if not name then return var.unique end
   if name == "base-ref" then return "lib" end

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
   -- pprint:pprint_to_stream(io.stderr,expr)
   local out = {}
   s:parameterize(
      {out = out},
      function()
         -- Toplevel block can be removed.  We assume the toplevel
         -- expression is a block representing a Lua module.
         s.indent = -1 -- Undo indent in w_bindings
         s.match(
            expr,
            {{"(block . ,bindings)", function(m)
                 s:w_bindings(m.bindings)
         end}})
         s:w("\n")
      end)
   local mod = {
      "local mod = {}\n",
      "local lib = require('lure.slc_runtime').new(mod)\n",
      out,
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

-- Recursive expression compiler.
function class.comp(s,expr)
   s.match(
      expr,
      {
         {"(block . ,bindings)", function(m)
             s:w(s:tab(),"do\n")
             s:w_bindings(m.bindings)
             s:w(s:tab(),"end\n")
         end},
         {"(lambda ,vars ,expr)", function(m)
             s:w("function(",s:commalist(m.vars),")\n")
             s:indented(
                function()
                   s:w_body(m.expr)
                   s:w(s:tab(),"end")
                end,
                0)
         end},
         {"(if ,cond ,etrue, efalse)", function(m)
             s:w("if ", iol_atom(m.cond), " then\n")
             s:indented(
                function()
                   s:w_body(m.etrue)
                   s:w(s:tab(), "else\n")
                   s:w_body(m.efalse)
                   s:w(s:tab(), "end")
                end,
                -1)
         end},
         {"(set! ,var ,expr)", function(m)
             s:w(iol_atom(m.var), " = ")
             s:i_comp(m.expr)
         end},
         {"(return ,expr)", function(m)
             s:w("return ")
             s:i_comp(m.expr)
         end},
         {"(app ,fun . ,args)", function(m)
             local w_f = m.fun.var and form[m.fun.var]
             if w_f then
                w_f(s, m.args)
             else
                s:w(iol_atom(m.fun),"(",s:commalist(m.args),")")
             end
         end},
         {"(,form . ,args)", function(m)
             error("form '" .. m.form .. "' not supported")
         end},
         {",atom", function(m)
             s:w_atom(m.atom)
         end}
      }
   )
end

local function new(config)
   local obj = { match = se_match.new(), indent = 0, config = config or {} }
   setmetatable(obj, { __index = class })
   return obj
end
class.new = new
return class

