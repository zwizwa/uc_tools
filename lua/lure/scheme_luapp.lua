-- Lua code pretty-printer from lambda, block, if, set!
-- Based on se_match

local se        = require('lure.se')
local se_match  = require('lure.se_match')
local iolist    = require('lure.iolist')
local lure_comp = require('lure.comp')
local scheme_frontend = require('lure.scheme_frontend')
local l = se.list
local ins = table.insert

local class = {}

-- Cherry-pick some methods (micro-mixin?)
class.parameterize = lure_comp.parameterize
class.indented     = lure_comp.indented
class.tab          = lure_comp.tab


-- Convention for newline/tab:

-- Expressions are always compiled in binding position, are already at
-- indented position and should not print newline.

local infix = {
   ['add'] = '+'
}

local function free_var(name)
   if not lua_name then
      error("undefined variable '" .. name .. "'")
   end
   return lua_name
end

local function mangle(var)
   assert(var and var.unique)
   local name = var.var
   if not name then return var.unique end
   if var.free then return free_var(name) end

   local subst = {
      ["next"] = "nxt",
      ["-"] = "_", -- "_dash_",
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

local function iol_atom(a)
   if type(a) == 'table' and a.class == 'var' then
      return mangle(a)
   elseif type(a) == 'number' then
      return a
   elseif type(a) == 'string' then
      return a
   elseif a == scheme_frontend.void then
      return 'nil'
   elseif type(a) == 'table' and a.class == 'quote' and type(a.expr) == 'string' then
      return {"'",a.expr,"'"}
   -- FIXME: This is likely not correct for all
   elseif type(a) == 'table' and a.class then
      return se.iolist(a)
   else
      log_desc({bad_atom = var})
      log_se_n(a,"BAD_ATOM: ")
      error("syntax error")
   end
end

local special_function = {}
special_function['module-register!'] = function(s, args)
   s.match(
      args,
      {{"(,name ,ref)", function(m)
           s:w("mod[",iol_atom(m.name),"] = ",iol_atom(m.ref))
        end}})
end

local function commalist(lst)
   local iol = {}
   for el, last in se.elements(lst) do
      ins(iol, iol_atom(el))
      if not se.is_empty(last) then
         ins(iol, ", ")
      end
   end
   return iol
end


function class.w_bindings(s, bindings)
   for binding in se.elements(bindings) do
      s:w(s:tab())
      s.match(
         binding,
         {
            -- Statements
            {"(_ ,expr)", function(b)
                s:i_comp(b.expr)
            end},
            -- Special case the function definitions
            {"(,var (lambda ,args ,expr))", function(b)
                s:indented(
                   function()
                      s:w("local function ", iol_atom(b.var), "(", commalist(b.args),")","\n")
                      s:w_body(b.expr)
                      s:w(s:tab(),"end")
                   end)
            end},
            -- Other variable definitions
            {"(,var ,expr)", function(b)
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
   local mod =
      {"local mod = {}\n", out,
       "return mod\n"}
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
             s:w("function(",commalist(m.vars),")\n")
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
         {"(,fun . ,args)", function(m)
             local w_f = m.fun.var and special_function[m.fun.var]
             -- FIXME: Also do infix this way
             if w_f then
                w_f(s, m.args)
             else
                local op = m.fun.var and infix[m.fun.var]
                if op then
                   local a, b = se.unpack(m.args, {n=2})
                   s:w(iol_atom(a)," ",op," ",iol_atom(b))
                else
                   s:w(iol_atom(m.fun),"(",commalist(m.args),")")
                end
             end
         end},
         {",atom", function(m)
             s:w_atom(m.atom)
         end}
      }
   )
end

local function new()
   -- FIXME: Make sure match raises error on mismatch.
   local obj = { match = se_match.new(), indent = 0 }
   setmetatable(obj, { __index = class })
   return obj
end
class.new = new
return class

