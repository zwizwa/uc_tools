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

local function mangle(name)
   if not name then return "" end
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
   return {"_",name}
end

local function iol_atom(var)
   if type(var) == 'table' and var.class == 'var' then
      return {var.unique,mangle(var.var)}
   end
   return se.iolist(var)
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
   return { class = "iolist", iolist = out }
end

function class.i_comp(s, expr)
   s:indented(function() s:comp(expr) end)
end

-- Recursive expression compiler.
function class.comp(s,expr)
   s.match(
      expr,
      {
         -- FIXME: All these need expression result assignment and
         -- explicit return.  Solve that in a preprocessing step, e.g:

         -- (lambda () (block (
         --   (a #<nil>)
         --   (_ (if cond
         --        (set! a 1)
         --        (set! a 2)))
         --   (return a)
         --   ))

         -- Reduced block form where all statements have been included
         -- as bindings to '_' to indicate ignored value.
         {"(block . ,bindings)", function(m)
             s:w(s:tab(),"do\n")
             s:w_bindings(m.bindings)
             s:w(s:tab(),"end\n")
         end},
         -- Reduced lambda form with single body expression..
         -- Second form is generic.
         {"(lambda ,vars ,expr)", function(m)
             s:w("function(",commalist(m.vars),")\n")
             s:indented(
                function()
                   s:w_body(m.expr)
                   s:w(s:tab(),"end")
                end,
                -2)
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
             s:w(iol_atom(m.fun),"(",commalist(m.args),")")
         end},
         {",atom", function(m)
             if se.expr_type(m.atom) == 'var' then
                s:w(iol_atom(m.atom))
             elseif type(m.atom) == 'number' then
                s:w(m.atom)
             elseif type(m.atom) == 'string' then
                s:w(m.atom)
             elseif m.atom == scheme_frontend.void then
                s:w('nil')
             else
                log_se_n(expr,"BAD: ")
                error("syntax error")
             end
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

