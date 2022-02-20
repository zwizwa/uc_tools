-- C code pretty-printer
-- This is a partial clone of backend_lua.lua

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

-- FIXME: Go over tree once to collect the reverse mappings:
--  (r22:r21 (r34:base-ref 'cons))
-- FIXME: Create a library with generic iterators.
-- FIXME: Actually it's simpler: compile the quotation into cons in the frontend.
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
      log_desc({bad_atom = a})
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
           s:w(iol_atom(m.vec), "[", iol_atom(m.idx),"]")
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
                      s:w("T ", iol_atom(b.var), "(", s:commalist(b.args),") {","\n")
                      s:w_body(b.expr)
                      s:w(s:tab(),"}")
                   end)
            end},
            -- Other variable definitions
            {"(,var ,expr)", function(b)
                -- Lua does not allow naked values to appear in a
                -- statement position.  Replace them with a comment.
                s:w("T ", iol_atom(b.var), " = ")
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
function class.compile(s,top_expr)

   -- -- The IR expr is a single lambda expression, parameterized by
   -- -- 'lib_ref', a function that performs library symbol lookup for
   -- -- all free variables that were found by scheme_frontend.
   -- local bindings =
   --    s.match(
   --       top_expr,
   --       {{'(lambda (,base_ref) (block . ,bindings))',
   --         function(m)
   --            assert(m.base_ref.class == 'var')
   --            return m.bindings
   --    end}})

   -- FIXME: We now take output from scheme_sm pass, which doesn't
   -- have lambda.
   


   local mod_body = {}
   s:parameterize(
      {out = mod_body},
      function()
         -- s:w_bindings(bindings)
         s:comp(top_expr)
         s:w("\n")
      end)
   local mod = {
      mod_body
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



-- Recursive expression compiler.
function class.comp(s,expr)
   s.match(
      expr,
      {
         {"(block . ,bindings)", function(m)
             s:w(s:tab(),"{\n")
             s:w_bindings(m.bindings)
             s:w(s:tab(),"}\n")
         end},
         {"(labels . ,bindings)", function(m)
             -- FIXME: labels
             s:w(s:tab(),"{\n")
             s:w_bindings(m.bindings)
             s:w(s:tab(),"}\n")
         end},
         {"(lambda ,vars ,expr)", function(m)
             s:w("function(",s:commalist(m.vars),") {\n")
             s:indented(
                function()
                   s:w_body(m.expr)
                   s:w(s:tab(),"}")
                end,
                0)
         end},
         {"(if ,cond ,etrue, efalse)", function(m)
             s:w("if (", iol_atom(m.cond), ") {\n")
             s:indented(
                function()
                   s:w_body(m.etrue)
                   s:w(s:tab(), "} else {\n")
                   s:w_body(m.efalse)
                   s:w(s:tab(), "}")
                end,
                -1)
         end},
         {"(set! ,var ,expr)", function(m)
             s:w(iol_atom(m.var), " = ")
             s:i_comp(m.expr)
             s:w(";")
         end},
         {"(return ,expr)", function(m)
             s:w("return ")
             s:i_comp(m.expr)
         end},
         {"(app ,fun . ,args)", function(m)
             -- Note that in C, we require all function applications
             -- to be top level functions, represented by the 'prim'
             -- data type.  Any flattening needs to happen in a
             -- previous pass.
             assert(m.fun.class == 'prim')
             local w_f = m.fun.name and form[m.fun.name]
             if w_f then
                w_f(s, m.args)
             else
                s:w(m.fun.name,"(",s:commalist(m.args),");")
             end
         end},
         {"(goto ,label)", function(m)
             s:w("goto ", iol_atom(m.label), ":")
         end},
         {"(hint ,tag . ,args)", function(m)
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

