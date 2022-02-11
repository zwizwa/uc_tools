-- Pretty printer for special forms.

local se     = require('lure.se')
local iolist = require('lure.iolist')
local comp   = require('lure.comp')

local ins = table.insert
local a2l = se.array_to_list
local l = se.list
local car = se.car
local cadr = se.cadr
local cdr = se.cdr


-- Define types



local function trace(tag, expr)
   log('\n')
   log_se_n(expr, tag .. ": ")
end

local function w_se(s, expr)
   s:w(se.iolist(expr))
end
local function indented(s, fn)
   s:parameterize({ indent = s.indent + 1 }, fn)
end
local function tab(s)
   local iol = {}
   for i=0,s.indent do ins(iol, "  ") end
   return iol
end

-- Forms that are not in this table are treated the same as pp_app
local pprint_form = {
   ['block'] = function(s, expr)
      local _, bindings = se.unpack(expr, {n=1, tail=true})
      s:w("\n",s:tab(),"(block")
      s:indented(
         function()
            for binding in se.elements(bindings) do
               s:w("\n",s:tab())
               local var, vexpr = se.unpack(binding, {n=2})
               s:w("(", se.iolist(var), " ")
               s:indented(
                  function()
                     s:pprint(vexpr)
                  end)
               s:w(")")
            end
      end)
      s:w(")")
   end,
   ['lambda'] = function(s, expr)
      local _, vars, body = se.unpack(expr, {n=2, tail=true})
      s:w("\n",s:tab(),"(lambda ", se.iolist(vars)," ")
      s:indented(
         function()
            s:w(s:tab())
            for e in se.elements(body) do
               s:pprint(e)
            end
      end)
   end,
   ['if'] = function(s, expr)
      local _, var, etrue, efalse = se.unpack(expr, {n=4})
      s:w("(if ", se.iolist(var)," ")
      s:indented(
         function()
            s:pprint(etrue)
            s:w(" ")
            s:pprint(efalse)
         end)
   end,
   ['labels'] = function(s, expr)
      local _, clauses = se.unpack(expr, {n=1, tail=true})
      s:w("(labels ")
      for clause in se.elements(clauses) do
         local name, body = se.unpack(clause, {n=2})
         s:indented(
            function()
               s:w("\n",s:tab(),"(", se.iolist(name), " ")
               s:indented(
                  function()
                     s:pprint(body)
                  end)
               s:w(")")
            end)
      end
      s:w(")")
   end,

}
local function pp_app(s, expr)
   s:w(se.iolist(expr))
end

local function pp_prim(s, thing)
   s:w(se.iolist(thing))
end

local pprinter = {
   ['string']  = pp_prim,
   ['number']  = pp_prim,
   ['boolean'] = pp_prim,
   ['pair']    = function(s, expr)
      local car, cdr = unpack(expr)
      local pp = s.pprint_form[car]
      if pp ~= nil then
         return pp(s, expr)
      else
         return pp_app(s, expr)
      end
   end
}
local function pprint(s, expr)
   local typ = se.expr_type(expr)
   assert(typ)
   local f = pprinter[typ]
   if f then
      return f(s, expr)
   else
      -- Assume everything else can be printed by se.iolist
      pp_prim(s, expr)
   end
end

local function pprint_to_stream(s,stream,expr)
   local function w(s, ...)
      iolist.write(
         function(str) stream:write(str) end,
         {...})
   end
   comp.parameterize(
      s, { w = w },
      function()
         s:pprint(expr)
         s:w("\n")
      end)
end

local class = {
   compile,
   pprint_to_stream = pprint_to_stream,
   pprint = pprint,
   pprint_form = pprint_form,
   indented = indented,
   tab = tab,
   parameterize = comp.parameterize,
}
function class.new()
   local obj = {
      indent = -1 -- 'block' will indent
   }
   setmetatable(obj, { __index = class })
   return obj
end

function class.log_pp(ir)
   local pp = class.new()
   pp:pprint_to_stream(io.stderr, ir)
end

return class

