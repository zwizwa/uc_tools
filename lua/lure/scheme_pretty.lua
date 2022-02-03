-- Pretty printer for special forms.

local se = require('lure.se')
local iolist = require('lure.iolist')
local comp = require('lure.comp')

local ins = table.insert
local a2l = se.array_to_list
local l = se.list
local car = se.car
local cadr = se.cadr
local cdr = se.cdr


-- Define types

local function s_id(s, thing) return thing end


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

local pprint_form = {
   ['block'] = function(s, expr)
      local _, bindings, rest = se.unpack(expr, {n=2, tail=true})
      local nb = se.length(bindings)
      if nb > 0 then
         s:w(s:tab(),"(block (\n")
      else
         s:w(s:tab(),"(block ()\n")
      end
      s:indented(
         function()
            s:indented(
               function()
                  for binding in se.elements(bindings) do
                     if se.length(binding) == 1 then
                        s:w(s:tab(),"(",car(binding),")\n")
                     else
                        local var, vexpr = se.unpack(binding, {n=2})
                        if type(vexpr) ~= 'table' then
                           s:w(s:tab(),se.iolist(binding),"\n")
                        else
                           s:w(s:tab(),"(",var,"\n")
                           s:indented(
                              function() s:pprint(vexpr) end)
                           s:w(s:tab(),")\n")
                        end
                     end
                  end
               end)
            if nb > 0 then
               s:w(s:tab(),")\n")
            end
            for e in se.elements(rest) do
               s:pprint(e)
            end
         end)
      s:w(s:tab(),")\n")
   end,
   -- Forms that are not in this table are treated same as pp_app
   ['lambda'] = function(s, expr)
      local _, vars, body = se.unpack(expr, {n=2, tail=true})
      s:w(s:tab(),"(lambda ", se.iolist(vars),"\n")
      s:indented(
         function()
            for e in se.elements(body) do
               s:pprint(e)
            end
      end)
      s:w(s:tab(),")\n")
   end,
   ['if'] = function(s, expr)
      local _, var, etrue, efalse = se.unpack(expr, {n=4})
      s:w(s:tab(),"(if ",var,"\n")
      s:indented(
         function()
            s:pprint(etrue)
            s:pprint(efalse)
         end)
      s:w(s:tab(),")\n")
   end,
}
local function pp_app(s, expr)
   s:w(s:tab(),se.iolist(expr),"\n")
end

local pprinter = {
   ['string'] = s_id,
   ['number'] = s_id,
   ['boolean'] = s_id,
   ['pair'] = function(s, expr)
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
   local f = pprinter[typ]
   if f == nil then error('expand: bad type ' .. typ) end
   return f(s, expr)
end

local function pprint_to_stream(s,stream,expr)
   local function w(s, ...)
      iolist.write(
         function(str) stream:write(str) end,
         {...})
   end
   comp.parameterize(
      s, { w = w },
      function() s:pprint(expr) end)
end

local class = {
   pprint_to_stream = pprint_to_stream,
   pprint = pprint,
   pprint_form = pprint_form,
   indented = indented,
   tab = tab,
   parameterize = comp.parameterize,
}
local function new()
   local obj = { count = 0, indent = 0 }
   setmetatable(obj, { __index = class })
   return obj
end
class.new = new
return class

