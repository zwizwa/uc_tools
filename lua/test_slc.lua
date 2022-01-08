#!/usr/bin/env lua
package.path = package.path .. ";./?.lua"

-- Scheme to Lua compiler.
-- Is this worth it?
--
-- Yes.  It might be useful to plug this into the interpreter,
-- essentially for the lambda forms.  However, it will probably have
-- some limitations, e.g. tail calls will be difficult to make work
-- properly without some kind of outer interpreter loop.
--
-- So this is just a doodle.
--
-- The tail call optimization could actually do the same as the
-- interpreter, replacing expressions with thunks?


local se   = require('lib.se')
local comp = require('lib.comp')


local function ifte(c,t,f)
   if c then return t else return f end
end

-- Shorthand for container type conversions
local l = se.list
local a = se.list_to_array

local form = {}
local slc = { form = form }

function slc:se_comment(expr)
   self:w(self:tab(), "-- ", se.iolist(expr), "\n")
end

form['lambda'] = function(self, expr)
   local _, args, body = se.unpack(expr, {n = 2, tail = true})
   self:w(self:tab(), "function(", comp.clist(a(args)), ")\n")
   self:save_context(
      {'indent'},
      function()
         self:inc('indent')
         self:w(self:tab(), "# body\n")
         self:compile({'begin',body})
      end)
   self:w("end\n")
end

form['begin'] = function(self, expr)
   self:se_comment(expr)
   local _, forms = se.unpack(expr, {n = 1, tail = true})
   for form, rest in se.elements(forms) do
      self.binder = ifte(se.is_empty(rest), "return ", nil)
      se:compile(form)
   end
end

function slc:compile(expr)
   if type(expr) == 'string' then
      -- Variable reference
   elseif type(expr) == 'table' then
      -- S-expression
      local form_name, form_args = se.unpack(expr, {n = 1, tail = true})
      assert(form_name and type(form_name == 'string'))
      local form_fn = self.form[form_name]
      if form_fn then
         form_fn(expr)
      else
         -- Application
         self:w(self:tab(), self.binder or "",
                form_name,"(",comp.clist(a(form_args)),")\n")
      end
   end
end

function slc.new()
   local function index(obj,k)
      for _,tab in ipairs({obj, slc, comp}) do
         local mem = rawget(tab, k)
         if mem then return mem end
      end
   end
   local obj = { indent = 0 }
   setmetatable(obj, {__index = index})
   return obj
end

local function test()
   local expr = l('lambda',l('a','b'),l('add','a','b'))
   local c = slc.new()
   slc.form.lambda(c, expr)
end

test()
