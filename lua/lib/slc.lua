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
--
-- Other problem is nesting let: there are some optimizations
-- possible, but it might be simplest to compile all blocks in the
-- same way (declare var + assign inside block), and add an explicit
-- return at the end.
--
-- This printer uses a different convention than smc.lua: the next
-- expression in a let* or begin list can be compiled at point,
-- i.e. the caller of :compile will perform the indentation.

local se   = require('lib.se')
local comp = require('lib.comp')

local prompt = require('prompt')
local function log(str) io.stderr:write(str) end
function log_desc(obj) log(prompt.describe(obj) .. "\n") end

local function ifte(c,t,f)
   if c then return t else return f end
end
local function maybe_assign(var)
   if not var then return "" end
   return {var, " = "}
end


-- Shorthand for container type conversions
local l = se.list
local a = se.list_to_array

local form = {}
local slc = { form = form }


function slc:se_comment(expr)
   self:w("-- ", se.iolist(expr), "\n", self:tab())
end

slc.symbol_prefix = '_'

form['lambda'] = function(self, expr)
   local _, args, body = se.unpack(expr, {n = 2, tail = true})
   self:w(maybe_assign(self.var),"function(", comp.clist(a(args)), ")\n")
   self:save_context(
      {'var'},
      function()
         self:indented(
            function()
               self.var = self:gensym()
               self:w(self:tab(), "local ", self.var, "; ")
               self:compile({'begin',body})
               self:w("return ", self.var, "\n", self:tab())
            end)
      end)
   self:w("end\n", self:tab())
end

form['begin'] = function(self, expr)
   self:begin(expr,nil)
end
form['module-begin'] = function(self, expr)
   -- FIXME: There is no support for infix atm.
   self:w("local function add(a,b) return a + b end\n")
   self:w("mod = {}\n")
   local function register(var)
      self:w("mod.",var," = ",var,"\n")
   end
   self:begin(expr, register)
   self:w("return mod\n")
end

function slc:begin(expr, register)
   local _, forms = se.unpack(expr, {n = 1, tail = true})
   self:save_context(
      {'var'},
      function()
         local var = self.var
         for form, rest in se.elements(forms) do
            -- Define is only valid inside begin.
            if type(form) == 'table' and form[1] == 'define' then
               local _, spec, fun_body = se.unpack(form, { n = 2, tail = true })
               local name, args = se.unpack(spec, { n = 1, tail = true })
               assert(type(name) == 'string')
               -- log('define ' .. name .. '\n')
               self.var = name
               self:w("local ", self.var, "; ")
               self:compile({'lambda',{args,fun_body}})
               if register then register(self.var) end
            else
               self.var = ifte(se.is_empty(rest), var, nil)
               self:compile(form)
            end
         end
      end)
end

function slc:indented(fun)
   self:save_context({'indent'},
      function()
         self:inc('indent')
         fun()
      end)
end

function slc:block(fun)
   assert(self.var)
   self:w("do\n")
   self:indented(
      function()
         self:w(self:tab())
         fun()
      end)
   self:w("end\n", self:tab())
end


form['let*'] = function(self, expr)
   local _, bindings, forms = se.unpack(expr, {n = 2, tail = true})
   self:block(
      function()
         self:save_context(
            {'var'},
            function()
               for form in se.elements(bindings) do
                  local var, var_expr = se.unpack(form, {n = 2})
                  assert(type(var) == 'string')
                  self.var = var
                  self:w("local ",var,"; ")
                  self:compile(var_expr)
               end
         end)
         self:compile({'begin', forms})
   end)
end

function slc:compile(expr)
   if type(expr) == 'string' then
      -- Variable reference
      self:w(maybe_assign(self.var),expr,"\n",self:tab())
   elseif type(expr) == 'number' then
      -- Constant
      self:w(maybe_assign(self.var),expr,"\n",self:tab())
   elseif type(expr) == 'table' then
      -- S-expression
      local form_name, form_args = se.unpack(expr, {n = 1, tail = true})
      assert(form_name and type(form_name == 'string'))
      local form_fn = self.form[form_name]
      if form_fn then
         form_fn(self, expr)
      else
         -- Application
         -- FIXME: convert to ANF
         self:w(maybe_assign(self.var),
                form_name,"(",comp.clist(a(form_args)),")\n",self:tab())
      end
   end
end

function slc:to_string(expr)
   local buf = {}
   self:save_context(
      {'write'},
      function()
         self.write = function(_, str) table.insert(buf, str) end
         self:compile(expr)
      end)
   return table.concat(buf,"")
end
function slc:eval(expr)
   local lua_code = self:compile_to_string(expr)
   return loadstring(lua_code)
end

function slc:loadscheme(filename)
   local stream = io.open(filename,"r")
   local parser = se.new(stream)
   parser.log = function(self, str) io.stderr:write(str) end
   local exprs = parser:read_multi()
   local expr = {'module-begin',exprs}
   stream:close()
   local str = self:to_string(expr)
   log(str)
   return loadstring(str)
end

function slc:reset()
   self.sym_n = 0
   assert(0 == self.indent)
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
   obj:reset()
   return obj
end


return slc
