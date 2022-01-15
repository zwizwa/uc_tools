-- Limited Scheme to Lua compiler (no tail calls, continuations)

-- Older notes:
--
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
--
-- HOAS wrapper was added.  Seemed simpler to add some conditionals
-- here and there.

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

slc.symbol_prefix = '_r'

function slc:hoas(iolist)
   return ifte(self.config.hoas, iolist, "")
end

function slc:is_bound(var)
   for e in se.elements(self.env) do
      if e == var then return true end
   end
   return false
end

function slc:compile_function_body(body)
   self:parameterize(
      {var = self:gensym()},
      function()
         self:w(self:tab(), "local ", self.var, "; ")
         self:compile(body)
         self:w("return ", self.var, "\n", self:tab())
      end)
end

form['lambda'] = function(self, expr)
   local _, args, body = se.unpack(expr, {n = 2, tail = true})
   self:w(maybe_assign(self.var),
          self:hoas({self.config.hoas,":lambda(",se.length(args),", "}),
          "function(", comp.clist(a(args)), ")\n")
   self:save_context(
      {'var','env'},
      function()
         for var in se.elements(args) do
            self.env = {var, self.env}
         end
         self:indented(
            function()
               self:compile_function_body({'begin',body})
            end)
      end)
   self:w("end",
          self:hoas(")"),
          "\n", self:tab())
end

form['begin'] = function(self, expr)
   self:begin(expr,nil)
end
form['module-begin'] = function(self, expr)
   self:w(self:hoas({"return function(",self.config.hoas,")\n"}))
   if not self.config.hoas then
      -- FIXME: There is no support for infix atm so for testing we
      -- insert this primitive.  Not necessary in HOAS mode where it
      -- can be injected.
      self:w("local function add(a,b) return a + b end\n")
   end
   self:w("mod = {}\n")
   local function register(var)
      self:w("mod.",var," = ",var,"\n")
   end
   self:begin(expr, register)
   self:w("return mod\n")
   self:w(self:hoas("end\n"))
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
   self:compile_letstar(bindings, forms)
end
function slc:compile_letstar(bindings, forms)
   self:block(
      function()
         self:save_context(
            {'var','env'},
            function()
               for form in se.elements(bindings) do
                  local var, var_expr = se.unpack(form, {n = 2})
                  assert(type(var) == 'string')
                  self.var = var
                  self.env = {var, self.env}
                  self:w("local ",var,"; ")
                  self:compile(var_expr)
               end
         end)
         self:compile({'begin', forms})
   end)
end

form['if'] = function(self, expr)
   local _, condition, iftrue, iffalse = se.unpack(expr, {n = 4})
   local li = self:let_insert({string = true, number = true})
   condition = li:maybe_insert_var(condition)
   if not li:compile_inserts({'if', condition, iftrue, iffalse}) then
      if not self.config.hoas then
         local function ce(e)
            self:indented(function()
                  self:w(self:tab());
                  self:compile(e)
            end)
         end
         self:w('if ', condition, ' then\n');
         ce(iftrue)
         self:w('else\n')
         ce(iffalse)
         self:w('end\n',self:tab())
      else
         local function ce(e)
            self:w(self:tab(),"function()\n")
            self:save_context(
               {'var','indent'},
               function()
                  self:inc('indent')
                  self:compile_function_body(e)
               end)
            self:w(self:tab(),"end")
         end
         self:w(maybe_assign(self.var))
         self:w(self.config.hoas,':ifte(',condition,', \n');
         ce(iftrue)
         self:w(', \n')
         ce(iffalse)
         self:w(')\n',self:tab())
      end
   end
end

function slc:compile(expr)
   if type(expr) == 'table' then
      -- S-expression
      local form_name, form_args = se.unpack(expr, {n = 1, tail = true})
      assert(form_name and type(form_name == 'string'))
      local form_fn = self.form[form_name]
      if form_fn then
         form_fn(self, expr)
      else
         -- Application
         local li = self:let_insert({string = true, number = true})
         local anf_args = {}
         for arg in se.elements(form_args) do
            table.insert(anf_args, li:maybe_insert_var(arg))
         end
         if not li:compile_inserts({form_name, se.array_to_list(anf_args)}) then

            local function _w(iol)
               self:w(maybe_assign(self.var),iol,"\n",self:tab())
            end

            if self.config.hoas then
               local function lookup(var)
                  -- Non-lexical variables need to be explicitly
                  -- defined in the primitive dictionary.  Note 'var'
                  -- is not a good name here.
                  return ifte(type(var) == 'string' and self:is_bound(var),
                              var,{self.config.hoas,".prim.",var})
               end
               form_name = lookup(form_name)
               for i=1,#anf_args do anf_args[i] = lookup(anf_args[i]) end

               _w({self.config.hoas,":app(",form_name,", ",comp.clist(anf_args),")"})
            else
               _w({form_name,"(",comp.clist(anf_args),")"})
            end
         end
      end
   else
      -- Lua doesn't allow for naked variable references or constants,
      -- so if there is no variable to bind, don't generate anything.
      if self.var then
         if type(expr) == 'string' then
            -- Variable reference
            self:w(maybe_assign(self.var),expr,"\n",self:tab())
         elseif type(expr) == 'number' then
            -- Constant
            self:w(maybe_assign(self.var),expr,"\n",self:tab())
         end
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

function slc:compile_module_file(filename)
   local stream = io.open(filename,"r")
   local parser = se.new(stream)
   parser.log = self.config.log
   local exprs = parser:read_multi()
   local expr = {'module-begin',exprs}
   stream:close()
   return self:to_string(expr)
end

function slc:loadscheme(filename)
   local str = self:compile_module_file(filename)
   if self.config.log then self.config.log(str) end
   return loadstring(str)()
end

function slc:reset()
   self.nb_sym = 0
   self.env = se.empty
   assert(0 == self.indent)
end

function slc.new(config)
   local function index(obj,k)
      for _,tab in ipairs({obj, slc, comp}) do
         local mem = rawget(tab, k)
         if mem then return mem end
      end
   end
   local obj = { indent = 0, config = config or {} }
   setmetatable(obj, {__index = index})
   obj:reset()
   return obj
end


return slc
