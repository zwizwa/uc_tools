-- Limited Scheme to Lua compiler (no tail calls, continuations)
--
-- Notes:
--
-- . Straightforward compilation. No tail calls, no continuations.
--
-- . Extending this to higher order syntax (HOAS) was straightforward
--   to do by adding conditionals to emit extra wrapper functions.
--
-- . This C code printer uses a different convention than smc.lua: the
--   next expression in a let* or begin list can be compiled at point,
--   i.e. the caller of :compile will perform the indentation.
--
-- . As long as this doesn't handle infinite loops or very large data
--   structures, the lack of tail call optimization is probably ok.

-- As for context: This is probably a distraction.  For smc.lua I'm
-- sticking with scheme.lua, a more direct Scheme interpreter that can
-- do tail calls.  Also, HOAS is not what I am looking for at this
-- time, as macros are probably a better bet for exploratory work.
-- See alose scheme_macros.lua

local se            = require('lib.se')
local iolist        = require('lib.iolist')
local comp          = require('lib.comp')
local scheme_macros = require('lib.scheme_macros')

-- Tools
require('lib.log')
local function log_w(...)      iolist.write(log, {...}) end
local function log_se(e)       log_w(se.iolist(e)) end
local function log_se_n(e,tag) if(tag) then log(tag) end ; log_se(e) ; log('\n') end

local function ifte(c,t,f)
   if c then return t else return f end
end
local function maybe_assign(var)
   if not var then return "" end
   return {var, " = "}
end

-- FIXME: Maybe best to just map everything to generated names and
-- only preserve names at module boundary.
local function mangle_name(name)
   local subst = {
      ["-"] = "_dash_",
      ["!"] = "_bang_",
      ["?"] = "_pred_",
      [">"] = "_gt_",
      ["/"] = "_div_",
      ["="] = "_is_",
   }
   for from,to in pairs(subst) do
      name = string.gsub(name,from,to)
   end
   return name
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


function slc:ref(scheme_var)
   local var = mangle_name(scheme_var)
   for e in se.elements(self.env) do
      if e == var then return e end
   end
   return nil
end
function slc:is_bound(var)
   return slf:reaf(var) and true
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

form['module-begin'] = function(self, expr)

   -- module-set! will add to this dictionary
   self:w("local _mod_defs = {}\n")

   self:w(self:hoas({"return function(",self.config.hoas,")\n"}))
   if not self.config.hoas then
      -- FIXME: There is no support for infix atm so for testing we
      -- insert this primitive.  Not necessary in HOAS mode where it
      -- can be injected.
      self:w("local function add(a,b) return a + b end\n")
   end


   -- Parameterize scheme_macros.begin to use 'module-letrec' instead
   -- of 'letrec' to treat the toplevel definitions differently.
   local top_expr = {'begin', se.cdr(expr)}
   local begin_config = {letrec = 'module-letrec'}
   local module_letrec_expr = scheme_macros.begin(top_expr, begin_config)
   -- log("module-begin: ") ; log_se(module_letrec_expr)
   self:compile(module_letrec_expr)

   self:w("return _mod_defs\n")
   self:w(self:hoas("end\n"))
end
form['module-letrec'] = function(self, expr)
   self:compile(scheme_macros.letrec(expr, {set = 'module-set!'}))
end
function compile_set(mod)
   return
      function(self, expr)
         local set, var, vexpr = se.unpack(expr, {n = 3, tail = true})
         assert(type(var) == 'string')
         local li = self:let_insert({string = true, number = true})
         vexpr = li:maybe_insert_var(vexpr)
         if not li:compile_inserts(l(set, var, vexpr)) then
            local mvar = mangle_name(var)
            self:w(mvar, " = ", vexpr, "\n",self:tab())
            if mod then
               -- Don't mangle the keys.
               self:w("_mod_defs['",var,"'] = ",vexpr,"\n",self:tab())
            end
         end
      end
end
form['set!']        = compile_set(false)
form['module-set!'] = compile_set(true)


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
                  local var, var_expr = comp.unpack_binding(form, 'nil')
                  assert(type(var) == 'string')
                  var = mangle_name(var)
                  self.var = var
                  self.env = {var, self.env}
                  self:w("local ",var,"; ")
                  self:compile(var_expr)
               end
         end)
         self:compile_begin(forms)
   end)
end
function slc:compile_begin(forms)
   self:save_context(
      {'var'},
      function()
         local var = self.var
         for form, rest in se.elements(forms) do
            self.var = ifte(se.is_empty(rest), var, nil)
            -- log_desc({form = form})
            -- log('begin: form: ') ; log_se(form) ; log('\n')
            self:compile(form)
         end
      end)
end

form['if'] = function(self, expr)
   local _, condition, iftrue, iffalse = se.unpack(expr, {n = 4})
   local li = self:let_insert({string = true, number = true})
   condition = li:maybe_insert_var(condition)
   if not li:compile_inserts(l('if', condition, iftrue, iffalse)) then
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
            self:w("end")
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

-- Wrap some macros from scheme_macros.lua
local function macro(m)
   return function(self, expr)
      local expanded = m(expr)
      -- log_se(expanded) ; log('\n')
      self:compile(expanded)
   end
end
local function use_macros(names)
   for _, name in ipairs(names) do
      local m = scheme_macros[name]
      assert(m)
      form[name] = macro(m)
   end
end
use_macros({'begin','letrec'})

function slc:compile_trace(expr)
   log_se_n(expr, "compile_trace: ")
end

function slc:compile(expr)
   assert(expr)
   self:compile_trace(expr)
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
         form_name = li:maybe_insert_var(form_name)
         for arg in se.elements(form_args) do
            table.insert(anf_args, li:maybe_insert_var(arg))
         end
         if not li:compile_inserts({form_name, se.array_to_list(anf_args)}) then

            local function _w(...)
               self:w(maybe_assign(self.var),{...},"\n",self:tab())
            end

            if self.config.hoas then
               local function lookup(scheme_var)
                  assert(var and type(var) == 'string')
                  local var = mangle_name(scheme_var)
                  -- Lexical scope, represented as Lua var.
                  if (self:is_bound(var)) then
                     return var
                  end
                  -- In HOAS mode everything needs to be explicitly
                  -- defined in the prim table.
                  return {self.config.hoas,".prim.",var}
               end
               form_name = lookup(form_name)
               for i=1,#anf_args do anf_args[i] = lookup(anf_args[i]) end
               _w({self.config.hoas,":app(",form_name,", ",comp.clist(anf_args),")"})
            else
               -- In Lua mode we just assume variables are there and
               -- will only handle infix opereators specially.
               log_w('form_name: ',form_name,'\n')
               local infix = self.infix[form_name]
               if infix then
                  log_w('infix: ',infix,'\n')
                  assert (#anf_args == 2)
                  _w({anf_args[1], " ", infix, " ", anf_args[2]})
               else
                  _w({mangle_name(form_name),"(",comp.clist(anf_args),")"})
               end
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


slc.infix = {
   ['+'] = '+',
   ['-'] = '-',
   ['*'] = '*',
   ['<'] = '<',
   ['>'] = '>',
   ['and'] = 'and',
}

return slc
