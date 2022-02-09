-- Mixin for writing naive direct code emitting compilers.
-- This is factored out from smc.lua to be re-used for different compilers.
local comp = {}

local iolist = require('lure.iolist')
local se     = require('lure.se')
local l = se.list

-- Make writing output syntax as convenient as possible.
-- Erlang style strings + multiple arguments.

-- FIXME: in terms of iolist
function comp:w(...)
   for _,el in ipairs({...}) do
      if type(el) == 'table' then
         self:w(unpack(el))
      elseif el == true  then self:write('true')
      elseif el == false then self:write('false')
      else
         self:write(el)
      end
   end
end

-- E.g. for debug output
function comp:w_if0(c_code, comment)
   local c_n = {" // " , comment or "", "\n"}
   self:w("#if 0", c_n, c_code, "#endif", c_n)
end

-- Default implementation writes to stdout.
function comp:write(str)
   io.stdout:write(str)
end

function comp:tab()
   local strs = {}
   for i=1,self.indent do
      table.insert(strs,"  ")
   end
   return strs
end

function comp:indented(fn, maybe_offset)
   local offset = maybe_offset or 1
   self:parameterize({ indent = self.indent + offset }, fn)
end


function comp.clist(in_lst)
   local lst = {}
   for _,el in ipairs(in_lst) do
      table.insert(lst, el)
      table.insert(lst, ", ")
   end
   table.remove(lst, #lst)
   return lst
end

-- Tracking the language's lexical scope can be implemented using
-- dynamic scope in the compiler, as it recurses into the syntax.
-- This function saves and restores a list of compiler keys
-- (e.g. 'env', 'stack_ptr', 'indent').  Note that 'env' is
-- implemented using a cons list instead of a hash table to facilitate
-- sharing.
function comp:save_context(keys, inner_fun)
   local saved = {}
   for i,key in ipairs(keys) do saved[key] = self[key]  end
   local rv = inner_fun()
   for i,key in ipairs(keys) do self[key]  = saved[key] end
   return rv
end

-- New version using tables.
-- This is a bit closer to SRFI-39 parameterize form.
--
-- Note that setting a table value to nil is the same as it not being
-- there, so that doesn't work!
function comp:parameterize(bindings_tab, inner_fun)
   local saved = {}
   for key,val in pairs(bindings_tab) do
      saved[key] = self[key]
      self[key] = val
   end
   local rv = inner_fun()
   for key,_ in pairs(bindings_tab) do
      self[key] = saved[key];
   end
   return rv
end


-- Track maximum
--
function comp:track_max(varname, val)
   if self[varname] < val then
      self[varname] = val
   end
end
function comp:track_max_indexed(varname, n, val)
   assert(n)
   local max = self[varname][n]
   assert(max)
   if max < val then max = val end
   self[varname][n] = max
end

-- Counter
function comp:inc(countername)
   local n = self[countername]
   self[countername] = n + 1
   return n
end
function comp:dec(countername)
   local n = self[countername]
   self[countername] = n - 1
   return n
end

-- Generate symbols for let-insertion.  These use a prefix that is not
-- legal in the code so they never clash with source variables.
function comp:gensym(prefix)
   -- Generated symbols should not clash with any program text, which
   -- is why we use the comment character here.
   if not prefix then prefix = self.symbol_prefix or ";" end;
   local n = self:inc('nb_sym')
   return prefix..n
end


-- Environment maps variables to lists.
-- Create binding
function comp:def(var, val)
   assert(self.env)
   assert(val ~= nil)
   -- trace("DEF", l(var,val))
   local cell = {class = 'cell', val = val}
   self.env = {l(var,cell), self.env}
end
-- Reference and assigment operate on the chained environment.  One
-- table per function activation, linked by 'parent' member.
function comp:find_cell(var)
   local unique = var.unique
   assert(self.env)
   for pair in se.elements(self.env) do
      local v, rest = unpack(pair)
      if v.unique == unique then return se.car(rest) end
   end
   local mangled = iolist.to_string(se.iolist(var))
   log_se_n(self.env,"ENV:")
   error("undefined variable '" .. mangled .. "'")
end
function comp:ref(var)
   return self:find_cell(var).val
end
function comp:set(var, val)
   local cell = self:find_cell(var)
   cell.val = val
end




-- Let insertion happens often enough, so make an abstraction.
-- User needs to define :compile_letstar
local let_insert = {}
function comp:let_insert(config)
   if not config then config = { string = true } end
   local obj = {comp = self, bindings_list = se.empty, config = config}
   setmetatable(obj, {__index = let_insert})
   return obj
end
function let_insert:maybe_insert_var(expr)
   assert(expr)
   if self.config[type(expr)] then return expr end
   local var = self.comp:gensym()
   self.bindings_list = {l(var, expr), self.bindings_list}
   return var
end
function let_insert:compile(inner)
   self.comp:compile_letstar(self.bindings_list, l(inner))
end
function let_insert:compile_inserts(inner)
   if not self:bindings() then return false end
   self:compile(inner)
   return true
end
function let_insert:bindings()
   return not se.is_empty(self.bindings_list)
end

function comp.unpack_binding(binding, void)
   assert(binding)
   local var_name, maybe_expr = se.unpack(binding, { n = 1, tail = true })
   -- support empty bindings to support letrec (scheme_macros.lua)
   local expr = void or '#<void>'
   if not se.is_empty(maybe_expr) then
      expr = se.unpack(maybe_expr, {n = 1 })
   end
   assert(type(var_name) == 'string')
   assert(expr)
   return var_name, expr
end

-- Create a new compiler by concatenating multiple passes.  Each pass
-- takes input ir to output ir + and shares a global configuration
-- table.

-- Uncurried
function comp.multipass(maybe_config, passes, ir)
   local config = maybe_config or {}
   for _,pass in ipairs(passes) do
      -- Load the module, or use provided module
      local mod = pass
      if type(pass) == 'string' then
         -- log("PASS: " .. pass .. "\n")
         mod = require(pass)
      else
         pass = mod.name or "<anonymous-pass>"
      end
      -- Instantiate the compiler, passing it shared config.
      -- log_desc({multipass_config = config, pass = pass})

      local c = mod.new(config)
      -- Run the compiler
      ir = c:compile(ir)
      if config.trace then
         config.trace(ir, pass, config)
      end
   end
   return ir
end

-- Curried, wrapping multple passes as a single compiler object that
-- can be used as a pass.  ( Wannabe Monad. )
function comp.make_multipass_new(passes)
   return function(config)
      -- log_desc({multipass_config = config})
      return {
         compile = function(_, ir)
            return comp.multipass(config, passes, ir)
         end
      }
   end
end


return comp
