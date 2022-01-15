-- Mixin for writing naive direct code emitting compilers.
-- This is factored out from smc.lua to be re-used for different compilers.
local comp = {}

local se = require('lib.se')
local l = se.list

-- Make writing output syntax as convenient as possible.
-- Erlang style strings + multiple arguments.
function comp:w(...)
   for _,el in ipairs({...}) do
      if type(el) == 'table' then
         self:w(unpack(el))
      else
         self:write(el)
      end
   end
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

-- Track maximum
function comp:track_max(varname, val)
   if self[varname] < val then
      self[varname] = val
   end
end

-- Counter
function comp:inc(countername)
   local n = self[countername]
   self[countername] = n + 1
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




return comp
