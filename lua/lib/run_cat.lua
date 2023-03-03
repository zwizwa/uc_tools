-- Part of uc_tools/lua
-- https://opensource.org/licenses/MIT
-- COPYRIGHT HOLDER = Tom Schouten
-- YEAR = 2023

-- Interpreter for Forth-like / Joy-like stack language.

return function(maybe_cfg)

local cfg = maybe_cfg or {}
local script_dir = cfg.script_dir or nil -- optional
local plugin_dir = cfg.plugin_dir or nil -- optional
local log_desc = cfg.log_desc or function() end

assert(cfg.read_cat)
local read = cfg.read_cat({
      -- Config
})

local cat = {}

local function push(self, v)
   assert(v)
   table.insert(self.ds, v)
   return rv
end

local function execute(self, f)
   -- execute dictionary method
   assert(f)
   assert(type(f) == 'function')
   -- typically words return nil, but we propagate anyway in
   -- case user has different ideas.
   return f(self)
end

function cat:interpret()
   local w = self:pop()

   -- New style literals
   if type(w) == 'table' then
      assert(w.tag)
      return push(self, w[w.tag])
   end

   -- All the rest are strings
   assert(type(w) == 'string')

   -- Number literals
   local val = tonumber(w)
   if val then
      return push(self, val)
   end

   -- Dictionary words
   local f = self[w]
   if f then
      return execute(self, f)
   end

   -- Command script
   if nil ~= script_dir then
      local script = script_dir .. w
      local f = io.open(script, "r")
      if f ~= nil then
         f:close()
         push(self, script)
         return self:interpret_file()
      end
   end

   -- Lua plugin
   if nil ~= plugin_dir then
      local plugin = plugin_dir .. w .. ".lua"
      local f = io.open(plugin, "r")
      if f ~= nil then
         f:close()
         if true then
            -- run it using loadscript to allow reloading
            push(self, w)
            return self:plugin()
         else
            -- run it using require which gives better error handling
            -- FIXME: maybe perform require when script fails?
         end
      end
   end

   -- FIXME: This should abort to toplevel.
   log_desc({error='undefined',word=w})
   self:abort()

end



function lookup(self, w)
   -- It's useful to have an extra level here: user can add
   -- dictionarys to the mixins array.
   local val

   val = rawget(self, w)
   if val ~= nil then return val end

   local mixins = rawget(self, 'mixins')
   if mixins then
      for i,cmds in ipairs(mixins) do
         if type(cmds) == 'table' then
            val = rawget(cmds,w)
         elseif type(cmds) == 'function' then
            val = cmds(self, w)
         end
         if val ~= nil then return val end
      end
   end
   val = cat[w]
   if val ~= nil then return val end
   return nil
end

function cat.new(env)
   obj = { ds = {}, env = env, mixins = {} }
   setmetatable(obj, {__index = lookup })
   -- obj isn't always needed so pass it as second value
   return obj
end

function cat:pop()
   local n = #self.ds
   local rv = self.ds[n]
   table.remove(self.ds, n)
   return rv
end

function cat:ps() log_desc(self.ds) end
function cat:p()  log_desc(self:pop()) end
local function op2(fun)
   return
      function(self)
         local b = self:pop()
         local a = self:pop()
         push(self, fun(a,b))
      end
end
cat['+'] = op2(function(a,b) return a+b end)
cat['-'] = op2(function(a,b) return a-b end)

local function parse_string(str)
   local tokens = read.tokenize(read.characters(str))
   local syntax = read.parse(read.elements(tokens))
   -- log_desc(syntax)
   -- The parser output is quoted code. Unpack outer code quotation.
   assert(syntax and syntax.tag == 'code')
   return read.elements(syntax.code)
end

function cat:interpret_string()
   local line = self:pop()
   for w in parse_string(line) do
      push(self, w)
      self:interpret()
   end
end

function cat:interpret_file()
   local script = self:pop()
   assert(script)
   local f = io.open(script, "rb")
   if f == nil then
      error("Can't open command file '" .. script .. "'\n")
      exit(1)
   end
   local str = f:read("*all")
   f:close()
   push(self, str)
   self:interpret_string()
end

return cat
end
