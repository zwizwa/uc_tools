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

local read
if cfg.read_cat then
   -- Instantiate it
   read = cfg.read_cat({
         -- Config
   })
end

local forth = {}

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

local function interpret(self, w)

   -- New style literals
   if type(w) == 'table' then
      assert(w.tag)
      return push(self, w[w.tag])
   end

   -- All the rest are strings
   assert(type(w) == 'string')

   -- Old style quoted string literals
   -- FIXME: Remove this in favor of new style literals
   if w:sub(1,1) == "'" then
      return push(self, w:sub(2))
   end

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
         return self:load()
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
   val = forth[w]
   if val ~= nil then return val end
   return nil
end

function forth.new(env)
   obj = { ds = {}, env = env, mixins = {} }
   setmetatable(obj, {__index = lookup })
   -- obj isn't always needed so pass it as second value
   return function(w) interpret(obj, w) end, obj
end

function forth:pop()
   local n = #self.ds
   local rv = self.ds[n]
   table.remove(self.ds, n)
   return rv
end

function forth:ps() log_desc(self.ds) end
function forth:p()  log_desc(self:pop()) end
local function op2(fun)
   return
      function(self)
         local b = self:pop()
         local a = self:pop()
         push(self, fun(a,b))
      end
end
forth['+'] = op2(function(a,b) return a+b end)
forth['-'] = op2(function(a,b) return a-b end)

local function line_words(line)
   if read then
      -- Optionally use the syntax implemented by read_cat.
      local tokens = read.tokenize(read.characters(line))
      local syntax = read.parse(read.elements(tokens))
      -- log_desc(syntax)
      -- The output syntax is quoted code. Unpack outer code quotation.
      assert(syntax and syntax.tag == 'code')
      return read.elements(syntax.code)
   else
      return line:gmatch("%S+")
   end
end

-- The line syntax is defined operationally, i.e. whatever this routine returns.
function forth.interpret_line(line, interpret)
   -- The base syntax is very simple: whitespace separates words.
   for w in line_words(line) do
      -- A word starting with the comment character causes the rest of
      -- the line to be ignored.  FIXME: Remove after integrating parser.
      if type(w) == 'string' and w:sub(1,1) == '#' then
         break
      end
      interpret(w)
   end
end

function forth.interpret_file(script, interpret)
   assert(script)
   -- Make sure it exists
   local f = io.open(script, "rb")
   if f == nil then
      error("Can't open command file '" .. script .. "'\n")
      exit(1)
   end
   f:close()
   -- Convert it to words.
   for line in io.lines(script) do
      forth.interpret_line(line, interpret)
   end
end

return forth
end
