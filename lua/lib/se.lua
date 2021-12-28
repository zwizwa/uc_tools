-- Parse s-expressions
-- Keep it simple at first: no improper lists, no strings.

local se = {}

function se:next()
   local char = self.stream:read(1)
   if not char then
      error("unexpected EOF")
   end
   -- self:log("next: " .. char .. "(" .. char:byte(1) .. ")\n")
   return char
end
function se:peek()
   if self.head then return self.head end
   while true do
      self.head = self:next()
      if self.head ~= ';' then return self.head end
      -- Skip comments
      while '\n' ~= self:next() do
      end
   end
end
function se:pop()
   local char = self:peek()
   self.head = nil
   return char
end
local whitespace = {[' '] = true, ['\n'] = true, ['\r'] = true, ['\t'] = true}
local function is_whitespace(str)
   return whitespace[str] or false
end
function se:skip_space()
   while true do
      local char = self:peek()
      if not is_whitespace(char) then return char end
      self:pop()
   end
end
function se:read_atom()
   local chars = {}
   while true do
      local char = self:peek()
      if is_whitespace(char) or '(' == char or ')' == char or nil == char then
         local str = table.concat(chars,"")
         local num = tonumber(str)
         return num or str
      end
      table.insert(chars, char)
      self:pop()
   end
end
function se.list(...)
   return se.array_to_list({...})
end
function se.array_to_list(arr)
   local lst = {}
   for i=#arr,1,-1 do
      lst = {arr[i], lst}
   end
   return lst
end
function se.is_pair(x)
   return type(x) == 'table' and (#x == 2)
end
function se.elements(lst)
   local pair = lst
   return function()
      if pair then
         local el, rest = unpack(pair)
         pair = rest
         -- It's very convenient to also return the tail of the list.
         return el, pair
      end
   end
end
function se.array(lst)
   local arr = {}
   for el in se.elements(lst) do
      table.insert(arr, el)
   end
   return arr
end
function se.length(lst)
   local n = 0
   for el in se.elements(lst) do
      n = n + 1
   end
   return n
end
function se.car(pair)
   assert(type(pair) == 'table')
   return pair[1]
end
function se.cdr(pair)
   assert(type(pair) == 'table')
   return pair[2]
end
function se.cons(car, cdr)
   return {car, cdr}
end
-- It might be simpler to do something like se.unpack to fit better in
-- the language.  Functions are a little annoying.
function se.unpack_array(expr, config, body)
   assert(type(config) == 'table')
   local len = se.length(expr)
   if not config.tail then
      -- Do strict match by default.
      assert(config.n)
      assert(len == config.n)
   else
      if not config.n then config.n = len end
      if config.n > len then config.n = len end
   end
   local args = {}
   for i=1,config.n do
      table.insert(args, se.car(expr))
      expr = se.cdr(expr)
   end
   table.insert(args, expr) -- tail
   return args
end
function se.unpack(expr, config)
   local args = se.unpack_array(expr, config, body)
   return unpack(args)
end
function se.match(expr, config, body)
   local args = se.unpack_array(expr, config, body)
   return body(unpack(args))
end

function se:read_list()
   local objs = {}
   while true do
      if ')' == self:skip_space() then
         self:pop()
         -- For processing it is much more convenient to represent
         -- this as a list of pairs instead of an array.
         -- return objs
         return self.array_to_list(objs)
      end
      local obj = self:read()
      table.insert(objs, obj)
   end
end
function se:read()
   if '(' == self:skip_space() then
      self:pop()
      return self:read_list()
   else
      return self:read_atom()
   end
end

function se.new(stream)
   assert(stream)
   local obj = { stream = stream }
   setmetatable(obj, { __index = se })
   return obj
end

return se

