-- Parse s-expressions
-- Keep it simple at first: no improper lists, no strings.

local se = {}

function se:next()
   local char = self.stream:read(1)
   assert(char)
   if char then
      self:log("next: " .. char .. "(" .. char:byte(1) .. ")\n")
   end
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
         return self:atom(table.concat(chars,""))
      end
      table.insert(chars, char)
      self:pop()
   end
end
function se:read_list()
   assert('(' == self:pop())
   local objs = {}
   while true do
      if ')' == self:skip_space() then
         self:pop()
         return self:list(objs)
      end
      local obj = self:read()
      table.insert(objs, obj)
   end
end
function se:read()
   if '(' == self:skip_space() then
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

