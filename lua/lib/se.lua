-- Parse s-expressions
-- Keep it simple at first: no improper lists, no strings.

-- Pairs are Lua's 2-element arrays
-- The empty list is Lua's nil

-- FIXME: That is a mistake! It makes it impossible to put nil in an
-- array and then iterate over the elements.  Use e.g. '#<nil>' to
-- represent the empty list.

local empty = '#<nil>'

local se = { empty = empty }

function se:next()
   local char = self.stream:read(1)
   if char == '\n' then
      self.nb_newlines = self.nb_newlines + 1
   end
   if not char then
      error({error="EOF"})
   end
   -- self:log("next: " .. char .. "(" .. char:byte(1) .. ")\n")
   return char
end

function se:skip_line()
   while '\n' ~= self:next() do end
end

function se:peek()
   if self.head then return self.head end
   while true do
      self.head = self:next()
      -- On the first line, '#' is also a comment character.
      -- This is there for #! and Racket #lang forms.
      if self.head == '#' and self.nb_newlines < 1 then
         self.head = ';'
      end
      if self.head ~= ';' then return self.head end
      self:skip_line() -- Skip comment
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
-- Note that this does not work well if the array contains an empty
-- list, which is represented by Lua nil.
function se.array_to_list(arr)
   local lst = empty
   for i=#arr,1,-1 do
      local el = arr[i]
      lst = {el, lst}
   end
   return lst
end
function se.list_to_array(lst)
   local arr = {}
   for el in se.elements(lst) do
      table.insert(arr, el)
   end
   return arr
end

-- Same as Scheme (list ...)
function se.list(...)
   return se.array_to_list({...})
end
function se.elements(lst)
   local l = lst
   return function()
      assert(l)
      if l ~= empty then
         if type(l) ~= 'table' then
            error('bad list pair: ' .. type(l))
         end
         local el, rest = unpack(l)
         l = rest
         -- It's very convenient to also return the tail of the list.
         return el, l
      end
   end
end
function se.reverse(lst)
   local l = empty
   for el in se.elements(lst) do
      l = {el, l}
   end
   return l
end

-- FIXME: Test
function se.map_to_array(fun, lst)
   local arr = {}
   for el in se.elements(lst) do
      table.insert(arr, fun(el))
   end
   return arr
end
function se.map(fun, lst)
   return se.array_to_list(se.map_to_array(fun,lst))
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
function se.is_empty(lst)
   return lst == empty
end
function se.append(a, b)
   for el in se.elements(se.reverse(a)) do
      b = {el, b}
   end
   return b
end

function se.iolist(expr)
   if expr ~= empty and type(expr) ~= 'table' then
      return expr
   elseif type(expr) == 'table' and expr.class then
      if expr.iolist then
         return expr.iolist(expr)
      else
         return {'#<',expr.class,'>'}
      end
   else
      local iol = {"("}
      for el, rest in se.elements(expr) do
         table.insert(iol, se.iolist(el))
         if not se.is_empty(rest) then
            table.insert(iol, " ")
         end
      end
      table.insert(iol, ")")
      return iol
   end
end



-- It might be simpler to do something like se.unpack to fit better in
-- the language.  Functions are a little annoying.
function se.unpack_array(expr, config, body)
   assert(type(config) == 'table')
   local len = se.length(expr)
   if not config.tail then
      -- Do strict match by default.
      assert(config.n)
      if len ~= config.n then
         if config.no_match then
            return config.no_match(expr)
         end
         error('bad_match, got:' .. len .. ", expected:" .. config.n)
      end
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

-- Read multiple expressions, gather them in a list.
function se:read_multi()
   local exprs = {}
   while true do
      local ok, err = pcall(function() return self:skip_space() end)
      if not ok then
         assert(err and err.error == 'EOF')
         break
      end
      table.insert(exprs, self:read())
   end
   return se.array_to_list(exprs)
end

function se.string_to_stream(str)
   assert(str and type(str) == 'string')
   local n = 1
   local obj = {}
   function obj.read(self)
      -- log_desc({n=n,str=str})
      if n <= #str then
         local rv = str:sub(n,n)
         n = n + 1
         return rv
      end
   end
   return obj
end

function se.new(stream)
   assert(stream)
   local obj = { stream = stream, nb_newlines = 0 }
   setmetatable(obj, { __index = se })
   return obj
end

function se.read_file_multi(filename)
   assert(filename and type(filename) == 'string')
   local stream = io.open(filename,"r") or error("Can't open '" .. filename .. "'")
   local parser = se.new(stream)
   parser.log = function(self, str) io.stderr:write(str) end
   local exprs = parser:read_multi()
   stream:close()
   return exprs
end



return se

