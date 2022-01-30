-- Parse s-expressions
-- Keep it simple at first: no improper lists, no strings.

-- Pairs are Lua's 2-element arrays
-- The empty list is represented by '#<nil>'

-- require('lib.log')

local empty = '#<nil>'

local se = { empty = empty }

function se:next()
   local char = self.stream:read(1)
   -- if char then log(char) end
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

local function charset(str)
   local set = {}
   for i=1,#str do
      local c = str:sub(i,i)
      set[c] = true
   end
   return set
end
local function is_charset(str)
   local set = charset(str)
   return function(c) return set[c] or false end
end
local is_whitespace  = is_charset(' \n\r\t')
local is_end_of_atom = is_charset(' \n\r\t' .. "()'`,")

function se:skip_space()
   while true do
      local char = self:peek()
      if not is_whitespace(char) then return char end
      self:pop()
   end
end

se.const = {
   ['#f'] = 'false',
   ['#t'] = 'true',
}

function se:read_atom()
   local chars = {}
   while true do
      local char = self:peek()
      if is_end_of_atom(char) or nil == char then
         local str = table.concat(chars,"")
         local const = se.const[str]
         if const then return const end
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
   assert(lst)
   local l = lst
   return function()
      if l ~= empty then
         if type(l) ~= 'table' then
            log_desc({bad_list = lst})
            if type(l) == 'string' then
               error('bad list pair: ' .. l)
            else
               error('bad list pair: type=' .. type(l))
            end
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
      local single_val = fun(el)
      table.insert(arr, single_val)
   end
   return arr
end
function se.map(fun, lst)
   assert(lst)
   return se.array_to_list(se.map_to_array(fun,lst))
end
function se.foldr(on_pair, on_empty, lst)
   local function foldr(lst)
      if se.is_empty(lst) then
         return on_empty
      else
         return on_pair(se.car(lst), foldr(se.cdr(lst)))
      end
   end
   return foldr(lst)
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
function se.cadr(ppair)
   return se.car(se.cdr(ppair))
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

se.ticks = {
   ["'"] = 'quote',
   [","] = 'unquote',
   ["`"] = 'quasiquote',
}

function se:read()
   local function tagged(tag)
      self:pop()
      return se.list(tag, self:read())
   end
   local c = self:skip_space()
   local tick = self.ticks[c]
   if (tick) then
      return tagged(tick)
   elseif '(' == c then
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
      local function skip_space() self:skip_space() end
      local ok, err = pcall(skip_space)
      if not ok then
         if (err and err.error == 'EOF') then break end
         error(err)
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

function se.expr_type(e)
   local typ = type(e)
   if typ == 'table' then
      if e[1] ~= nil and e[2] ~= nil then
         return 'pair'
      else
         log_desc(e)
         error('bad table')
      end
   end
   return typ
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

function se.read_string(str)
   local stream = se.string_to_stream(str)
   local parse = se.new(stream)
   local expr = parse:read(str)
   return expr
end

function se.is_expr(expr, tag)
   if type(expr) ~= 'table' then return false end
   if not tag then return true end
   return expr[1] == tag
end

-- Note: expr does not have the quasiquote.
function se.qq_eval(env, expr)
   if type(expr) ~= table then
      return expr
   end
   if se.is_expr(expr, 'unquote') then
      local _, var = se.unpack(expr, {n=2})
      local val = env[var]
      assert(val)
      return val
   end
   return se.map(
      function(expr1)
         return se.qq_eval(env, expr1)
      end,
      expr)
end


return se

