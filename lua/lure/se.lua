-- Parse s-expressions
-- Keep it simple at first: no improper lists, no strings.

-- Pairs are Lua's 2-element arrays
-- The empty list is represented by '#<nil>'

-- require('lure.log')


-- LIBRARY

local tab = require('lure.tab')

local empty = '#<nil>'

local se = { empty = empty }

-- Represent EOF as a thing, not nil.
local EOF = { "<EOF>" }


local function assert_pair(pair) assert(type(pair) == 'table') end
local function car(pair) assert_pair(pair); return pair[1] end; se.car = car
local function cdr(pair) assert_pair(pair); return pair[2] end; se.cdr = cdr
local function cadr(ppair) return car(cdr(ppair)) end; se.cadr = cadr
local function caar(ppair) return car(car(ppair)) end; se.caar = caar
local function cdar(ppair) return cdr(car(ppair)) end; se.cdar = cdar
local function cons(a, d) return {a, d} end; se.cons = cons
local function is_empty(lst) return lst == empty end; se.is_empty = is_empty

-- Note that this does not work well if the array contains an empty
-- list, which is represented by Lua nil.
local function array_to_list(arr)
   local lst = empty
   for i=#arr,1,-1 do
      local el = arr[i]
      lst = {el, lst}
   end
   return lst
end
se.array_to_list = array_to_list


-- Same as Scheme (list ...)
local function list(...) return array_to_list({...}) end; se.list = list


local function list_to_array(lst)
   local arr = {}
   for el in se.elements(lst) do
      table.insert(arr, el)
   end
   return arr
end
se.list_to_array = list_to_array

local function elements(lst)
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
se.elements = elements

local function reverse(lst)
   local l = empty
   for el in elements(lst) do
      l = {el, l}
   end
   return l
end
se.reverse = reverse


-- FIXME: avoid array
local function take(iter, nb)
   return array_to_list(map.take(iter), nb)
end

-- FIXME: avoid array
local function map_to_array(fun, lst)
   local arr = {}
   for el in elements(lst) do
      local single_val = fun(el)
      table.insert(arr, single_val)
   end
   return arr
end
local function map(fun, lst)
   assert(lst)
   return array_to_list(map_to_array(fun,lst))
end
se.map = map


-- FIXME: generalize map, zip, ...
local function zip_to_array(fun, lst_a, lst_b)
   local arr = {}
   for el in elements(lst_a) do
      local single_val = fun(el, car(lst_b))
      lst_b = cdr(lst_b)
      table.insert(arr, single_val)
   end
   return arr
end
local function zip(fun, lst_a, lst_b)
   assert(lst_a)
   assert(lst_b)
   return array_to_list(zip_to_array(fun,lst_a,lst_b))
end
se.zip = zip


local function foldr(on_pair, on_empty, lst)
   local function foldr(lst)
      if is_empty(lst) then
         return on_empty
      else
         return on_pair(car(lst), foldr(cdr(lst)))
      end
   end
   return foldr(lst)
end
se.foldr = foldr

local function foldl(update, state, lst)
   while not is_empty(lst) do
      state = update(car(lst), state)
      lst = cdr(lst)
   end
   return state
end
se.foldl = foldl

local function array(lst)
   local arr = {}
   for el in elements(lst) do
      table.insert(arr, el)
   end
   return arr
end
se.array = array

local function length(lst)
   local n = 0
   for el in elements(lst) do
      n = n + 1
   end
   return n
end
se.length = length

-- FIXME: avoid reverse
local function append(a, b)
   for el in elements(reverse(a)) do
      b = {el, b}
   end
   return b
end
se.append = append


local function equalp(a, b)
   local ta = type(a)
   local tb = type(b)
   if ta ~= tb then return false end
   if ta ~= 'table' then return a == b end
   if ta.class then
      -- FIXME this is not implemented.
      assert(ta.class.equal)
      return ta.class.equal(a, b)
   end
   -- It is assumed these are pairs
   return equalp(a[1],b[1]) and equalp(a[2],b[2])
end
se.equalp = equalp


-- Use a tagged s-expression as a stack.
local function push_cdr(el, lst)
   local tail = cdr(lst)
   lst[2] = {el, tail}
end
se.push_cdr = push_cdr



-- READER

function se:next()
   local char = self.stream:read(1)
   if not char then
      return EOF
   end
   -- log(char)
   if char == '\n' then
      self.nb_newlines = self.nb_newlines + 1
   end
   -- self:log("next: " .. char .. "(" .. char:byte(1) .. ")\n")
   return char
end

function se:skip_line()
   while true do
      local c = self:next()
      if c == '\n' or c == EOF then return end
   end
end

function se:peek()
   if self.head then return self.head end
   while true do
      self.head = self:next()
      if self.head == EOF then return EOF end
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
local is_end_of_atom = is_charset(' \n\r\t' .. "()'`,.")

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
      if is_end_of_atom(char) or EOF == char then
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

function se.iolist(expr)
   local typ = type(expr)
   if typ == 'function' then
      return '#<function>'
   elseif expr ~= empty and typ ~= 'table' then
      return expr
   elseif typ == 'table' and expr.class then
      if expr.iolist then
         if type(expr.iolist) == 'function' then
            return expr.iolist(expr)
         else
            return expr.iolist
         end
      else
         return {'#<',expr.class,'>'}
      end
   else
      local iol = {"("}
      for el, rest in elements(expr) do
         table.insert(iol, se.iolist(el))
         if not is_empty(rest) then
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
      table.insert(args, car(expr))
      expr = cdr(expr)
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
      local c = self:skip_space()
      if '.' == c then
         self:pop()
         local tail_obj = self:read()
         assert(')' == self:skip_space())
         return self.append(self.array_to_list(objs), tail_obj)
      elseif ')' == c then
         self:pop()
         -- For processing it is much more convenient to represent
         -- this as a list of pairs instead of an array.
         -- return objs
         return self.array_to_list(objs)
      elseif EOF == c then
         error("missing ')'")
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
      return list(tag, self:read())
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
      local char = self:skip_space()
      if char == EOF then break end
      table.insert(exprs, self:read())
   end
   return array_to_list(exprs)
end

function se.string_to_stream(str)
   assert(str and type(str) == 'string')
   local n = 1
   local obj = {}
   function obj.close(self)
   end
   function obj.read(self, nb_bytes)
      -- simple wrapper, only supports character read
      assert(nb_bytes == 1)
      -- log_desc({n=n,str=str})
      if n <= #str then
         local rv = str:sub(n,n)
         n = n + 1
         return rv
      end
   end
   return obj
end

-- We assume the following: Cons pair's are "naked".  This allows
-- usinging Lua's {,} constructor.  To distiguish these from other
-- objects represented as tables, we require those to have .class
-- variable defined.
function se.expr_type(e)
   local typ = type(e)
   if typ == 'table' then
      if e[1] ~= nil and e[2] ~= nil then
         return 'pair'
      else
         if e.class then
            return e.class
         else
            log_desc(e)
            error('bad table')
         end
      end
   end
   return typ
end
function se.is_pair(e)
   return 'pair' == se.expr_type(e)
end

function se.new(stream)
   assert(stream)
   local obj = { stream = stream, nb_newlines = 0 }
   setmetatable(obj, { __index = se })
   return obj
end

function se.read_stream_multi_and_close(stream)
   local parser = se.new(stream)
   parser.log = function(self, str) io.stderr:write(str) end
   local exprs = parser:read_multi()
   stream:close()
   return exprs
end

function se.read_file_multi(filename)
   assert(filename and type(filename) == 'string')
   local stream = io.open(filename,"r") or error("Can't open '" .. filename .. "'")
   return se.read_stream_multi_and_close(stream)
end

function se.read_string_multi(str)
   local stream = se.string_to_stream(str)
   return se.read_stream_multi_and_close(stream)
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
   if type(expr) ~= 'table' then
      return expr
   end
   if se.is_expr(expr, 'unquote') then
      local _, var = se.unpack(expr, {n=2})
      local val = env[var]
      if not val then
         error("qq_eval undefined variable '" .. var .. "'")
      end
      return val
   end
   local function sub(expr1)
      return se.qq_eval(env, expr1)
   end
   -- return map(sub, expr)
   -- This needs to operate on pairs to allow unquoted tails.
   return {sub(expr[1]), sub(expr[2])}
end


-- Constructors are used for pattern matching.
--
-- They can be represented as strings, functions or s-expressions.
-- Only for matching pattern as function the name can not be recovered
-- so won't work for defmacro.
function se.constructor(thing)
   local expr
   local cons
   local t = type(thing)
   if t == 'function' then
      cons = thing
   else
      if t == 'string' then
         expr = se.read_string(thing)
      else
         expr = thing
      end
      cons = function(probe)
         local e = se.qq_eval(probe, expr)
         return e
      end
   end
   return cons
end


-- Map over a particular data type contained in an s-expression.
-- I.e. turn an s-expression into a functor.
function fmap(class, fun, expr)
   local function map(expr)
      local typ = type(expr)
      if typ ~= 'table' then
         return expr
      elseif nil == expr.class then
         -- Pair
         return {map(expr[1]), map(expr[2])}
      elseif class == expr.class then
         return fun(expr)
      else
         return expr
      end
   end
   return map(expr)
end
se.fmap = fmap


return se

