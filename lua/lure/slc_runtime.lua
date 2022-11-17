require('lure.log_se')
local se = require('lure.se')
local match = require('lure.match')
local a2l = se.array_to_list

local rt = {}

function rt.new(mod)
   local lib = {}
   lib['module-register!'] = function(k,v) mod[k] = v end
   lib['new'] = function() error "new suppressed" end
   setmetatable(lib, {__index = rt})
   return function(k) return lib[k] end
end

-- Tail recursion trampoline.
-- See scheme_macros.lua
-- FIXME: The wrapped recursive calls only work in tail position!
--        Compiler should probably guarantee that.
-- FIXME: Optimzation: the argument vector and tag wrapper could be
-- re-used to avoid allocation while the loop is running.

rt['letrec-trampoline'] = function(init, ...)
   -- log('letrec-trampoline\n')
   local bodies = {...}

   -- We get passed the function bodies of each of the recursive
   -- functions, and the body of the letrec expression.  Each of these
   -- functions is "open", i.e. parameterized by the names of the
   -- recursive functions.

   -- What we do here is act as a fix point combinator for this
   -- collection of functions.  The bodies are position-encoded.

   -- Example:

   -- (letrec ((check (lambda (n) (if (n > 0) 'done (inc n))))
   --          (inc   (lambda (n) (check (+ n 1)))))
   --   (check 0))
   --
   -- Which might be an expansion of
   --
   -- (begin
   --   (define (check n) (if (n > 0) 'done (inc n)))
   --   (define (inc n)   (check (+ n 1)))
   --   (check 0))
   --
   -- Is represend as
   --
   -- bodies[1] = (lambda (check inc) (lambda (n) (if (n > 0) 'done (inc n))))
   -- bodies[2] = (lambda (check inc) (lambda (n) (check (+ n 1))))
   -- init      = (lambda (check inc) (lambda (n) (check 0)))
   --
   -- Where the lambda forms are converted to Lua in the usual way.


   -- We implement this by binding the function names to functions
   -- that construct a data structure that represents a postponed
   -- call.  This is what happens on the 'inside' of the function
   -- bodies. These wrappers are then returned to the trampoline
   -- because we expect these calls to always be in tail position
   -- (compiler needs to check!). The wrapper contains the argument
   -- list plus tag data that allows us to refer back to the correct
   -- function.
   local inside = {}
   for i=1,#bodies do
      inside[i] = function(...)
         -- We just need a unique tag here.  Something that is not
         -- known by the function bodies.  The table object will do.
         return {inside, i, {...}}
      end
   end
   -- On the outside, we bind the function bodies to those
   -- wrapper-generating functions.
   local outside = {}
   for i=1,#bodies do
      outside[i] = bodies[i](unpack(inside))
   end

   -- Package the loop
   local function start(i0, nxt_args)
      local nxt = outside[i0]
      local rvs
      while true do
         rvs = {nxt(unpack(nxt_args))}
         -- log_desc({rvs=rvs})
         local rvs1 = rvs[1]
         if type(rvs1) ~= 'table' then break end
         local maybe_tag, i, args = unpack(rvs1)
         if maybe_tag ~= inside then break end
         -- Found encoded tail call.
         nxt = outside[i]
         nxt_args = args
      end
      return unpack(rvs)
   end

   -- The loop is engaged when the init function calls one of the
   -- recursive functions.  So we parameterize init with another
   -- wrapper to start the loop.
   local starts = {}
   for i=1,#bodies do
      starts[i] = function(...) return start(i, {...}) end
   end
   return init(unpack(starts))()
end

local function vector_iolist(v)
   local l = a2l(v)
   return {'#', se.iolist(l)}
end

rt['vector'] = function(...)
   local v = {...}
   -- Vectors need to be tagged so they can be distinguished from
   -- pairs, which are not tagged.  Still not sure if this is the
   -- right way to go.  Maybe better to tag the cons pairs instead and
   -- reserve the basic Lua array datastructure to be primitive?  In
   -- any case, {a,b} is used everywhere to represent pairs.
   v.class = 'vector'
   v.iolist = vector_iolist
   return v
end

rt['make-vector'] = function(n, init)
   init = init or 0
   local v = {}
   for i=1,n do v[i] = init end
   return v
end

-- FIXME: Char is represented by number

rt['string-ref'] = function(str, i)
   local b = str:byte(i+1)
   -- log_desc({b=b})
   return b
end
rt['char->integer'] = function(char)
   return char
end
rt['integer->char'] = function(char)
   return char
end
rt['list->string'] = function(lst)
   local arr = {}
   for el in se.elements(lst) do
      table.insert(arr, string.char(el))
   end
   return table.concat(arr)
end
rt['string->symbol'] = function(str)
   return str
end
rt['char?'] = function(char)
   return type(char) == 'number'
end
rt['string-length'] = function(str)
   return #str
end


rt['vector-ref'] = function(vec, i)
   return vec[i+1]
end
rt['vector-set!'] = function(vec, i, val)
   vec[i+1] = val
end

-- FIXME: These need review + documentation as Scheme is weird about equality.
local function lua_eqp(a, b) return a == b end
rt['eq?']    = lua_eqp
rt['eqv?']   = lua_eqp
rt['=']      = lua_eqp
rt['equal?'] = se.equalp


rt['vector?'] = function(a)
   return type(a) == 'table'
end
rt['read-char'] = function()
   local c = io.stdin:read(1)
   return c:byte(1)
end
rt['write-char'] = function(c)
   return io.stdout:write(string.char(c))
end
rt['table-ref'] = function(tab, key)
   return tab[key]
end
rt['table-set!'] = function(tab, key, val)
   tab[key] = val
end
rt['table'] = function(assoc_list)
   local tab = {}
   for rec in se.elements(assoc_list) do
      local key, val = unpack(rec)
      tab[key] = val
   end
   return tab
end
rt['compile-qq-pattern'] = function(pat)
   local cns = se.constructor(pat)
   return match.compile(cns)
end

rt['match-qq-patterns'] = function(expr, clauses)
   for clause in se.elements(clauses) do
      local pat, handle = unpack(clause)
      local m = match.apply(pat, expr)
      if m then
         return handle(m)
      end
   end
   assert('no-match')
end
rt['log-se'] = function(se)
   log_se(se)
end
rt['log-se-n'] = function(se, tag)
   log_se_n(se, tag)
end
rt['assert'] = assert

local function import_from(other_mod, names)
   for _,name in ipairs(names) do
      rt[name] = other_mod[name]
   end
end
import_from(se, {'cons','list','car','cdr','cadr','reverse','map','zip','foldr','foldl','length'})

-- Compiler will generate code for these when applied, but we still
-- need to be able to reference the functions as values.
rt['+'] = function(a,b) return a + b end
rt['-'] = function(a,b) return a - b end
rt['*'] = function(a,b) return a * b end
rt['/'] = function(a,b) return a / b end
rt['>'] = function(a,b) return a > b end
rt['<'] = function(a,b) return a < b end
rt['quotient'] = function(a,b) return math.floor(a / b) end -- FIXME: verify this
rt['not'] = function(a) return not a end

-- (define p (make-parameter 'init_val))
-- (p) -> 'init_val
-- (p 'new_val) (p) -> 'new_val
-- See 'parameterize' in scheme_macros.lua
rt['make-parameter'] = function(init_val)
   local val = init_val
   return function(new_val)
      if new_val ~= nil then val = new_val end
      return val
   end
end

rt['false'] = false
rt['true']  = true

rt['void'] = function() end

rt['mark'] = function(tag, ...)
   log_se_n({tag,a2l({...})},"MARK:")
end

-- Stubs for interpreting scheme_sm mapped back to scheme.
--rt['hint'] = function() end
--local args = {}
--rt['alloc_args'] = function() end
--rt['arg-ref'] = function(n) return args[n+1] end
--rt['set-arg!'] = function(n,v) args[n+1]=v end



return rt


