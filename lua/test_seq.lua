#!./lua.sh

-- SYNTAX

-- The first argument contains the library of signal operations.
-- The signals support overloading for arithmetic operations.
-- Library functions will auto-lift numbers to signals.

require 'lure.log'

-- FIXME: curry the context argument.
-- FIXME: in should be a single argument

local function prog1(c, i)
   local function counter_sm(s) return s+1, s end
   local counter = c.close(0, counter_sm)
   return counter + i
end

local function prog2(c)
   function update(s) return c.add1(s), s end
   return c.close(0, update)
end

local function prog3(c, a)
   return c.add1(a)
end

local function prog4(c, a, b)
   return c.add(a, b)
end

local function prog5(c, a)
   return a + 1
end


-- The idea is to generate C array processing code.  The iteration
-- procedure is based around the idea of an array constructor with
-- self-reference for old values (triangle).
--
-- Before doing any of this, first make sure the compiler produces
-- code that can place return values directly into arrays etc...
-- CPS-style.  That seems to be the main property that's needed to
-- then generalize to array initializers as state machine unfolds.
--
-- Important to realize here is that the language can just be
-- applicative, and the Lua model can be as well, but the
-- implementation can use "parameterized output" or CPS-like
-- representation to pass destination location into the correct spot.
--
-- This kind of thing has already been implemented in lure, so maybe
-- it is best to switch over to that.  The main point of this file
-- here is:
-- 1. Lua can serve as HOAS for a causal signal/RTL language
-- 2. Representation as lazy lists works




-- local function prog6(c)
--    local arr =
--       c.array(
--          3, -- size of the array
--          function(s, i, ref)
--             -- i is the current index
--             -- ref allows access to previous results 0 to i-1
--             -- return values are state and output
--             -- output is stored in the array
--             local s_next = s + 1
--             local out = s + i
--             return s_next, out
--          end)
--    return arr
-- end


-- SEMANTICS: SIGNALS AS LAZY LISTS

-- Use lazy lists to represent signals.  The head is always strict.
-- The tail is delayed (memoized thunk).
local function memo(thunk)
   local cache = nil
   return function()
      if cache ~= nil then return cache end
      cache = thunk()
      return cache
   end
end

-- Signal metatable will be patched below to support Lua operator overloading.
local signal_mt = { }
local function signal(head, tail_thunk)
   s = { type = 'signal', head = head, tail = memo(tail_thunk) }
   setmetatable(s, signal_mt)
   return s
end
local function take(n, lst)
   local rv = {}
   for i=1,n do
      table.insert(rv, lst.head)
      lst = lst.tail()
   end
   return rv
end

-- Constant signals
local function pure(val)
   return signal(val, function() return pure(val) end)
end

-- Project prim x signal -> signal
local function project(thing)
   if type(thing) == 'table' and thing.type == 'signal' then return thing end
   return pure(thing)
end

-- Lift an n-argument lua function to a lazy list operation
local function lift(n,f)
   local fl
   fl = function(...)
      local args = {...}
      assert(n == #args)
      local heads = {}
      local tails = {}
      for i,a in ipairs(args) do
         local sig_a = project(a)
         heads[i] = sig_a.head
         tails[i] = sig_a.tail
      end
      return signal(
         f(unpack(heads)),
         function()
            local app_tails = {}
            for i,tail in ipairs(tails) do app_tails[i] = (tails[i])() end
            return fl(unpack(app_tails))
         end)
   end
   return fl
end


-- To implement 'close', the update function is probed using a signal
-- that does not have a defined tail.  This works as long as the tail
-- of the s_out and out signals are never evaluated.  The loop can
-- then be closed using a state variable.

-- Implement base routine on state vectors (lua lists).
local function close_vec(init, update)
   local state = init
   local tail_thunk
   tail_thunk = function()
      local s_in = {}
      for i=1,#state do
         s_in[i] = signal(state[i], nil)
      end
      local s_out, out = update(s_in)
      for i,si in ipairs(s_out) do
         state[i] = si.head
      end
      local rv = signal(out.head, tail_thunk)
      return rv
   end
   return tail_thunk()
end

-- Wrapper for single state variable.
local function close(init, update)
   return close_vec(
      {init},
      function(states)
         local next_state, out = update(states[1])
         return {next_state}, out
      end)
end


local signal_c = {
   add1  = lift(1, function(a)   return a+1 end),
   add   = lift(2, function(a,b) return a+b end),
   close = close
}

signal_mt.__add = signal_c.add

local f1 = prog1(signal_c, pure(100))
local f2 = prog2(signal_c)

-- log_desc({
--       one = c.one,
--       add1_one = c.add1(c.one),
--       one_tail = c.one.tail(),
--       add1_one_tail = c.add1(c.one).tail(),
--       f2 = f2,
--       f2_tail = f2.tail(),
--       f2_tail_tail = f2.tail().tail(),
--       f1 = f1,
--       f1_tail = f1.tail(),
-- })

log_desc({
      f1 = take(3,f1),
      f2 = take(3,f2)
})

local function counter(n)
   return function()
      local val = n
      n = n + 1
      return val
   end
end


-- SEMANTICS: COMPILER

-- Erlang's iolist
local function w_thing(thing)
   if type(thing) == 'table' then
      for _, subthing in ipairs(thing) do
         w_thing(subthing)
      end
   else
      io.stdout:write(thing .. "")
   end
end
local function w(...) w_thing({...}) end

local function compile(prog, nb_input)
   local code = {}
   local new_var = counter(0)
   local signal_mt = {}
   local function signal(...)
      local rep = {...}
      rep.type = 'signal'
      setmetatable(rep, signal_mt)
      return rep
   end
   local function project(thing)
      if type(thing) == 'table' and thing.type == 'signal' then return thing end
      return signal('lit', thing)
   end

   local function app(op, args_)
      local var = new_var()
      local args = {}
      for i,a in ipairs(args_) do args[i] = project(a, lit) end
      table.insert(code, {'let', var, {op, args}})
      return signal('ref',var)
   end
   local function prim(op, n)
      return function(...)
         local args = {...}
         assert(#args == n)
         return app(op, args)
      end
   end
   local new_state = counter(0)
   local init_state = {}
   local function close(init, fun)
      local svar = new_state()
      init_state[svar+1] = init
      local sin = signal('sref',svar)
      local sout, out = fun(sin)
      table.insert(code, {'set-state!', svar, sout})
      return out
   end
   local c = {
      add  = prim('add',2),
      add1 = prim('add1',1),
      close = close,
   }
   signal_mt.__add = c.add

   local args = {c}
   for i=1,nb_input do
      table.insert(args, signal('input',i-1))
   end
   local out = { prog(unpack(args)) }

   -- log_desc({code=code,out=out,init=init_state,nb_input=nb_input})
   local function w_code(w, code)
      if type(code) == 'table' then
         w('( ')
         for _,c in ipairs(code) do
            w_code(w, c)
            w(' ')
         end
         w(')')
      else
         w(code)
      end
   end
   w("\n")
   w("input:\n")
   log_desc({init_state=init_state,nb_input=nb_input})
   w("code:\n")
   for _,c in ipairs(code) do
      w_code(w, c)
      w('\n')
   end
   w("output:\n")
   for _,c in ipairs(out) do
      w_code(w, c)
      w('\n')
   end

   return code
end

compile(prog3, 1)
compile(prog4, 2)
compile(prog5, 1)
compile(prog2, 1)
compile(prog1, 1)




-- TODO:
-- . C code generator
-- . Lua (Lure) code generator for LuaJIT
-- . Combinators

