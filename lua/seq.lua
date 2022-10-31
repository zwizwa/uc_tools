#!./lua.sh

-- Core idea in Seq is closing over register feedback.

require 'lure.log'

local function prog1(c, i)
   local function counter_sm(s)
      return c.add(s, c.one), s
   end
   local counter = c.close(0, counter_sm)
   return c.add(counter, i)
end

local function prog2(c)
   function update(s) return c.add1(s), s end
   return c.close(0, update)
end


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
local function list(head, tail_thunk)
   return { head = head, tail = memo(tail_thunk) }
end

local function pure(val)
   return list(val, function() return pure(val) end)
end
local function lift1(f)
   local fl
   fl = function(a)
      return list(f(a.head),
                  function() return fl(a.tail()) end)
   end
   return fl
end
local function lift2(f)
   local fl
   fl = function(a,b)
      return list(f(a.head,b.head),
                  function() return fl(a.tail(),b.tail()) end)
   end
   return fl
end


-- To implement 'close', the update function is probed using a signal
-- that does not have a defined tail.  This works as long as the tail
-- of the s_next and out results are never evaluated.  The loop can
-- then be closed using a state variable.
local function close(init, update)
   local state = init
   local tail_thunk
   tail_thunk = function()
      s_prev = {head = state, tail = function() error('undefined_tail') end}
      local s_next, out = update(s_prev)
      state = s_next.head
      return list(out.head, tail_thunk)
   end
   return tail_thunk()
end


local c = {
   one   = pure(1),
   add1  = lift1(function(a) return a+1 end),
   add   = lift2(function(a,b) return a+b end),
   close = close
}

local f1 = prog1(c, pure(100))
local f2 = prog2(c)
log_desc({
      c.one,
      c.add1(c.one),
      c.one.tail(),
      c.add1(c.one).tail(),
      f2,
      f2.tail(),
      f2.tail().tail(),
      f1,
      f1.tail(),
})

