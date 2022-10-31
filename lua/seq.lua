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


-- In Haskell the 'close' operator can tuck away the state signals in
-- the monad.  In Lua lazy lists are used.

local function memo(thunk)
   local cache = nil
   return function()
      if cache ~= nil then return cache end
      cache = thunk()
      return cache
   end
end



local function pure(val)
   return {
      head=val,
      tail=memo(function() return pure(val) end)
   }
end
local function lift1(f)
   local fl
   fl = function(a)
      return {
         head=f(a.head),
         tail=memo(function() return fl(a.tail()) end)
      }
   end
   return fl
end
local function lift2(f)
   local fl
   fl = function(a,b)
      return {
         head=f(a.head,b.head),
         tail=memo(function() return fl(a.tail(),b.tail()) end)
      }
   end
   return fl
end

-- For probing we rely on the tail not being evaluated, so it can be
-- undefined.  This function is never called.
local function undefined_tail()
   error('undefined_tail')
end

local function close(init, update)
   local s = init
   local do_update
   do_update = function()
      s_prev = {head = s, tail = undefined_tail}
      local s_next, out = update(s_prev)
      s = s_next.head
      return {head = out.head, tail = memo(do_update)}
   end
   return do_update()
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

