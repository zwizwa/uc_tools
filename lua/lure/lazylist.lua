-- Lazy lists with eager heads and lazy tails.

--  The head is always strict.  The tail is delayed (memoized thunk).
local function memo(thunk)
   local cache = nil
   return function()
      if cache ~= nil then return cache end
      cache = thunk()
      return cache
   end
end

-- FIXME: How to parameterize this?
-- This is only for test_seq.lua direct implementation.
-- Signal metatable will be patched below to support Lua operator overloading.
local mt = {
}

local function signal(head, tail_thunk)
   s = { type = 'signal', head = head, tail = memo(tail_thunk) }
   setmetatable(s, mt)
   return s
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


local function take(n, lst)
   local rv = {}
   for i=1,n do
      table.insert(rv, lst.head)
      lst = lst.tail()
   end
   return rv
end


return {
   take = take,
   signal = signal,
   mt = mt,
   pure = pure,
   project = project,
   lift = lift,
}

