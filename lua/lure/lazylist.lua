-- Lazy lists with eager heads and lazy tails.

-- Parameterized module: metatable for operators.
return function(mt)

--  The head is always strict.  The tail is delayed (memoized thunk).
local function memo(thunk)
   local cache = nil
   return function()
      if cache ~= nil then return cache end
      cache = thunk()
      return cache
   end
end

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


-- To implement causal recursion, the update function is probed using
-- a signal that does not have a defined tail.  This works as long as
-- the tail of the s_out and out signals are never evaluated.  The
-- loop can then be recd using a state variable.

-- Implement base routine on state vectors (lua lists).
local function rec_vec(init, update)
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
local function rec(init, update)
   return rec_vec(
      {init},
      function(states)
         local next_state, out = update(states[1])
         return {next_state}, out
      end)
end



return {
   take = take,
   signal = signal,
   mt = mt,
   pure = pure,
   project = project,
   lift = lift,
   rec_vec = rec_vec,
   rec = rec,
}

end


