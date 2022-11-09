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


-- Going to admit, this took me a while to figure out how to do
-- elegantly, but after fumbling about, the principle is really always
-- the same:

-- Circular structures are possible by 1. declaring still-undefined
-- variables and binding to them inside thunks, and 2. patching those
-- variables before those thunks are evaluated.  The cycle below is in
-- state_next variable, which is referenced in the tail of state_sigs,
-- and defined later after passing state_sigs to update.

-- state_init_vals is a table of initial values (not signals!)
--
-- update function takes input state signal, produces next state and
-- output signals.
--
local function rec_vec(state_init_vals, update)
   local state_next -- table of signals
   local out -- arbitrary output we just pass on
   local state = {}
   for key in pairs(state_init_vals) do
      state[key] = signal(
         state_init_vals[key],
         function() return state_next[key] end)
   end
   state_next, out = update(state)
   return out
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


