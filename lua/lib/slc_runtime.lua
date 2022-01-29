local rt = {}

-- See test_slc.lua
function rt.named_let(state, body)
   -- The named let symbol is bound to a wrapper so it behaves just
   -- like an ordinary function.
   local done
   local once = body(function(...) state = {...} ; done = false end)

   -- The trampoline
   while true do
      done = true
      local rvs = {once(unpack(state))}
      if done then return unpack(rvs) end
   end
end

function rt.vector(...)
   return {...}
end


