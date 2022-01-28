-- slc.lua does not support constant-space tail calls.
-- to work around this we do implement named let using a trampoline.
--

-- example:
--
-- (let loop ((n 0) (a 2))
--   (if (> n 3) a (loop (+ n 1) (* a a))))
--
local function test()
   -- The 'loop' symbol is bound to a wrapper so it behaves just like
   -- a function.
   local done
   local n1 = 0
   local a1 = 2
   local function loop(n, a)
      done = false
      n1 = n
      a1 = a
      return
   end
   -- The loop body is given a hidden name, but it otherwise directly
   -- implements the code.
   local function loop_body(n, a)
      log_desc({n=n,a=a})
      if n > 3 then
         return a
      else
         return loop(n + 1, a * a)
      end
   end
   -- The trampoline
   while true do
      done = true
      local rvs = {loop_body(n1, a1)}
      if done then return unpack(rvs) end
   end
end


require('lib.log')
log('rv = ' .. test() .. '\n')
