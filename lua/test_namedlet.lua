-- slc.lua does not support constant-space tail calls.
-- to work around this we do implement named let using a trampoline.
--
-- note that this only works when the function defined by named let is
-- in tail position

-- example:
--
-- (define (test)
--   (let squares ((n 0) (a 2))
--     (if (> n 3) a (squares (+ n 1) (* a a)))))
--

local function named_let_loop(state, body)
   -- The 'loop' symbol is bound to a wrapper so it behaves just like
   -- a function.
   local done
   local once = body(
      function(...)
         state = {...}
         done = false
         return
      end)

   -- The trampoline
   while true do
      done = true
      local rvs = {once(unpack(state))}
      if done then return unpack(rvs) end
   end
end

local function test()
   return named_let_loop(
      {0, 2},
      function(squares)
         return function(n, a)
            log_desc({n=n,a=a})
            if n > 3 then
               return a
            else
               return squares(n + 1, a * a)
            end
         end
      end)
end


require('lib.log')
log('rv = ' .. test() .. '\n')
