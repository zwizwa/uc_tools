-- I/O functionality for actor.lua based on libuv.
local actor = require('lib.actor')
local uv = require('lluv')

local actor_uv = {}

-- Start with the simplest event: deliver a message in the future.
function actor_uv.send_after(task, msg, ms)
   local t = uv.timer()
   t:start(
      ms, 0,
      function(timer)
         timer:close()
         task:send(msg) -- deliver
         task.scheduler:schedule() -- propagate
      end)
   -- For future extensions, e.g. to allow cancellable sends.
   return t
end


return actor_uv


