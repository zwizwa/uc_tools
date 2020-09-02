-- I/O functionality for actor.lua based on libuv.
--
-- These are small wrappers that all have the same style: for a libuv
-- event, set up a callback that will send a message and then run the
-- actor scheduler.

local actor = require('lib.actor')
local uv = require('lluv')

local linebuf = require('lib.linebuf')

local mixins = require('lib.mixins')

local function log(str)
   io.stderr:write(str)
end

local actor_uv = {}

-- Deliver a message in the future.
function actor_uv:send_after(msg, ms, t)
   if not t then
      t = uv.timer()
   end
   t:start(
      ms, 0,
      function(timer)
         timer:close()
         send:send(msg) -- deliver
         send.scheduler:schedule() -- propagate
      end)
   return t
end

function actor_uv:sleep(ms)
   local t = uv.timer()
   -- A token is needed for use in the recv filter.  If we create our
   -- own private timer we can use that as a guaranteed unique token.
   local msg0 = t
   self:send_after(msg0, ms, t)
   task:recv(function(msg) return msg0 == msg end)
end

-- Create a task with actor_uv behavior mixed in.
function actor_uv.task(scheduler, obj)
   mixins.add(obj, actor_uv)
   return scheduler:task(obj)
end

-- A TCP server
function actor_uv.spawn_tcp_server(scheduler, obj)
   assert(obj.ip)
   assert(obj.port)

   local function connect(lsocket, err)

      local asocket = lsocket:accept()
      local task = actor_uv.task(scheduler, obj)

      task.socket = asocket
      scheduler:spawn(
         function(task)
            task:handle()
            asocket:close()
         end,
         task)

      -- Configure how data will be pushed into the actor network.  It
      -- seems simpler to do this at the write end as opposed to the
      -- read end.
      local push
      if obj.mode == 'line' then
         -- Line buffer is presented with chunks from the socket,
         -- which then get pushed into the mailbox of a task.
         local buf = linebuf:new()
         buf.push_line = function(self, line)
            task:send(line) -- deliver
            scheduler:schedule() -- propagate
         end
         push = function(data)
            buf:push(data)
         end
      elseif obj.mode == 'raw' then
         push = function(data)
            task:send(data)
            scheduler:schedule() -- propagate
         end
      else
         error('bad .mode')
      end

      asocket:start_read(
         function(_, err, data)
            if err then
               -- Task will close socket after delivering response, so
               -- we only get here on error.
               log("error,disconnect\n")
               task:halt()
            else
               -- log("push: " .. data)
               push(data)
            end
         end)

   end
   local lsocket = uv.tcp()
   lsocket:bind(obj.ip, obj.port)
   lsocket:listen(connect)
   return lsocket
end


return actor_uv
