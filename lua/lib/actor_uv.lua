-- I/O functionality for actor.lua based on libuv.
local actor = require('lib.actor')
local uv = require('lluv')

local linebuf = require('lib.linebuf')

local function log(str)
   io.stderr:write(str)
end

local actor_uv = {}

-- Deliver a message in the future.
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


-- A TCP server
function actor_uv.spawn_tcp_server(scheduler, config, serve)
   assert(config.ip)
   assert(config.port)

   local function connect(lsocket, err)

      local asocket = lsocket:accept()
      local task = scheduler:task()
      task.socket = asocket
      scheduler:spawn(
         function(task)
            serve(task)
            asocket:close()
         end,
         task)

      -- Configure how data will be pushed into the actor network.
      local push
      if config.mode == 'line' then
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
      elseif config.mode == 'raw' then
         push = function(data)
            task:send(data)
            scheduler:schedule() -- propagate
         end
      else
         error('bad config.mode')
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
   lsocket:bind(config.ip, config.port)
   lsocket:listen(connect)
   return lsocket
end


return actor_uv
