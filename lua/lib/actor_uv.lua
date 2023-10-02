-- I/O functionality for actor.lua based on libuv.
-- https://github.com/moteus/lua-lluv
--
-- These are small wrappers that all have the same style: for a libuv
-- event, set up a callback that will send a message and then run the
-- actor scheduler.

local uv        = require('lluv')
local actor     = require('lib.actor')
local linebuf   = require('lib.linebuf')
local packetbuf = require('lib.packetbuf')
local mixin     = require('lib.mixin')

local function log(str)
   io.stderr:write(str)
end

local actor_uv = {}

-- Deliver a message in the future.
function actor_uv.send_after(task, msg, ms, t)
   if not t then
      t = uv.timer()
   end
   t:start(
      ms, 0,
      function(timer)
         timer:close()
         task:send_and_schedule(msg)
      end)
   return t
end

-- FIXME: It's probably ok to put a timer object inside the actor_uv
-- object and reuse that.
function actor_uv:sleep(ms)
   local t = uv.timer()
   -- A token is needed for use in the recv filter.  If we create our
   -- own private timer we can use that as a guaranteed unique token.
   local msg0 = t
   actor_uv.send_after(self, msg0, ms, t)
   self:recv(function(msg) return msg0 == msg end)
end

function actor_uv:recv_with_timeout(timeout_ms, filter)
   if not filter then filter = function(_) return true end end
   local t = uv.timer()
   -- msg0 pointer is unique tag, can be used with '=='
   local msg0 = {"timer", timeout_us}
   actor_uv.send_after(self, msg0, timeout_ms, t)
   local msg = self:recv(function(msg) return (msg0 == msg) or filter(msg) end)
   if msg ~= msg0 then
      -- We got a proper reply, so cancel the timer.  FIXME: Does this
      -- need to check that the timer didn't queue a message?
      t:stop()
      t:close()
   end
   return msg
end

-- Create a task with actor_uv behavior mixed in.
function actor_uv.task(scheduler, obj)
   assert(scheduler)
   assert(obj)
   mixin.add(obj, actor_uv)
   return scheduler:task(obj)
end

-- A TCP server
function actor_uv.spawn_tcp_server(scheduler, serv_obj)
   assert(serv_obj.ip)
   assert(serv_obj.port)

   local function connect(lsocket, err)

      -- This produces a mixin object that implements :handle()
      local task = serv_obj:connection()
      -- We then add task behavior mixin.
      actor_uv.task(scheduler, task)
      task.socket = lsocket:accept()
      if not task.socket then
         error("Cannot create TCP server socket port " .. serv_obj.port)
      end

      scheduler:spawn(
         function(task)
            task:connect()
            task.socket:close()
         end,
         task)

      -- Configure how data will be pushed into the actor network.  It
      -- seems simpler to do this at the write end as opposed to the
      -- read end.
      local push

      if serv_obj.mode == 'line' then
         -- Line buffer is presented with chunks from the socket,
         -- which then get pushed into the mailbox of a task.
         local buf = linebuf.new()
         buf.push_line = function(self, line) task:send_and_schedule({task.socket,line}) end
         push = function(data) buf:push(data) end

      elseif serv_obj.mode and serv_obj.mode[1] == 'packet' then
         local size_bytes = serv_obj.mode[2]
         assert('number' == type(size_bytes))
         local buf = packetbuf.new(size_bytes)
         buf.push_packet = function(self, packet) task:send_and_schedule({task.socket,packet})  end
         push = function(data) buf:push(data) end

      elseif serv_obj.mode == 'raw' then
         push = function(data) task:send_and_schedule({task.socket,data}) end

      else
         error('bad .mode')
      end

      task.socket:start_read(
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
   lsocket:bind(serv_obj.ip, serv_obj.port)
   lsocket:listen(connect)
   return lsocket
end

-- Blocking abstraction for asynchronous socket writes.
function actor_uv:write_socket(socket, data)
   socket:write(
      data,
      function()
         -- FIXME: handle write errors in callback.
         -- Use the socket as sync message
         self:send_and_schedule(socket)
      end)
   self:recv(function(msg) return socket == msg end)
end



-- Note that the reason we parameterize push here is that sometimes it
-- is simpler to split a packet before sending it to the task,
-- i.e. such that the task only sees complete packets, and maybe some
-- other routing can be performed that is specific to the binary
-- encoding of the incoming data.

function actor_uv.spawn_process(scheduler, executable, args, body, push)
   assert(executable)
   assert(args)

   if nil == push then
      push = function(data, push_packet) push_packet(data) end
   end

   -- Task data
   local task = {
      stdin  = uv.pipe(),
      stdout = uv.pipe(),
      stderr = uv.pipe(),
   }
   -- We then add task behavior mixin.
   actor_uv.task(scheduler, task)

   local function error_handler(handle, err, status, signal)
      -- Note: No documentation was found about the libuv error
      -- handler API, but there seem to be two cases that occur in
      -- practice: an error happens during startup, e.g. executable
      -- not found, or the process dies. e.g. due to a bug or fatal
      -- error in the program.
      handle:close()
      log_desc({error_handler = {err=err,status=status,signal=signal}})
   end

   -- Create libuv process handle
   task.handle = uv.spawn({
         file = executable,
         stdio = {
            { stream = task.stdin,  flags = uv.CREATE_PIPE + uv.READABLE_PIPE },
            { stream = task.stdout, flags = uv.CREATE_PIPE + uv.WRITABLE_PIPE },
            { stream = task.stderr, flags = uv.CREATE_PIPE + uv.WRITABLE_PIPE }
         },
         args = args
   }, error_handler)

   -- Spawn the task.  It can use task:recv() to pop the mailbox,
   -- which will give tagged {port, ...} messages for incoming pipe
   -- data.  It can also write to task.stdin
   scheduler:spawn(body, task)

   -- Anything coming in on stdout gets sent to the task.  Note that
   -- we cannot call error() in the uv callbacks, as that is the main
   -- Lua thread, and errors there will terminate the Linux process.
   -- So any error is propagated to the task, which then can be
   -- handled, or the task can die with error() inside its coroutine
   -- such that the scheduler can pick up the coroutine crash and call
   -- any registered monitors.
   local function read_callback(tag_from)
      return function(_,err,data)
         if not err then
            push(data,
                 function(packet)
                    task:send_and_schedule({tag_from,packet})
                 end)
         else
            task:send_and_schedule({"error",{tag_from, err, data}})
         end
      end
   end

   task.stdout:start_read(read_callback("stdout"))
   task.stderr:start_read(read_callback("stderr"))

   return task


end

return actor_uv
