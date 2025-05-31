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

-- The task object below is the task handling the connection.
function actor_uv.line_mode_push(task)
   -- Line buffer is presented with chunks from the socket,
   -- which then get pushed into the mailbox of a task.
   local buf = linebuf.new()
   buf.push_line = function(self, line) task:send_and_schedule({task.socket,line}) end
   return function(data) buf:push(data) end
end
function actor_uv.packet_mode_push(task, serv_obj)
   local size_bytes = serv_obj.mode[2]
   assert('number' == type(size_bytes))
   local buf = packetbuf.new(size_bytes)
   buf.push_packet = function(self, packet) task:send_and_schedule({task.socket,packet})  end
   return function(data) buf:push(data) end
end
function actor_uv.raw_mode_push(task)
   return function(data) task:send_and_schedule({task.socket,data}) end
end

-- A TCP server
function actor_uv.spawn_tcp_server(scheduler, serv_obj)
   assert(serv_obj.ip)
   assert(serv_obj.port)

   local listener = {
   }

   local function connect(lsocket, err)

      -- This produces a mixin object that implements :handle()
      local task = serv_obj:connection()

      -- We then add task behavior mixin.
      actor_uv.task(scheduler, task)
      task.socket = lsocket:accept()
      if not task.socket then
         error("Cannot create TCP server socket port " .. serv_obj.port)
      end

      -- How to get remote port?  To uniquely identify, use the table
      -- pointer.
      local conn_name = task.socket:getpeername() .. '@' .. string.sub(tostring(task), 10)

      listener[conn_name] = task

      scheduler:spawn(
         function(task)
            task:connect()
            task.socket:close()
            listener[conn_name] = nil
         end,
         task)

      -- Configure how data will be pushed into the actor network.  It
      -- seems simpler to do this at the write end as opposed to the
      -- read end.  The function is stored inside the connection task
      -- object such that mode can be changed later.

      if serv_obj.mode == 'line' then
         task.push = actor_uv.line_mode_push(task)

      elseif serv_obj.mode and serv_obj.mode[1] == 'packet' then
         task.push = actor_uv.packet_mode_push(task, serv_obj)

      elseif serv_obj.mode == 'raw' then
         task.push = actor_uv.raw_mode_push(task)

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
               task.push(data)
            end
         end)

   end
   local lsocket = uv.tcp()
   listener.lsocket = lsocket

   lsocket:bind(serv_obj.ip, serv_obj.port)
   lsocket:listen(connect)

   return listener
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

function actor_uv.spawn_process(scheduler, executable, args, body, push, error_handler)
   assert(executable)
   assert(args)

   if nil == push then
      push = function(data, push_packet) push_packet(data) end
   end

   -- Task data
   local task = {
   }
   -- We then add task behavior mixin.
   actor_uv.task(scheduler, task)

   task.close = function()
      uv:close(task.handle) ; task.handle = nil
      uv:close(task.stdin)  ; task.stdin  = nil
      uv:close(task.stdout) ; task.stdout = nil
      uv:close(task.stderr) ; task.stderr = nil
   end

   local function default_error_handler(handle, err, status, signal)
      -- Note: No documentation was found about the libuv error
      -- handler API, but there seem to be two cases that occur in
      -- practice: an error happens during startup, e.g. executable
      -- not found, or the process dies. e.g. due to a bug or fatal
      -- error in the program.
      task:close()
      log_desc({uv_error_handler = {err=err,status=status,signal=signal}})
   end

   -- Anything coming in on stdout gets sent to the task.  Note that
   -- we cannot call error() in the uv callbacks, as that is the main
   -- Lua thread, and errors there will terminate the Linux process.
   -- So any error is propagated to the task, which then can be
   -- handled, or the task can die with error() inside its coroutine
   -- such that the scheduler can pick up the coroutine crash and call
   -- any registered monitors.
   local function read_callback(tag_from)
      return function(_,err,data)
         -- log_desc({read_callback={err=err,data=data}})
         if not err then
            push(data,
                 function(packet)
                    task:send_and_schedule({tag_from,packet})
                 end)
         else
            -- Error codes are in lua-lluv/src/lluv_error.c
            -- https://github.com/moteus/lua-lluv v1.1.10
            task:send_and_schedule({"error",{tag_from, err:no(), err:msg()}})
         end
      end
   end

   -- Create libuv process handle
   task.cmd = { executable = executable, args = args }

   local function uv_spawn()
      task.stdin  = uv.pipe()
      task.stdout = uv.pipe()
      task.stderr = uv.pipe()
      task.handle =
         uv.spawn(
            {
               file = task.cmd.executable,
               stdio = {
                  { stream = task.stdin,  flags = uv.CREATE_PIPE + uv.READABLE_PIPE },
                  { stream = task.stdout, flags = uv.CREATE_PIPE + uv.WRITABLE_PIPE },
                  { stream = task.stderr, flags = uv.CREATE_PIPE + uv.WRITABLE_PIPE }
               },
               args = task.cmd.args
            },
            error_handler or default_error_handler)

      task.stdout:start_read(read_callback("stdout"))
      task.stderr:start_read(read_callback("stderr"))

   end
   uv_spawn()

   -- To restart just the uv process, keeping the actor alive.
   task.restart = uv_spawn

   -- Spawn the task.  It can use task:recv() to pop the mailbox,
   -- which will give tagged {port, ...} messages for incoming pipe
   -- data.  It can also write to task.stdin
   scheduler:spawn(body, task)



   return task


end

-- The cfg supports some modes:
--
-- 1. if 'body' is defined, that will be used as the task body
--
-- 2. if `handle_line` is defined, we will create a body and just push
--    lines to that function
--
-- 3. if neither is defined, lines will just go to stderr

function actor_uv.spawn_line_process(scheduler, executable, args, cfg)
   cfg = cfg or {}

   local buf = linebuf.new()
   local function push(data, push_packet)
      buf.push_line = function(self, line) push_packet(line) end
      buf:push(data)
   end
   local body = cfg.body or function(task)
      while true do
         local msg = task:recv()
         local from, line = unpack(msg)
         if from == "stdout" then
            local handle_line = cfg.handle_line or log
            handle_line(line)
         else
            log_desc({ignoring_line_process_msg = msg})
         end
      end
   end
   local function error_handler(handle, err, status, signal)
      log_desc({uv_error_handler =
                   {err=err,status=status,signal=signal}})
   end
   local task = actor_uv.spawn_process(
      scheduler, executable, args, body, push, error_handler)
   return task
end





return actor_uv
