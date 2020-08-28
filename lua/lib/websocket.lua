-- Code based on erl_tools minimal WebSocket server.  Basic idea is to
-- get some two-way connection between web and a Lua app without any
-- dependencies.

-- Build on libuv async I/O library as a system interface.
local uv = require('lluv')

-- Use a simple push-style state machine for line buffering.
local linebuf = require('lib.linebuf')

-- But implement the rest of the logic using a multitasking
-- abstraction over Lua coroutines.  Scheduler object is used a lot so
-- use a short name.
local actor = require('lib.actor')
local scheduler = actor.scheduler.new()

local function log(str)
   io.stderr:write(str)
end

local function serve(self)
   local req = self:recv()
   local hdr = {}
   while true do
      local line = self:recv()
      if line == '\r\n' then break end
      table.insert(hdr, line)
   end
   log("req: " .. req)
   -- FIXME: At least format responses a bit better.
   if req == "GET / HTTP/1.1\r\n" then
      self.socket:write("Hello\n")
   else
      self.socket:write("404\n")
   end
   self.socket:close()
end

local websocket = {}
function websocket.start(port)
   local function connect(lsocket, err)
      -- Line buffer is presented with chunks from the socket, which
      -- then get pushed into the mailbox of a task.
      local asocket = lsocket:accept()
      local buf = linebuf:new()
      local task = scheduler:spawn(serve, { socket = asocket })
      buf.push_line = function(self, line)
         scheduler:send(task, line)
         scheduler:schedule()
      end
      asocket:start_read(
         function(_, err, data)
            if err then
               -- FIXME: Remove cold list to allow tasks to be
               -- garbage-collected?
               log("disconnect\n")
               task:halt()
            else
               buf:push(data)
            end
      end)
   end
   local lsocket = uv.tcp()
   lsocket:bind('0.0.0.0',port)
   lsocket:listen(connect)
end

return websocket
