-- Minimalistic webserver.
--
-- This is obviously not intended as a serious web server, but might
-- be useful as a plumbing tool in addition to being a test for the
-- actor library.

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
   -- First line is request
   local req = self:recv()
   -- Rest is headers up to empty line.  Collect those in an array.
   local hdr = {}
   while true do
      local line = self:recv()
      if line == '\r\n' then break end
      table.insert(hdr, line)
   end
   -- Handle request
   log("req: " .. req)
   if req == "GET / HTTP/1.1\r\n" then
      log("->200\n")
      self.socket:write(
         "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n" ..
         "<h1>Hello1</h1>")
   else
      log("->404\n")
      self.socket:write("HTTP/1.1 404 Not Found\r\n\r\n404\r\n")
   end
   -- Keepalive is not implemented.
   self.socket:close()
end

local webserver = {}
function webserver.start(port)
   local function connect(lsocket, err)
      collectgarbage()

      -- Line buffer is presented with chunks from the socket, which
      -- then get pushed into the mailbox of a task.
      local asocket = lsocket:accept()
      local buf = linebuf:new()
      local task = scheduler:task()
      task.socket = asocket
      scheduler:spawn(serve, task)
      buf.push_line = function(self, line)
         task:send(line) -- deliver
         scheduler:schedule() -- propagate
      end
      asocket:start_read(
         function(_, err, data)
            if err then
               -- Task will close socket after delivering response, so
               -- we only get here on error.
               log("error,disconnect\n")
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

return webserver
