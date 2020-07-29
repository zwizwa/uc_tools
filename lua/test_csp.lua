#!/usr/bin/lua

local prompt = require('prompt')
local function log(str) io.stderr:write(str) end
local function log_desc(thing) log(prompt.describe(thing)) end


local csp = require('lib.csp')



function test_send_recv()
   -- log_desc(getmetatable(sched))
   local sched = csp.scheduler.new()
   local ch = "ch0"

   local function task1_body(self)
      for i=1,10 do
         self:send(ch, "hello")
      end
   end
   local function task2_body(self)
      while true do
         local data1 = self:recv(ch)
         log("task2 recv data1: " .. data1 .. "\n")
         local data2 = self:recv(ch)
         log("task2 recv data2: " .. data2 .. "\n")
      end
   end

   sched:spawn(task1_body, "task1")
   sched:spawn(task2_body, "task2")

end

-- FIXME: doesn't work yet

function test_rcp()
   local sched = csp.scheduler.new()
   local ch = "ch0"

   local function client_body(self)
      for i=1,10 do
         self:send(ch, "request")
         local reply = self:recv(ch)
         assert(reply)
         log("reply: " ..  reply .. "\n")
      end
   end
   local function server_body(self)
      while true do
         local request = self:recv(ch)
         assert(request)
         log("request: " .. request .. "\n")
         local reply = "reply"
         self:send(ch, reply)
      end
   end

   sched:spawn(client_body, "client")
   sched:spawn(server_body, "server")

end


test_send_recv()
test_rcp()


