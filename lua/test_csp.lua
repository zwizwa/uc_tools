#!/usr/bin/lua

local prompt = require('prompt')
local function log(str) io.stderr:write(str) end
local function log_desc(thing) log(prompt.describe(thing)) end


local csp = require('lib.csp')


function test_send_recv()
   local sched = csp.scheduler.new()
   local ch = sched:new_channel()

   local function sender_body(self)
      for i=1,10 do
         self:send(ch, "hello")
      end
   end

   local function receiver_body(self)
      while true do
         local data1 = self:recv(ch)
         log("receiver data1: " .. data1 .. "\n")
         local data2 = self:recv(ch)
         log("receiver data2: " .. data2 .. "\n")
      end
   end

   sched:spawn(sender_body, "sender")
   sched:spawn(receiver_body, "receiver")

end



function test_rpc()
   local sched = csp.scheduler.new()
   local ch = sched:new_channel()

   local function client_body(self)
      for i=1,10 do
         self:send(ch, "request" .. i)
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
         local reply = request .. "_reply"
         self:send(ch, reply)
      end
   end

   sched:spawn(client_body, "client")
   sched:spawn(server_body, "server")

end


function test_external_input()
   local sched = csp.scheduler.new()
   local ch = sched:new_channel()

   local function receiver_body(self)
      while true do
         local data = self:recv(ch)
         log("receiver data: " .. data .. "\n")
      end
   end

   sched:spawn(receiver_body, "receiver")
   for i=1,5 do
      sched:push(ch, "data" .. i + 100)
   end

end


-- FIXME: implement a leak test that creates lots of tasks and
-- channels.

test_send_recv()
test_rpc()
test_external_input()



