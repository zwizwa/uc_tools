#!/usr/bin/lua

local prompt = require('prompt')
local function log(str) io.stderr:write(str) end
local function log_desc(thing) log(prompt.describe(thing)) end

local actor = require('lib.actor')

function test_send_recv()
   local sched = actor.scheduler.new()

   local function receiver_body(self)
      log("receiver start\n")
      while true do
         local data1 = self:recv(ch)
         log("receiver data1: " .. data1 .. "\n")
         local data2 = self:recv(ch)
         log("receiver data2: " .. data2 .. "\n")
      end
   end

   local receiver = sched:spawn(receiver_body, {name = "receiver"})

   local function sender_body(self)
      log("sender start\n")
      for i=1,10 do
         self:send(receiver, "hello")
      end
   end

   sched:spawn(sender_body, {name = "sender"})

   sched:schedule()
end


test_send_recv()
