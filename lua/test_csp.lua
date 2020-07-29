#!/usr/bin/lua

local prompt = require('prompt')
local function log(str) io.stderr:write(str) end
local function log_desc(thing) log(prompt.describe(thing)) end


local csp = require('lib.csp')



function test()
   local sched = csp.scheduler.new()
   -- log_desc(getmetatable(sched))

   local function task1_body(self)
      for i=1,10 do
         self:send("ch0", "hello")
      end
   end
   local function task2_body(self)
      while true do
         local data1 = self:recv("ch0")
         log("task2 recv data1: " .. data1 .. "\n")
         local data2 = self:recv("ch0")
         log("task2 recv data2: " .. data2 .. "\n")
      end
   end

   sched:spawn(task1_body, "task1")
   sched:spawn(task2_body, "task2")

end

test()


