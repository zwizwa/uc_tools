#!/usr/bin/lua

-- This tests the actor.lua library bound to the libuv i/o framework.
-- See test_actor.lua for pure tests.

local prompt = require('prompt')
local function log(str) io.stderr:write(str) end
local function log_desc(thing) log(prompt.describe(thing)) end

local actor = require('lib.actor')
local actor_uv = require('lib.actor_uv')

local uv = require('lluv')

-- local default_ms = 1000
local default_ms = 100

function test_uv1()
   local sched = actor.scheduler.new()

   -- Add a task to the scheduler.  This by itself does not start the
   -- task: actor send and spawn are asynchronous and are normally
   -- called inside a scheduler run, but we are outside of the
   -- scheduler loop here, so we call schedule manually after spawing.
   local t1 = sched:spawn(
      function(self)
         for i=1,3 do
            actor_uv.send_after(self, i .. ':tick\n', default_ms)
            log(self:recv())
         end
         uv.stop()
      end,
      sched:task({}))

   -- This runs all tasks until all tasks are blocked.  In this
   -- example there will be just the one task that has set up an uv
   -- timer and is blocking, waiting for the delayed message it sent
   -- itself.
   sched:schedule()

   -- We let uv take over.  A timer has been installed, so its
   -- mainloop will start executing until uv.stop() is called
   -- explicitly by the task.
   uv.run()

end

function test_uv2()

   -- Similar to test_uv1, but with two tasks sending messages to each
   -- other.  This serves as an example of how to create circular
   -- networks of tasks.

   local sched = actor.scheduler.new()

   -- For circular task references, the 2-step start process can be
   -- used.  Create the task (without behavior), and then use that
   -- structure to pass to spawn, which will attach behavior that can
   -- then reference the task structures.

   local t2 = sched:task({})
   local t1 = sched:task({})

   sched:spawn(
      function(self)
         for i=1,3 do
            actor_uv.send_after(t2, i .. ':tick\n', default_ms)
            log(self:recv())
         end
         uv.stop()
      end, t1)

   sched:spawn(
      function(self)
         -- Echo server
         while true do
            t1:send("echo:" .. self:recv())
         end
      end, t2)

   -- This runs all tasks until all tasks are blocked.  In this
   -- example there will be just the one task that has set up an uv
   -- timer and is blocking, waiting for the delayed message it sent
   -- itself.
   sched:schedule()

   -- We let uv take over.  A timer has been installed, so its
   -- mainloop will start executing until uv.stop() is called
   -- explicitly by the task.
   uv.run()

end

function test_uv3()
   local sched = actor.scheduler.new()
   sched:spawn(
      function(self)
         -- Put something in mailbox to test filter mechanism used in
         -- sleep.
         self:send("abc\n")
         for i=1,3 do
            log(i .. ":sleep\n")
            actor_uv.sleep(self, default_ms)
         end
         log(self:recv())
         uv.stop()
      end,
      sched:task({}))
   sched:schedule()
   uv.run()
end


log("test_uv1\n") ; test_uv1()
log("test_uv2\n") ; test_uv2()
log("test_uv3\n") ; test_uv3()

