-- Minimalistic Actor Model implementation based on Lua coroutines,
-- loosely inspired by Erlang and encouraged by Emacs distel.
--
-- For a usage example, see webserver.lua
-- For a C version, see ns_actor.h actor.h test_actor.c
--
-- Note that many of Erlang's features are not implemented: links,
-- monitors, distribution, smp.  Most notable missing feature is an
-- ability to filter the message queue, which is essential to
-- implement some patterns.  That will probably need to be added
-- before this is practically useful.

local prompt = require('prompt')
local function log(str) io.stderr:write(str) end
local function log_desc(thing) log(prompt.describe(thing)) end

local actor = { scheduler = {}, task = {} }

function actor.scheduler.new()
   local s = { hot = {} }
   setmetatable(s, { __index = actor.scheduler })
   return s
end

function actor.scheduler:schedule()
   while self:schedule_one() do end
end

function actor.scheduler:schedule_one()
   for task,_true in pairs(self.hot) do
      -- Remove from hot list before scheduling.  If task sends itself
      -- a message it will get moved back to the hot list.
      self:remove(task)
      if not task:resume() then
         -- If task died, make sure it is not on the hot list.
         self:remove(task)
      end
      -- The 'for' loop is only for convenience.  We do not go through
      -- the loop as the self.hot structure likely changed.
      return true
   end
   return false
end

-- Factored out for debugging purposes.
function actor.scheduler:add(task)    self.hot[task] = true end
function actor.scheduler:remove(task) self.hot[task] = nil  end


-- Spawning is optionally a two-step process to be able to create
-- circular task references.  task.new() creates the task structure
-- such that it is ready to receive messages but doesn't yet have
-- behavior attached, while scheduler.spawn will attach code to it and
-- optionally create a new task structure if it wasn't passed in.


-- The code below is to make sure that tasks get garbage-collected
-- properly.  These are just to give an indication during development
-- and are left here commented-out for reference.  Measurement on x64
-- gave 500 bytes for coroutine_size.
--
-- -- It appears that __gc method on Lua tables doesn't work, so use a
-- -- dummy ffi cdata object to attach a finalizer.
-- local ffi = require('ffi')
-- ffi.cdef('struct test_gc { uint32_t dummy; }')
-- local ctype = ffi.typeof('struct test_gc')
-- function test_gc()
--    local cdata = ffi.new(ctype)
--    ffi.gc(cdata, function() io.stderr:write("gc\n") end)
--    return cdata
-- end

function actor.scheduler:task()
   return actor.task.new(self)
end

function actor.scheduler:spawn(body, task)
   if not task then task = self:task() end
   -- task.test_gc = test_gc()
   -- local m1 = collectgarbage('count')
   task.coroutine = coroutine.create(
      function()
         body(task)
         task:exit()
      end)
   -- local m2 = collectgarbage('count')
   -- log("coroutine_size(kb): " .. 1024*(m2-m1) .."\n")
   --
   -- Note that we only create the coroutine and do not resume.
   -- Spawning is an asynchronous operation just like sending a
   -- message.  Mark it hot, so it will be executed by the scheduler
   -- at the next occasion.
   self:add(task)
   return task
end

function actor.scheduler:send(task, msg)
   if task.mbox then
      table.insert(task.mbox, msg)
      self:add(task)
   else
      -- A task without a mbox is a dead task.  Messages sent to dead
      -- tasks will be dropped.
      -- log("sending to dead task\n")
   end
end


-- Create a task that is ready to accept messages.  It needs a
-- mailbox, a scheduler reference, and the metatable.  It is not yet
-- associated to a coroutine.
function actor.task.new(scheduler)
   task = { mbox = {}, scheduler = scheduler }
   setmetatable(task, { __index = actor.task })
   return task
end

function actor.task:resume()
   local co = self.coroutine
   local ok, rv = coroutine.resume(co)
   if not ok then
      -- log("resume->false\n")
      self:exit()
      local statusmsg = coroutine.status(co)
      local traceback = debug.traceback(co)
      log("coroutine_resume: status=" .. statusmsg .. "\n")
      log(rv .. "\n") -- error message
      log(traceback)
      log("\n")
   else
      -- Note that when the coroutine "runs of the end", ok will be
      -- true, but coroutine.status will return dead.  In that case
      -- self:exit() will already have been called in the coroutine
      -- body.  See scheduler.spawn
      --
      -- log("status: " .. coroutine.status(co) .. "\n")
   end
   return status
end

function actor.task:exit()
   -- Remove from hot list to make sure scheduler will not try to
   -- resume this task.
   self.scheduler:remove(self)
   -- To ensure that nothing will reschedule us through send, remove
   -- the mailbox as an indication that task is dead.  Any un-handled
   -- messages are lost.
   self.mbox = nil
   return
end

-- Send never blocks.  It appends the message to the task's mailbox
-- and marks the task 'hot', to be scheduled at the next occasion.
function actor.task:send(msg)
   self.scheduler:send(self, msg)
end

-- Receive blocks if there are no available messages.  Note that a
-- filtering mechanism is necessary for implementing message priority
-- schemes.  See e.g. the implementation of actor_uv.sleep.
function actor.task:recv(filter)
   if not filter then
      -- Pick the first available message if no filter is specified.
      filter = function() return true end
   end
   while true do
      for i=1,#self.mbox do
         if filter(self.mbox[i]) then
            return table.remove(self.mbox, i)
         end
      end
      coroutine.yield()
   end
end

return actor
