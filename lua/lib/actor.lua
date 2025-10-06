-- Minimalistic Actor Model implementation based on Lua coroutines,
-- loosely inspired by Erlang and encouraged by Emacs distel.
--
-- For a usage example, see webserver.lua
-- For a C version, see ns_actor.h actor.h test_actor.c
--
-- See bottom of this file for more notes.


-- Mixins seem appropriate to layer the different abstractions: the
-- core send/recv functionality from this file, the i/o abstractions
-- from actor_uv.lua, and the user behavior.

local mixin  = require('lib.mixin')
local prompt = require('prompt')
local function log(str) io.stderr:write(str) end
local function logf(...) log(string.format(...)) end
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

function actor.scheduler:self()
   return self.current_task
end

function actor.scheduler:schedule_one()
   for task,_true in pairs(self.hot) do
      -- Remove from hot list before scheduling.  If task sends itself
      -- a message it will get moved back to the hot list.
      self:remove(task)
      self.current_task = task
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

function actor.scheduler:task(obj)
   return actor.task.init(self, obj)
end

function actor.scheduler:spawn(body, task)
   assert(task)
   assert(not task.coroutine)
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
      logf("sending to dead task %s\n", task.name)
   end
end

-- Default name.  Task should probably override this.
actor.task.name = "<unknown>"


-- Create a task that is ready to accept messages.  It needs a
-- mailbox, a scheduler reference, and the metatable.  It is not yet
-- associated to a coroutine.
function actor.task.init(scheduler, task)
   -- Instead of auto-creating, it seems simpler to require user to
   -- specify the object to which we will add behavior.
   assert(task)
   -- Object can be set up with initial content.
   task.mbox = {}
   task.scheduler = scheduler
   task.monitor = {}
   mixin.add(task, actor.task)
   return task
end

function actor.task:resume()
   local log_ = self.log or log

   local co = self.coroutine
   local ok, rv = coroutine.resume(co)
   if not ok then
      -- log("resume->false\n")
      local statusmsg = coroutine.status(co)
      local traceback = debug.traceback(co)
      log_("actor.task:resume_error: status: " .. statusmsg .. "\n")
      log_("actor.task:resume_error: traceback:\n")
      -- If not nil this is the Lua error message.
      if (rv) then log(rv .. "\n") end
      log_(traceback)
      log_("\n")
      self:exit()
   else
      -- Note that when the coroutine "runs of the end", ok will be
      -- true, but coroutine.status will return dead.  In that case
      -- self:exit() will already have been called in the coroutine
      -- body.  See scheduler.spawn
      --
      local statusmsg = coroutine.status(co)
      if statusmsg ~= "suspended" then
         log_("actor.task:resume_ok: status=" .. statusmsg .. "\n")
         self:exit()
      end
   end
   return status
end

-- When a task gets killed, all the linked tasks get notified.
function actor.task:add_monitor(other_task, ref)
   self.monitor[ref] = other_task
end

-- Called when the coroutine is dead.
function actor.task:exit(reason)
   -- Send a message to all tasks monitoring this one.
   for ref, tsk in pairs(self.monitor) do
      tsk:send({self, {'down', ref, reason}})
   end
   -- Remove from hot list to make sure scheduler will not try to
   -- resume this task.  Do this after sending monitor message to
   -- handle degenerate self-monitor.
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

-- For events that are generated outside of a schedule() run, the
-- scheduler needs to be run after a send() call.  The send() call
-- only queues the message.  E.g. in an event-driven system, this is
-- called from event callback functions.
function actor.task:send_and_schedule(msg)
   self:send(msg)
   self.scheduler:schedule()
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


-- Unresolved questions
--
-- 1. Direct vs. indirect references.
--
--    Currently, direct references are used.  This allows tasks to be
--    garbage-collected.  Why would indirect references be preferable?
--    One thing I can think of is naming tasks in a different object
--    space, e.g. a different Lua process or a different physical
--    machine, as Erlang can do.  That is outside of the scope of this
--    implementation.  Any other reasons?
--
-- 2. Efficiency of mailbox implementation.  Minor.  The properties
--    I'm looking for are simplicty, convenience, and memory
--    efficiency, not so much run time efficiency.  Event systems
--    mostly do nothing.
--
--




