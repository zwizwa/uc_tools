-- Same mechanism as csp.c

-- Initially the idea was to expose csp.c to Lua.  However, the meat
-- of the implementation really is in the data structures, and if Lua
-- is available, it makes a lot more sense to just use Lua
-- datastructures for everything, and rely on garbage collection.  So
-- this is a re-implementation of the schedule_task() from csp.c with
-- some more general data structures:
--
-- . Move cold lists to channel objects
-- . Use "send" and "recv" symbols to tag directions

-- Note that task and scheduler are orthogonal and communicate only
-- through a small interface.  E.g. the scheduler does not know that
-- the tasks are coroutines, and other mechanisms could be added.  In
-- fact, external event input interfacing uses a task that is not a
-- coroutine.

local prompt = require('prompt')
local function log(str) io.stderr:write(str) end
local function log_desc(thing) log(prompt.describe(thing)) end

local csp = {
   scheduler = {},
   task = {},
   channel = {},
   pusher = {}
}

-- CHANNEL

-- Channel structures just store the cold list.  Each of send, recv
-- contain a set of waiting tasks, represented as a table mapping task
-- to event.
function csp.channel.new()
   local ch = { send = {}, recv = {} }
   return ch
end
function csp.channel:add_cold(dir)
end


-- SCHEDULER

-- The scheduler is just a hot task list.  The cold task list is
-- stored outside of the scheduler, in the channel data structures.
function csp.scheduler.new()
   local s = { hot_task_list = {} }
   setmetatable(s, { __index = csp.scheduler })
   return s
end

-- Make this a scheduler method, in case we need to track channels
-- later.  Currently that is not necessary.
function csp.scheduler:new_channel()
   return csp.channel.new()
end

-- These two do not actually refer to the scheduler object.  They
-- operate only on the channel data structure obtained from the task
-- event list.
function csp.scheduler:add_cold(cold_task)
   -- log("add_cold: " .. cold_task.name .. "\n")
   cold_task.selected = nil
   for i,evt in ipairs(cold_task.events) do
      local waiting = evt.channel[evt.direction]
      waiting[cold_task] = evt
   end
end
function csp.scheduler:remove_cold(cold_task)
   for i,evt in ipairs(cold_task.events) do
      local waiting = evt.channel[evt.direction]
      waiting[cold_task] = nil
   end
end


function csp.scheduler:add_hot(hot_task)
   table.insert(self.hot_task_list, hot_task)
end

function csp.scheduler:rendezvous(send_task, send_evt, recv_task, recv_evt)
   -- log("rendezvous send=" .. send_task.name .. ", recv=" .. recv_task.name .. ", data=" .. send_evt.data .. "\n")

   -- Mark the active event and transfer data
   recv_task.selected = recv_evt
   send_task.selected = send_evt
   recv_evt.data = send_evt.data

   -- Run recv task first.  This makes traces easier to read because
   -- send/recv pairs will look like function calls, but beware that
   -- tasks cannot rely on this property.  Tasks are added to the hot
   -- list again if they are still active.
   if recv_task:resume() then self:add_hot(recv_task) end
   if send_task:resume() then self:add_hot(send_task) end
end

local function other_direction(dir)
   if dir == "send" then return "recv" else return "send" end
end

function csp.scheduler:schedule_task(hot_task)
   -- log("schedule_task: " .. hot_task.name .. "\n")

   assert(hot_task.events)
   for i, hot_evt in ipairs(hot_task.events) do
      -- log("hot_evt " .. i .. "\n")
      -- log_desc(hot_evt)
      assert(hot_evt.channel)
      local waiting = hot_evt.channel[other_direction(hot_evt.direction)]
      -- From all te tasks that are waiting on this channel in this
      -- direction, we just need to pick a random one.
      for cold_task, cold_evt in pairs(waiting) do
         -- Remove from cold list before resuming, as resume will
         -- change the event structure, and remove_cold iterates over
         -- the current events.
         self:remove_cold(cold_task)
         if cold_evt.direction == "recv" then
            self:rendezvous(hot_task, hot_evt, cold_task, cold_evt)
         else
            assert(cold_evt.direction == "send")
            self:rendezvous(cold_task, cold_evt, hot_task, hot_evt)
         end
         -- We're done.  Tasks have been added to the hot list again
         -- if they were still active after resume.
         return
      end
   end
   -- None of the hot_task.events has a corresponding cold task
   -- waiting, so this becomes a cold task.
   self:add_cold(hot_task)
end

function csp.scheduler:schedule()
   while true do
      local hot_task = table.remove(self.hot_task_list)
      if not hot_task then break end
      self:schedule_task(hot_task)
   end
end

-- Start a task.
function csp.scheduler:spawn(body, init)
   local t = csp.task.spawn(body, init, self)
   self:add_hot(t)
   self:schedule()
   return t
end



-- TASK

-- Tasks implemented as Lua coroutines.

-- Name is optional.  It is only used for debug prints.
function csp.task.spawn(body, t, scheduler)
   if t == nil then t = {} end
   t.events = {}
   t.scheduler = scheduler
   setmetatable(t, { __index = csp.task })
   t.coroutine = coroutine.create(function() body(t) end)
   t:resume()
   return t
end

function csp.task:resume()
   local co = self.coroutine
   -- FIXME: distinguish between end and crash
   local status, rv = coroutine.resume(co)
   if not status then
      local statusmsg = coroutine.status(co)
      local traceback = debug.traceback(co)
      log("coroutine_resume: status=" .. statusmsg .. "\n")
      log(rv .. "\n") -- error message
      log(traceback)
      log("\n")
   end
   return status
end


-- This is the primitive task-blocking call.  The argument list is a
-- list of events that can wake up the task.  Exactly one event will
-- wake up the task, and that event is returned to the caller.
function csp.task:select(events)
   self.selected = nil
   self.events = events
   -- log("yield: dir=" .. evt.direction .. ",ch=" .. evt.channel .. "\n")
   coroutine.yield()
   -- log("resume: data=" .. self.selected.data .. "\n")  -- note this is also still there for send
   assert(self.selected)
   return self.selected
end

-- Single event send/receive and RCP are common.
function csp.task:send(channel, data)
   self:select({{
         data = data,
         direction = "send",
         channel = channel}})
end
function csp.task:recv(channel)
   return
      self:select({{
            direction = "recv",
            channel = channel}}).data
end
function csp.task:call(channel, request)
   self:send(channel, request)
   return self:recv(channel)
end




-- I/O

-- Sending an event into the CSP network requires a task structure and
-- an event to represent a blocking send operation.  Think of that
-- task as a buffer.  The task doesn't need to be a coroutine, it just
-- needs to implement the interface expected by the scheduler (resume
-- and events members).

-- Note that sending multiple events to the same channel this way does
-- not guarantee order.  We're effectively spawning a task for each
-- message.

function csp.scheduler:push(channel, data)
   local event = {
      channel = channel,
      direction = "send",
      data = data,
   }
   local task = {
      -- The only function of resume is to return false, which will
      -- ensure the wrapper task will not be rescheduled.
      resume = function() return false end,
      events = { event }
   }
   self:add_hot(task)
   self:schedule()
end

-- To implement delivery such that a single reader will always receive
-- messages in the order they were pushed, we can use a queue and a
-- queue manager task that listens for notification and send at the
-- same time.

function csp.pusher:push(data)
   table.insert(self.queue, data)
   self.scheduler:push(self.notify_channel, "notify")
end
local function pusher_body(task, p)
   while true do
      -- It's simplest to put the handler in the event struct.  If
      -- the notify_evt wakes us up, we don't need to do anything
      -- else than going through the loop again.  If it was the
      -- send, then we can remove the element we sent out.
      local evts = {}
      table.insert(
         evts, {
            handle = function()
               log("recv notify\n")
            end,
            direction = "recv",
            channel = p.notify_channel
      })
      if #(p.queue) > 0 then
         table.insert(
            evts, {
               handle = function()
                  log("sent data\n")
                  table.remove(p.queue)
               end,
               direction = "send",
               channel = p.out_channel,
               data = p.queue[1]
         })
      end
      local evt = task:select(evts)
      evt.handle()
   end
end

function csp.scheduler:new_pusher(out_channel, p)
   if p == nil then p = {} end
   p.scheduler = self
   p.queue = {}
   p.out_channel = out_channel
   p.notify_channel = self:new_channel()
   setmetatable(p, { __index = csp.pusher })
   p.task = self:spawn(function(task) pusher_body(task, p) end)
   return p
end




-- Blocking output can be implemented in a similar way on top of
-- evented output if necessary.

-- I/O chunking is probably best implemented as a coroutine.



return csp
