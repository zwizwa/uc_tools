-- Same mechanism as csp.c

-- Initially the idea was to expose csp.c to Lua.  However, the meat
-- of the implementation really is in the data structures, and if Lua
-- is available, it makes a lot more sense to just use Lua
-- datastructures for everything, and rely on garbage collection.  So
-- this is a re-implementation of schedule_task() from csp.c with some
-- more general data structures:
--
-- . Use channel objects and have them contain the cold lists
-- . Use "send" and "recv" symbols to tag directions
-- . Implement tasks using coroutines

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
   event_list = {},
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

function csp.scheduler:resume(task)
   local resume = task.resume
   local rv = resume(task)
   if rv then
      -- Tasks can signal halt via return value of resume.  If not
      -- halted, we need to add it to the hot list to reschedule,
      -- as another rendez-vous might be possible.
      self:add_hot(task)
   end
end

function csp.scheduler:rendezvous(send_task, send_evt, recv_task, recv_evt)
   -- log("rendezvous send=" .. send_task.name .. ", recv=" .. recv_task.name .. ", data=" .. send_evt.data .. "\n")

   -- Mark the active event and transfer data
   recv_task.selected = recv_evt
   send_task.selected = send_evt
   recv_evt.data = send_evt.data

   -- Run recv task first.  This makes traces easier to read because
   -- send/recv pairs will look like function calls, but beware that
   -- tasks cannot rely on this property.
   self:resume(recv_task)
   self:resume(send_task)
end

local send = "send"
local recv = "recv"

local function other_direction(dir)
   if dir == send then
      return recv
   else
      assert(dir == recv)
      return send
   end
end

function csp.scheduler:schedule_task(hot_task)
   -- log("schedule_task: " .. hot_task.name .. "\n")

   assert(hot_task.events)
   for i, hot_evt in ipairs(hot_task.events) do
      -- log("hot_evt " .. i .. "\n")
      -- log_desc(hot_evt)
      assert(hot_evt.channel)
      local cold_dir = other_direction(hot_evt.direction)
      local waiting = hot_evt.channel[cold_dir]
      -- From all te tasks that are waiting on this channel in this
      -- direction, we pick an arbitrary one: the first one that shows
      -- up in the iterator.
      for cold_task, cold_evt in pairs(waiting) do
         -- Remove from cold list before resuming, as resume will
         -- change the event structure, and remove_cold iterates over
         -- the current events.
         self:remove_cold(cold_task)
         if cold_evt.direction == recv then
            self:rendezvous(hot_task, hot_evt, cold_task, cold_evt)
         else
            assert(cold_evt.direction == send)
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

-- For selecting from multiple events, it is not clear what a good API
-- would be.  Trying out some shorthand.  This abstraction is
-- stateful, which makes it easier to conditionally build up an event
-- list.  See use in new_send_queue.
function csp.event_list:add_send(channel, data)
   table.insert(self.evts, { channel = channel, direction = send, n = #self.evts + 1, data = data })
end
function csp.event_list:add_recv(channel)
   table.insert(self.evts, { channel = channel, direction = recv, n = #self.evts + 1 })
end
function csp.event_list:select()
   return self.task:select(self.evts)
end
function csp.task:new_event_list()
   local l = { evts = {}, task = self }
   setmetatable(l, { __index = csp.event_list })
   return l
end


-- Single event send/receive and RPC are common, so provide
-- abstractions.  This illustrates how to use the event datastructures
-- directly.
function csp.task:send(channel, data)
   self:select({{
         data = data,
         direction = send,
         channel = channel}})
end
function csp.task:recv(channel)
   return
      self:select({{
            direction = recv,
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

-- Notes:
--
-- . Delivery is guaranteed to be ordered as long as the task on the
--   other side is always listening on this channel.
--
-- . If the other side is not listening (e.g. blocked on some other
--   event) and a new message is pushed, now two temporary tasks are
--   active, and it is not defined in which will be able to write
--   first.  To deal with this scenario, see the send_queue
--   abstraction below.

function csp.scheduler:push(channel, data)
   local event = {
      channel = channel,
      direction = send,
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



-- Async message queue / mailbox. scheduler:push is guaranteed to not
-- block. I.e. the task at the other end of the in_channel is always
-- listening, and will put messagaes in an internal queue to await
-- transfer to out_channel.

function csp.scheduler:new_send_queue(out_channel)
   local in_channel = self:new_channel()
   self:spawn(
      function(task)
         local queue = {}
         while true do
            local l = task:new_event_list()
            l:add_recv(in_channel)
            if #queue > 0 then
               l:add_send(out_channel, queue[1])
            end
            local evt = l:select()
            if evt.channel == out_channel then
               table.remove(queue, 1)
            else
               table.insert(queue, evt.data)
            end
         end
      end)
   local q = {}
   function q.push(_, data)
      self:push(in_channel, data)
   end
   return q
end




-- Blocking output can be implemented in a similar way on top of
-- evented output if necessary.

-- I/O chunking is probably best implemented as a coroutine.



return csp
