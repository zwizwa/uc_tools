-- Same mechanism as csp.c

-- Initially the idea was to expose csp.c to Lua.  However, the meat
-- of the implementation really is in the data structures, and if Lua
-- is available, it makes a lot more sense to just use Lua
-- datastructures for everything.  So this is a re-implementation of
-- chedule_task() csp.c with some more general data structures:
--
-- . Use "send" and "recv" to tag directions
-- . Use symbolic channel names
-- . Create channel index on-demand
-- . The main data structure is:
--   cold_tasks[channel][direction][cold_task] -> cold_event

-- Note that task and scheduler are orthogonal and communicate only
-- through a small interface.  E.g. the scheduler does not know that
-- the tasks are coroutines, and other mechanisms could be added.


local prompt = require('prompt')
local function log(str) io.stderr:write(str) end
local function log_desc(thing) log(prompt.describe(thing)) end

local csp = {
   scheduler = {},
   task = {}
}

-- SCHEDULER


function csp.scheduler.new()
   local s = {
      -- Just a list of tasks
      hot_task_list = {},
      -- This is indexed hierarchically by channel, direction and
      -- task, eventually mapping to event.  The structure reflects
      -- the most convenient access method.  See use of
      -- scheduler:waiting
      cold_task_index = {}
   }
   setmetatable(s, { __index = csp.scheduler })
   return s
end

-- Channels are just names that are used to index cold tasks.  They
-- are created on-demand.  Note that channels are not garbage
-- collected yet.
function csp.scheduler:waiting(ch,dir)
   if not self.cold_task_index[ch] then
      self.cold_task_index[ch] = { send = {}, recv = {} }
   end
   return self.cold_task_index[ch][dir]
end

function csp.scheduler:add_cold(cold_task)
   -- log("add_cold: " .. cold_task.name .. "\n")
   cold_task.selected = nil
   for i,evt in ipairs(cold_task.events) do
      local waiting = self:waiting(evt.channel, evt.direction)
      waiting[cold_task] = evt
   end
end

function csp.scheduler:remove_cold(cold_task)
   for i,evt in ipairs(cold_task.events) do
      local waiting = self:waiting(evt.channel, evt.direction)
      waiting[cold_task] = nil
      -- FIXME: Where to garbage-collect channels?  This seems to be a
      -- possible place, but not optimal.  E.g. if both hot and cold
      -- list are empty, the channel can be removed entirely, but this
      -- will perform a lot of on-demand creation of the chan-to_evt
      -- table.  Maybe once per scheduler run is better.  Or once
      -- every n runs.
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

      local waiting = self:waiting(
         hot_evt.channel, other_direction(hot_evt.direction))
      -- From all te tasks that are waiting on this channel in this
      -- direction, we just need to pick a random one.
      for cold_task, cold_evt in pairs(waiting) do
         -- Remove from cold list before resuming, as resume will
         -- change the event structure, and remove_cold iterates over
         -- the current events.
         self:remove_cold(cold_task)
         if cold_dir == "recv" then
            self:rendezvous(hot_task, hot_evt, cold_task, cold_evt)
         else
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
function csp.scheduler:spawn(body, name)
   local t = csp.task.spawn(body, name, self)
   self:add_hot(t)
   self:schedule()
   return t
end



-- TASK

-- Tasks implemented as Lua coroutines.

-- Name is optional.  It is only used for debug prints.
function csp.task.spawn(body, scheduler, name)
   local t = { events = {}, scheduler = scheduler, name = name }
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


-- Implement single event send/receive first.  Then find a good api
-- for select later.
function csp.task:send(channel, data)
   self:select({
         data = data,
         direction = "send",
         channel = channel})
end

function csp.task:recv(channel)
   return
      self:select({
            direction = "recv",
            channel = channel}).data
end

-- This is the main (only) blocking call that a task can issue.  The
-- argument list is a list of events that can wake up the task.
-- Exactly one event will eventually wake up the task, and that event
-- is returned to the caller.  Currently only two types of events are
-- supported: channel send and recv.  It is probably best to keep it
-- that way.  External events can be simulated using channels.  See
-- csp.c for inspiration.

-- Select already supports multiple blocking events.
function csp.task:select(...)
   self.selected = nil
   self.events = { ... }
   -- log("yield: dir=" .. evt.direction .. ",ch=" .. evt.channel .. "\n")
   coroutine.yield()
   -- log("resume: data=" .. self.selected.data .. "\n")  -- note this is also still there for send
   return self.selected
end







return csp
