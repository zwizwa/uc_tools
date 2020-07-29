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

local prompt = require('prompt')
local function log(str) io.stderr:write(str) end
local function log_desc(thing) log(prompt.describe(thing)) end

local csp = {
   scheduler = {},
   task = {}
}

local function other_direction(dir)
   if dir == "send" then return "recv" else return "send" end
end

function csp.scheduler.new()
   local s = {
      -- Just a list of tasks
      hot_task_list = {},
      -- This is indexed hierarchically by channel, direction and
      -- task, eventually mapping to event.  The structure reflects
      -- the most convenient access method.  See use of
      -- scheduler:cold_tasks
      cold_task_index = {}
   }
   setmetatable(s, { __index = csp.scheduler })
   return s
end

-- This is just an illustration... It's probably simpler to define the
-- event structure in-place, as part of select().  As opposed to csp.c
-- it is probably possible to give a decent api here.
-- function event.new()
--    local event = { direction = "send", channel = "channel0" }
--    return event
-- end



-- Channels are just names that are used to index cold tasks.  They
-- are created on-demand.  Note that channels are not garbage
-- collected.
function csp.scheduler:channel_cold_tasks(ch,dir)
   if not self.cold_task_index[ch] then
      self.cold_task_index[ch] = { send = {}, recv = {} }
   end
   return self.cold_task_index[ch][dir]
end

function csp.scheduler:add_hot(hot_task)
   table.insert(self.hot_task_list, hot_task)
end

function csp.scheduler:add_cold(cold_task)
   -- log("add_cold: " .. cold_task.name .. "\n")
   cold_task.selected = nil
   for i,evt in ipairs(cold_task.events) do
      local waiting = self:channel_cold_tasks(evt.channel, evt.direction)
      waiting[cold_task] = evt
   end
end

function csp.scheduler:remove_cold(cold_task)
   for i,evt in ipairs(cold_task.events) do
      local waiting = self:channel_cold_tasks(evt.channel, evt.direction)
      waiting[cold_task] = nil
      -- FIXME: Where to garbage-collect channels?  This seems to be a
      -- possible place, but not optimal.  E.g. if both hot and cold
      -- list are empty, the channel can be removed entirely, but this
      -- will perform a lot of on-demand creation of the chan-to_evt
      -- table.  Maybe once per scheduler run is better.  Or once
      -- every n runs.
   end
end

function csp.scheduler:do_send(send_task, send_evt, recv_task, recv_evt)
   -- log("do_send send=" .. send_task.name .. ", recv=" .. recv_task.name .. ", data=" .. send_evt.data .. "\n")

   recv_task.selected = recv_evt
   send_task.selected = send_evt
   recv_evt.data = send_evt.data

   -- Add both to hot list again if still active
   if recv_task:resume() then self:add_hot(recv_task) end
   if send_task:resume() then self:add_hot(send_task) end
end

function csp.scheduler:schedule_task(hot_task)
   -- log("schedule_task: " .. hot_task.name .. "\n")

   assert(hot_task.events)
   for i, hot_evt in ipairs(hot_task.events) do
      -- log("hot_evt " .. i .. "\n")

      local waiting = self:channel_cold_tasks(
         hot_evt.channel, other_direction(hot_evt.direction))
      -- ch_dir is a task -> event table
      -- we need to pick a random key,value pair from this list
      for cold_task, cold_evt in pairs(waiting) do
         self:remove_cold(cold_task)
         if cold_dir == "recv" then
            self:do_send(hot_task, hot_evt, cold_task, cold_evt)
         else
            self:do_send(cold_task, cold_evt, hot_task, hot_evt)
         end
         -- We're done
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


-- Implement single send/receive first.  Then find a good api for
-- select later.
function csp.task:send(channel, data)
   return
      self:select({
            data = data,
            direction = "send",
            channel = channel})
end

function csp.task:recv(channel)
   return
      self:select({
            direction = "recv",
            channel = channel})
end

function csp.task:select(evt)
   self.selected = nil
   self.events = { evt }
   -- log("yield: dir=" .. evt.direction .. ",ch=" .. evt.channel .. "\n")
   coroutine.yield()
   -- log("resume: data=" .. self.selected.data .. "\n")  -- note this is also still there for send
   return self.selected.data
end

local function coroutine_resume(co)
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

function csp.task:resume()
   return coroutine_resume(self.coroutine)
end

function csp.scheduler:spawn(body, name)
   local t = { events = {}, scheduler = self, name = name }
   setmetatable(t, { __index = csp.task })
   -- The coroutine body executes up to the first blocking point,
   -- e.g. the first self:select() call.
   t.coroutine = coroutine.create(function() body(t) end)
   coroutine_resume(t.coroutine)
   -- Call scheduler to propagate events.
   self:add_hot(t)
   self:schedule()
   return t
end



return csp
