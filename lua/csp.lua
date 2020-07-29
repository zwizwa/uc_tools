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
-- . chan_to_evt[ch][dir] is a map from task to event

local csp = {}

local function other_direction(dir)
   if dir == "send" then return "recv" else return "send" end
end

function csp:init()
   self.hot_tasks = {}
   self.chan_to_evt = {}
end

-- Channels are just names that are used to index cold tasks.  They
-- are created on-demand.  Note that channels are not garbage
-- collected.
function csp:chan_to_evt(ch)
   if not self.chan_to_evt(ch) then
      self.chan_to_evt[ch] = { send = {}, recv = {} }
   end
   return self.chan_to_evt[ch]
end

function csp:add_hot(hot_task)
   table.insert(self.hot_tasks, hot_task)
end

function csp:add_cold(cold_task)
   cold_task.selected = false
   for i,evt in ipairs(cold_task.events) do
      local chan_to_evt_ch = self:chan_to_evt(evt.channel)
      chan_to_evt_ch[evt.direction][cold_task] = evt
   end
end

function csp:remove_cold(cold_task)
   for i,evt in ipairs(cold_task.events) do
      local chan_to_evt_ch = self:chan_to_evt(evt.channel)
      chan_to_evt_ch[evt.direction][cold_task] = evt
   end
end

function csp:schedule_task(hot_task)
   for i, hot_evt in ipairs(hot_task.events) do
      local chan_to_evt_ch = self:chan_to_evt(hot_evt.channel)
      local ch_dir = chan_to_evt_ch[evt.direction]
      -- ch_dir is a task -> event table
      -- we need to pick a random key,value pair from this list
      for cold_task, cold_evt in pairs(ch_dir) do
         self:remove_cold(cold_task)
         if cold_dir == "recv" then
            self:do_send(hot_task, hot_evt, cold_task, cold_evt)
         else
            self:do_send(cold_task, cold_evt, hot_task, hot_evt)
         end
         -- Add both to hot list again if still active
         if (hot_task.resume)  then self:add_hot(hot_task)  end
         if (cold_task.resume) then self:add_hot(cold_task) end
         -- We're done
         return
      end
   end
   -- None of the hot_task.events has a corresponding cold task
   -- waiting, so this becomes a cold task.
   self:add_cold(cold)
end

function csp:do_send(send_task, send_evt, recv_task, recv_evt)
   recv.selected = recv_evt
   send.selected = send_evt
   recv.data = send.data
   -- FIXME: handle halt
   recv:resume()
   send:resume()
end

