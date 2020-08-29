-- Minimalistic Actor Model implementation based on Lua coroutines,
-- loosely inspired by Erlang and encouraged by Emacs distel.
--
-- For a usage example, see webserver.lua
-- For a C version, see ns_actor.h actor.h test_actor.c

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
      self:make_cold(task)
      if not task:resume() then
         -- If task died, make sure it is not on the hot list.
         self:make_cold(task)
      end
      return true
   end
   return false
end

-- Factored out for debugging purposes.
function actor.scheduler:make_hot(task)  self.hot[task] = true end
function actor.scheduler:make_cold(task) self.hot[task] = nil  end

function actor.scheduler:spawn(body, init)
   local task = actor.task.new(self, body, init)
   self:make_hot(task)
   return task
end

function actor.scheduler:send(task, msg)
   if task.mbox then
      table.insert(task.mbox, msg)
      self:make_hot(task)
   else
      -- A task without a mbox is a dead task.  Messages sent to dead
      -- tasks will be dropped.
      log("sending to dead task\n")
   end
end

-- Test to make sure that tasks get garbage-collected properly.  These
-- are just to give an indication during development and are left here
-- commented-out for reference.  Measurement on x64 gave 500 bytes for
-- coroutine_size.
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

function actor.task.new(scheduler, body, t)
   if not t then t = {} end
   t.mbox = {}
   t.scheduler = scheduler
   -- t.test_gc = test_gc()
   setmetatable(t, { __index = actor.task })
   --local m1 = collectgarbage('count')
   t.coroutine = coroutine.create(function() body(t) ; t:exit() end)
   --local m2 = collectgarbage('count')
   --log("coroutine_size(kb): " .. 1024*(m2-m1) .."\n")
   return t
end

function actor.task:resume()
   local co = self.coroutine
   local ok, rv = coroutine.resume(co)
   if not ok then
      log("resume->false\n")
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
      -- self:exit() will already have been called in the coroutine.
      -- See spawn.
      -- log("status: " .. coroutine.status(co) .. "\n")
   end
   return status
end

function actor.task:exit()
   -- Remove from hot list to make sure scheduler will not try to
   -- resume this task.
   self.scheduler:make_cold(self)
   -- To ensure that nothing will reschedule us through send, remove
   -- the mailbox as an indication that task is dead.  Any un-handled
   -- messages are lost.
   self.mbox = nil
   return
end

-- Receive blocks, send doesn't
function actor.task:send(msg)
   self.scheduler:send(self, msg)
end
function actor.task:recv()
   while #self.mbox == 0 do
      coroutine.yield()
   end
   return table.remove(self.mbox, 1)
end

return actor
