-- See ns_actor.h actor.h test_actor.c

local prompt = require('prompt')
local function log(str) io.stderr:write(str) end
local function log_desc(thing) log(prompt.describe(thing)) end

local actor = { scheduler = {}, task = {} }

function actor.scheduler.new()
   local s = { hot = {}, cold = {} }
   setmetatable(s, { __index = actor.scheduler })
   return s
end

function actor.scheduler:schedule()
   while self:schedule_one() do end
end

function actor.scheduler:schedule_one()
   for task,_true in pairs(self.hot) do
      -- move from hot to cold before resuming
      self:make_cold(task)
      if not task:resume() then
         self:make_dead(task)
      end
      return true
   end
   return false
end

function actor.scheduler:make_hot(task)
   self.hot[task] = true
   self.cold[task] = nil
end
function actor.scheduler:make_cold(task)
   self.hot[task] = nil
   self.cold[task] = true
end
function actor.scheduler:make_dead(task)
   self.hot[task] = nil
   self.cold[task] = nil
end

function actor.scheduler:spawn(body, init)
   local task = actor.task.new(self, body, init)
   self:make_hot(task)
   return task
end

function actor.scheduler:send(task, msg)
   table.insert(task.mbox, msg)
   self:make_hot(task)
end

function actor.task.new(scheduler, body, t)
   if not t then t = {} end
   t.mbox = {}
   t.scheduler = scheduler
   setmetatable(t, { __index = actor.task })
   t.coroutine = coroutine.create(function() body(t) end)
   return t
end

function actor.task:resume()
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

function actor.task:halt()
   self.scheduler:make_dead(self)
   return
end

-- Receive blocks, send doesn't
function actor.task:send(task, msg)
   self.scheduler:send(task, msg)
end
function actor.task:recv()
   while #self.mbox == 0 do
      coroutine.yield()
   end
   return table.remove(self.mbox, 1)
end


return actor
