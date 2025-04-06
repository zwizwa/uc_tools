-- Dataflow scheduler
--
-- There are 2 main architectures for this:
-- . pull-style (incremental build, e.g. make, redo)
-- . push-style (event propagator)
--
-- I'm currently most interested in a push-style network, as it fits
-- better in an event-driven architecture.  For an example of a
-- pull-style network, see the Erlang redo implementation in
-- erl_tools.
--
-- This implementation assumes node update is pure, allowing for
-- propagation to stop if a node's value did not change.

local prompt = require('prompt')
local function log(str) io.stderr:write(str) end
local function log_desc(thing) log(prompt.describe(thing)) end


local dataflow = { scheduler = {}, node = {} }

-- SCHEDULER

-- There doesn't seem to be a need for a scheduler object yet as all
-- connectivity is collected in the node objects.  Leave it here
-- though to later store the collection of input/output nodes if
-- necessary.

function dataflow.scheduler.new(s)
   if not s then s = {} end
   setmetatable(s, { __index = dataflow.scheduler })
   return s
end
function dataflow.scheduler:new_node(dep_nodes, init)
   return dataflow.node.new(self, dep_nodes, init)
end

-- NODE

function dataflow.node.new(scheduler, fwd_deps, init)
   local n = init
   if not n then n = {} end
   if not fwd_deps then fwd_deps = {} end
   n.value = nil
   n.fwd_deps = fwd_deps
   -- We don't know who depends on this node.  That will be filled in
   -- once dependent nodes are created...
   n.rev_deps = {}
   -- ... so register this node as a reverse dependency in all the
   -- forward dependencies.
   for i,dep in ipairs(fwd_deps) do
      dep.rev_deps[n] = true
   end
   setmetatable(n, { __index = dataflow.node })
   return n
end

-- Push a new value into the node.  If it actually changed, all
-- reverse dependencies need to be recomputed and any external
-- entities need to be notified of the change.
function dataflow.node:push(value)
   self.valid = true
   if self.value == value then return end
   self.value = value
   if self.notify then self.notify(self.value) end
   self:propagate()
end

-- After a node's value is changed, the effect needs to be propagated
-- through the reverse dependency graph.  Note that invalidation needs
-- to complete before evaluation.  If they would be interleaved, we
-- might use a node that would be invalidated later.  Invalidation is
-- shallow, and evaluation will result in a push and so propagate
-- recursively.
function dataflow.node:propagate()
   for node in pairs(self.rev_deps) do
      node.valid = nil
   end
   for node in pairs(self.rev_deps) do
      node:eval()
   end
end

-- Eval recurses through the forward dependency graph, i.e. the other
-- direction of push.  The result is memoized, i.e. only computed once
-- per push cycle.
function dataflow.node:eval()

   -- Cache
   if self.valid then return self.value end

   -- Abort: this is an input node
   if not self.update then return nil end

   -- Gather dep values
   local args = {}
   for i,dep in ipairs(self.fwd_deps) do
      local val = dep:eval()
      -- Abort: input not valid.
      if not val then return nil end
      table.insert(args, val)
   end

   -- Evaluate
   self:push(self.update(unpack(args)))
   return self.value
end


-- Remove a node from the network.  Note that this is only valid if
-- this is an output node, i.e. no nodes depend on this node.
function dataflow.node:remove()
   for i, node in ipairs(self.fwd_deps) do
      node.rev_devps[self] = nil
   end
end

return dataflow
