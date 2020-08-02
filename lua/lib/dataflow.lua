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
   -- ... so do that for this node's dependencies.
   for i,dep in ipairs(fwd_deps) do
      dep:add_rev_dep(n)
   end
   setmetatable(n, { __index = dataflow.node })
   return n
end

function dataflow.node:add_rev_dep(node)
   self.rev_deps[node] = true
end

-- Push carries the connotation that a value has been changed and all
-- dependencies need to be recomputed.  Implementation recurses from a
-- leaf node up the dependency graph using reverse dependencies at
-- each node.
function dataflow.node:push(value)
   -- if self.name then log("push " .. self.name .. "\n") end
   self:set_value(value)
   self:propagate()
end

function dataflow.node:propagate()

   -- Propagation goes in two phases.  First invalidate all
   -- dependencies without recursing.
   for node, _true in pairs(self.rev_deps) do
      node:invalidate()
   end

   -- Then sequence memoized evaluation for all dependencies and
   -- recursively propagate the change.
   for node, _true in pairs(self.rev_deps) do
      node:eval()
      node:propagate()
   end

end


-- Eval recurses from a result node down the dependency graph using
-- forward dependencies at each node.  The result is memoized,
-- i.e. only computed once per push cycle.  The aborts are caused by
-- incomplete networks, e.g. not all inputs are present.
function dataflow.node:eval()

   -- If already valid, return cached value.
   if self.value then
      -- if self.name then log("keep " .. self.name .. "\n")  end
      return self.value
   end

   -- Abort: input node without input
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
   -- if self.name then log("update " .. self.name .. "\n")  end
   self:set_value(self.update(unpack(args)))
   return self.value
end

-- Update current node's value and potentially send external
-- notification.  Note that internal propagation is handled
-- separately.
function dataflow.node:set_value(value)
   self.value = value
   if self.notify then self.notify(self.value) end
end

-- Invalidate causes subsequent eval to re-evaluate.
function dataflow.node:invalidate()
   self.value = nil
end

return dataflow
