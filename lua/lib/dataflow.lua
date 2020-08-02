-- Dataflow scheduler
--
-- There are 2 main architectures for this:
-- . pull-style (incremental build, e.g. make, redo)
-- . push-style (event propagator)
--
-- I'm currently not yet clear on what would be most useful.  The one
-- I use must is a pull-style redo clone which is actually used as a
-- push-style, but implemented as push(ignored_input) =
-- pull(all_targets).  The redo clone's main feature is a dynamic
-- dependency graph, which is not really what I'm looking for here.
--
-- Push-style requires an inverted dependency graph in addition to a
-- normal dependency graph, which is something you don't want to
-- create manually.
--
-- This is what I want:
--
-- . A static push-style system, implemented as simple as possible for
--   use on a micrcontroler, with datastructures generated at compile
--   time, and the only user C code consisting of pure functions.
--
-- . If possible, a way to invert the dependency graph dynamically,
--   still keeping it simple in C.



-- The realization is that in a push type system, both push and pull
-- are necessary.
--
-- Traversal consists of two parts:
--
-- . If we have the value of a node (and thus also its entire
--   dependency tree), compute the value of all the nodes that depend
--   on it.
--
-- . To compute the value of a node, compute the value of all its
--   dependencies and then execute the node's function.

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

   self.value = value

   -- Propagation goes in two phases.  First invalidate all
   -- dependencies.
   for node, _true in pairs(self.rev_deps) do
      node:invalidate()
   end

   -- Then sequence a pull for all dependencies.
   for node, _true in pairs(self.rev_deps) do
      node:pull()
      node:push(node.value)
   end

   -- This implements the "reactive output" of a network.
   if self.value and self.notify then
      self.notify(self.value)
   end
end

-- Invalidate causes subsequent pull to re-evaluate.
function dataflow.node:invalidate()
   self.value = nil
end

-- Pull recurses from a result node down the dependency graph using
-- forward dependencies at each node.  The aborts are caused by
-- incomplete networks, e.g. not all inputs are present.
function dataflow.node:pull()

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
      local val = dep:pull()
      -- Abort: input not valid.
      if not val then return nil end
      table.insert(args, val)
   end

   -- Evaluate
   -- if self.name then log("update " .. self.name .. "\n")  end
   self.value = self.update(unpack(args))
   return self.value
end



return dataflow
