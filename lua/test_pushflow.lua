#!./lua.sh

local prompt = require('prompt')
local function log(str) io.stderr:write(str) end
local function log_desc(thing) log(prompt.describe(thing)) end

log("test_dataflow.lua begin\n")

local pushflow = require('lib.pushflow')


function test()
   local s = pushflow.scheduler.new()
   local n = {};

   n.in1 = s:new_node({}, {name = "in1"})
   n.in2 = s:new_node({}, {name = "in2"})
   n.in3 = s:new_node({}, {name = "in3"})
   n.out1 = s:new_node(
      {n.in1, n.in2},
      {name = "out1",
       update = function(a,b) return a+b end,
       notify = function(val) log("out1 = " .. val .. "\n") end})
   n.out2 = s:new_node(
      {n.in3, n.out1},
      {name = "out2",
       update = function(a,b) return a+b end,
       notify = function(val) log("out2 = " .. val .. "\n") end})

   local function push(name,val)
      log("- push " .. name .. " " .. val .. "\n")
      n[name]:push(val)
   end

   push("in1", 1)
   push("in2", 2)
   push("in3", 3)

   push("in1", 4)
   push("in2", 5)
   push("in3", 6)

   -- Test the absence of propagation if value did not change.
   push("in3", 6)

end

test()
log("test_pushflow.lua end\n")

