#!/usr/bin/lua

local prompt = require('prompt')
local function log(str) io.stderr:write(str) end
local function log_desc(thing) log(prompt.describe(thing)) end

log("test_dataflow.lua begin\n")

local dataflow = require('lib.dataflow')


function test()
   local s = dataflow.scheduler.new()
   local in1 = s:new_node()
   local in2 = s:new_node()
   local out = s:new_node(
      {in1,in2},
      {update = function(a,b) return a+b end,
       notify = function(val) log("out = " .. val .. "\n") end})

   in1:push(1)
   in2:push(2)

   in1:push(3)
   in2:push(4)



end

test()
log("test_dataflow.lua end\n")

