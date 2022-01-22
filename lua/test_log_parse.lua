local log_parse = require('lib.log_parse')
local prompt = require('prompt')


local function log(str)
   io.stderr:write(str)
end
local function log_desc(thing)
   log(prompt.describe(thing))
   log('\n')
end

-- Push fragment, collect all lines.
local function test1()
   local parse = log_parse.new()
   local chunk =
      "rdm\nabc\ndef\n1234def1 lala\n"

      .. string.char(0x80 + 3)

      .. string.char(1)
      .. string.char(2)
      .. string.char(3)
      .. string.char(4)

      .. string.char(5)
      .. string.char(6)
      .. string.char(7)

      .. "end\n"

   for i,v in ipairs(parse:to_strings(chunk)) do
      log(i .. " " .. v)
   end
end

-- From application.  Can't include this unfortunately.
local trace = "test.trace"

-- Use mmap file
local function test2()
   local gen = log_parse.lines(trace)
   -- Print the first 10 messages
   for i=1,10 do
      log(gen())
   end
end

local function test3()
   log("\ntest3\n")
   for i=1,3 do
      log("iteration " .. i .. "\n")
      test2()
   end
end

local function test4()
   log("\ntest4\n")
   local nb = 0
   for line in log_parse.lines(trace) do
      nb = nb + 1
   end
   log(nb .. " lines in " .. trace .. "\n")
end

local function test5()
   log("\ntest5\n")
   local nb = 0
   for ts,off,len in log_parse.indices(trace) do
      if (nb < 3) then
         log_desc({ts = ts, off = off, len = len})
      end
      nb = nb + 1
   end
   log(nb .. " lines in " .. trace .. "\n")
end


test1() ; test1()
test3()
test4()
test5()
