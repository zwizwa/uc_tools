local log_parse = require('lib.log_parse')



local function log(str)
   io.stderr:write(str)
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

test1()
test1()



-- Use mmap file
local function test2()
   local gen = log_parse.lines("test.trace")
   -- Print the first 10 messages
   for i=1,10 do
      log(gen())
   end
end

local function test3()
   for i=1,3 do
      log("iteration " .. i .. "\n")
      test2()
   end
end

test3()
