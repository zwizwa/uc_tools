local log_parse = require('log_parse_lua51')

local parse = log_parse.new_log_parse()

local function pack(...)
   return {...}
end

local function log(str)
   io.stderr:write(str)
end

-- Push fragment, collect all lines.
local function test1()
local rv = pack(
log_parse.log_parse_to_string(
   parse,
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
))
for i,v in ipairs(rv) do
   log(i .. " " .. v)
end
end

--test1()
--test1()

-- Use mmap file
local function test2()
   local file = log_parse.new_log_file("test.trace")
   assert(file)
   local function gen()
      return log_parse.log_parse_next(parse, file)
   end
   -- Print the first 10 messages
   for i=1,10 do
      log(gen())
   end
end

test2()
