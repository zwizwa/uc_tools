local log_parse_lua51 = require('log_parse_lua51')

local parse = log_parse_lua51.new_log_parse()

local function pack(...)
   return {...}
end

local function test()
local rv = pack(
log_parse_lua51.log_parse_to_string(
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
   io.write(i .. " " .. v)
end
end

test()
test()

