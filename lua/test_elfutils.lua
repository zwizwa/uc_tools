#!/usr/bin/lua
local elfutils = require("lib.elfutils")

local prompt = require('prompt')
local function log(str) io.stderr:write(str) end
local function log_desc(thing) log(prompt.describe(thing)) end

log_desc(elfutils.DW_AT)


-- FIXME: This is currently specific to proprietary elf.
function test()
   local DW_AT = elfutils.DW_AT

   local filename = "/i/constell8/rdm-bridge/stm32f103/main_nano.x8.f103.elf"
   -- local filename = "/bin/bash"
   local elf = elfutils.open(filename)

   print(getmetatable(elf))

   local addr = elfutils.sym2addr(elf, "start")
   print(addr)
   local sym = elfutils.addr2sym(elf, addr)
   print(sym)


   -- elfutils.doodle(elf)
   local die = elfutils.sym2die(elf, "poll_functions")
   print(die)
   elfutils.die_log(elf, die)
   print(elfutils.die_attr(die, DW_AT.type))
   print(elfutils.die_attr(die, DW_AT.name))
   print(string.format("%x", elfutils.die_attr(die, DW_AT.location)))

end

test()

