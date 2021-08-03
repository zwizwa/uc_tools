-- FIXME: This requires a link from .so to the .dynamic.host.so in linux/
local cmod = require("lib.elfutils_lua51")

local elfutils = {}

-- It's simpler to define these in Lua and then use a generic method.
-- See dwarf.h for definitions.
elfutils.DW_AT = {
   location = 0x02,
   name = 0x03,
   type = 0x49,
}

function elfutils.test()
   io.stdout:write(cmod.name() .. "\n")
   local DW_AT = elfutils.DW_AT

   local filename = "/i/constell8/rdm-bridge/stm32f103/main_nano.x8.f103.elf"
   -- local filename = "/bin/bash"
   local elf = cmod.open(filename)

   print(getmetatable(elf))

   local addr = cmod.sym2addr(elf, "start")
   print(addr)
   local sym = cmod.addr2sym(elf, addr)
   print(sym)


   -- cmod.doodle(elf)
   local die = cmod.sym2die(elf, "poll_functions")
   print(die)
   cmod.die_log(elf, die)
   print(cmod.die_attr(die, DW_AT.type))
   print(cmod.die_attr(die, DW_AT.name))
   print(string.format("%x", cmod.die_attr(die, DW_AT.location)))

end


return elfutils



