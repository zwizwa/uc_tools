#!/usr/bin/lua
local elfutils = require("elfutils_lua51")


function test()
   io.stdout:write(elfutils.name() .. "\n")

   local filename = "/i/constell8/rdm-bridge/stm32f103/main_nano.x8.f103.elf"
   -- local filename = "/bin/bash"
   local elf = elfutils.open(filename)

   print(getmetatable(elf))

end


test()



