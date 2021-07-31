#!/usr/bin/lua
elfutils = require("elfutils_lua51")

io.stdout:write(elfutils.name() .. "\n")

-- local filename = "/i/constell8/rdm-bridge/stm32f103/main_nano.x8.f103.elf")
local filename = "/bin/bash"
local elf = elfutils.open(filename)


print(getmetatable(elf))




