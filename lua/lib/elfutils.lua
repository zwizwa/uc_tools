-- FIXME: This requires a link from .so to the .dynamic.host.so in linux/
local cmod = require("lib.elfutils_lua51")

assert("elfutils_lua51" == cmod.name())

local elfutils = {}

elfutils.DW_AT = cmod.make_DW_AT()


function elfutils.open(filename)
      return cmod.open(filename)
end

elfutils.sym2addr = cmod.sym2addr
elfutils.addr2sym = cmod.addr2sym
elfutils.sym2die  = cmod.sym2die

elfutils.die_attr = cmod.die_attr

elfutils.die_log  = cmod.die_log

return elfutils



