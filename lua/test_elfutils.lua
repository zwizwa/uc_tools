#!./lua.sh

local elfutils = require("lib.elfutils")

-- Logging.
local prompt
local function log(str)
   io.stderr:write(str)
end
local function log_desc(thing)
   if not prompt then prompt = require('prompt') end
   log(prompt.describe(thing))
   log("\n")
end




-- FIXME: This is currently specific to proprietary elf.
function test()
   -- log_desc(elfutils)

   local DW_AT = elfutils.DW_AT
   local inv_DW_AT = elfutils.inv_DW_AT

   -- log_desc(DW_AT)
   -- log_desc(inv_DW_AT)

   local filename = "/i/constell8/rdm-bridge/stm32f103/main_nano.x8.f103.elf"
   -- local filename = "/bin/bash"
   local elf = elfutils.open(filename)

   if false then
      local elf_meta = getmetatable(elf)
      log_desc({elf_meta = elf_meta})

      local addr = elfutils.C.sym2addr(elf, "start")
      print(addr)
      local sym = elfutils.C.addr2sym(elf, addr)
      print(sym)
   end


   -- elfutils.doodle(elf)

   if false then

      local die = elfutils.C.sym2die(elf, "poll_functions")
      print(die)

      log_desc({die_attr_list = elfutils.C.die_attr_list(die)})

      print(elfutils.C.die_attr(die, DW_AT.type))
      print(elfutils.C.die_attr(die, DW_AT.name))
      print(string.format("%x", elfutils.C.die_attr(die, DW_AT.location)))

      local die_attrs = elfutils.die_attrs(die)
      log_desc({die_attrs = die_attrs})
      local die_attrs_type = elfutils.die_attrs(die_attrs.type)
      log_desc({die_attrs_type = die_attrs_type})
      local die_attrs_type_type = elfutils.die_attrs(die_attrs_type.type)
      local die_attrs_type_sibling = elfutils.die_attrs(die_attrs_type.sibling)
      log_desc({die_attrs_type_type = die_attrs_type_type})
      log_desc({die_attrs_type_sibling = die_attrs_type_sibling})
   end

   if false then
      log_desc({test_recursive_die =
                   elfutils.die_unpack(
                      elfutils.C.sym2die(elf, "dmx_node_struct"))})
   end

   -- test the holy grail
   local nb_poll_functions = elfutils.read_variable(elf, "nb_poll_functions")
   -- log_desc({nb_poll_functions = nb_poll_functions})
   log_desc(nb_poll_functions)

   log_desc(elfutils.read_array(elf, "poll_functions", 2))

end

test()

