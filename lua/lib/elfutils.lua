-- Logging.
local prompt
local function log(str)
   io.stderr:write(str)
end
local function log_desc(thing)
   if not prompt then prompt = require('prompt') end
   log(prompt.describe(thing))
end

-- FIXME: This requires a link from .so to the .dynamic.host.so in linux/
local C = require("lib.elfutils_lua51")

assert("elfutils_lua51" == C.name())

local elfutils = {}

-- Keep the C functions collected in a sub table.  These are
-- considered to be internal functions, such that we can optimize for
-- simple C code, then write Lua wrappers for data structure
-- manipulation.
elfutils.C = C

local function setup_tables()
   elfutils.DW_AT = C.make_DW_AT()
   elfutils.inv_DW_AT = {}
   for name,code in pairs(elfutils.DW_AT) do
      elfutils.inv_DW_AT[code] = name
   end
end

setup_tables()
local DW_AT = elfutils.DW_AT
local inv_DW_AT = elfutils.inv_DW_AT



function elfutils.open(filename)
      return C.open(filename)
end

-- Unpack a DIE
-- FIXME: Is it important to keep the order of the attributes?
function elfutils.die_attrs(die)
   local attr_list = C.die_attr_list(die)
   local attrs = {}
   for i, at_code in ipairs(attr_list) do
      local at_name = inv_DW_AT[at_code]
      if not at_name then
         error("at_code " .. at_code .. "not supported")
      end
      local at_val = C.die_attr(die, at_code)
      attrs[at_name] = at_val
   end
   return attrs
end


-- The holy grail: given just a name, figure out how to dump a
-- target's data structure as a Lua structure, using type and location
-- information from the ELF.  Basically emulating GDB's print
-- function.
function elfutils.read_variable(elf, name)
   local die = C.sym2die(elf, name)
   local typ = C.die_attr(die, DW_AT.type)
   log_desc({die = die, typ = typ})

   return {a = {b = 123}}
end



return elfutils



