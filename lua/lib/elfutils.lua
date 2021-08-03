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
elfutils.metatables = {}

local function inv_table(tab)
   local inv_tab = {}
   for name,code in pairs(tab) do
      inv_tab[code] = name
   end
   return inv_tab
end

local function setup_tables()
   elfutils.DW_AT = C.get_DW_AT()
   elfutils.inv_DW_AT = inv_table(elfutils.DW_AT)

   elfutils.DW_TAG = C.get_DW_TAG()
   elfutils.inv_DW_TAG = inv_table(elfutils.DW_TAG)

   elfutils.metatables = C.get_metatables()
end

setup_tables()
local DW_AT = elfutils.DW_AT
local inv_DW_AT = elfutils.inv_DW_AT

local DW_TAG = elfutils.DW_TAG
local inv_DW_TAG = elfutils.inv_DW_TAG



function elfutils.open(filename)
      return C.open(filename)
end

-- Unpack a DIE
local function die_unpack_memoize(die, nodes)
   -- The DIE reference structure is a graph, so we need to memoize.
   --
   -- FIXME: ID is only unique inside a compilation unit, but that
   -- seems to be ok.
   local id = C.die_cuoffset(die)
   assert(id)
   local already_have = nodes[id]
   if already_have then
      return already_have
   end
   local node = { }

   -- Unpack the tag and attributes into the die table. Previously
   -- there was an 'attrs' sub table for the latter to distiguish from
   -- 'children' but the structure is deep enough already.

   node.tag = inv_DW_TAG[C.die_tag(die)]
   assert(node.tag)

   local attrs = node
   nodes[id] = node
   -- Attributes
   local attr_list = C.die_attr_list(die)
   for i, at_code in ipairs(attr_list) do
      local at_name = inv_DW_AT[at_code]
      if at_name ~= 'sibling' then
         if not at_name then
            error("at_code " .. at_code .. "not supported")
         end
         local at_val = C.die_attr(die, at_code)
         if (elfutils.metatables.die == getmetatable(at_val)) then
            at_val = die_unpack_memoize(at_val, nodes)
         end
         attrs[at_name] = at_val
      end
   end
   -- Children
   local child = C.die_child(die)
   while child do
      if not node.children then node.children = {} end
      table.insert(node.children, die_unpack_memoize(child, nodes))
      child = C.die_sibling(child)
   end

   return node
end

function elfutils.die_unpack(die)
   return die_unpack_memoize(die, {})
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


-- FIXME: Some deref tools are going to be necessary.  E.g. add a
-- metatable to the expanded lua table such that it becomes possible
-- to recursively expand, map the lua dotted access to die/attr
-- parsing in the C lib.

-- Or just always recursively unpack.



return elfutils



