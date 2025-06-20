-- This is support code for the C module elfutils_lua51.c
-- See comments in that file.

-- Very ad-hoc, many missing cases.  Implements minimal
-- functionality needed for instrumentation:


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

-- FIXME: This requires a link from .so to the .dynamic.host.so in linux/
local C = require("elfutils_lua51")

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
   if not node.tag then
      log("WARNING: bad tag: " .. (C.die_tag(die) or nil) .. "\n")
   end

   local attrs = node
   nodes[id] = node
   -- Attributes
   local attr_list = C.die_attr_list(die)
   for i, at_code in ipairs(attr_list) do
      local at_name = inv_DW_AT[at_code]
      if at_name ~= 'sibling' then
         if not at_name then
            -- Don't raise error for future compat.
            -- error("attribute code " .. at_code .. " not supported")
            -- Instead provide generic name.
            at_name = string.format("AT_0x%x", at_code)
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


-- Unpack everything
function elfutils.compilation_units(elf)
   local cu_dies = elfutils.C.compilation_units(elf)
   local unpacked = {}
   for i,cu_die in ipairs(cu_dies) do
      unpacked[i] = elfutils.die_unpack(cu_die)
   end
   return unpacked
end

function elfutils.filtermap(elf, f)
   local lst = {}
   local cu_dies = elfutils.C.compilation_units(elf)
   local unpacked = {}
   for i,cu_die in ipairs(cu_dies) do
      local cu = elfutils.die_unpack(cu_die)
      if cu.children then
         for i,die in ipairs(cu.children) do
            local val = f(die)
            if val then
               table.insert(lst, val)
            end
         end
      end
   end
   return lst
end

-- The idea is to make this as transparent as possible: give just a
-- name (and e.g. array size), and return a usable Lua structure.  We
-- go for convenience and simplicity over detailed control.  All the
-- DIE data is available so just roll your own if you need to.

-- There are 2 modes:
-- - recursive read (default)
-- - lazy read, where variables and members are represented by reader
--  functions.


-- How to red this code: The Lua data structure is fairly close to
-- DWARF with node.tag the DW_TAG_ in symbolic form, all attributes in
-- symbolic form with values converted to some Lua representation, and
-- node.children a list of DIE child+siblings.
--
-- In good tradition I'm not going to document it further.  To see how
-- this fits together, log_desc() is your friend.  That's the idea:
-- make the structure printible so examples can be used to
-- incrementally create queries instead of trying to decipher a huge
-- document.

local type_deref = {
   typedef = true,
}
local flat_types = {
   base_type = true,
   structure_type = true,
}
function flatten_type(type)
   if type_deref[type.tag] then
      return flatten_type(type.type)
   else
      assert(flat_types[type.tag])
      assert(type.byte_size)
      return type
   end
end
local base_type_map = {
   ["long unsigned int"] = "u32"
}

-- FIXME: We assume all words are little endian for now.  This is
-- probably encoded somewhere.
function read_le_word(env, addr, nb)
   assert(env)
   assert(env.read_memory)
   assert(addr)
   assert(nb)

   local bytes = env.read_memory(env, addr, nb)
   assert(bytes)
   local dir = -1  -- FIXME generalize to big-endian
   local offset = nb
   local accu = 0
   while nb > 0 do
      accu = accu * 256 + bytes[offset]
      offset = offset + dir
      nb = nb - 1
   end
   -- log(string.format("0x%x -> 0x%x\n", addr, accu))
   return accu

end


-- Methods to read data types as they are named in the DWARF
-- structure.

local type_reader = {}

local function read_type(env, type, addr)
   assert(type)
   assert(type.tag)
   local reader = type_reader[type.tag]
   if not reader then
      error("elfutils.read_type, unsupported type: " .. type.tag)
   end
   return reader(env, type, addr)
end

function type_reader.volatile_type(env, type, addr)
   assert(type.type)
   return read_type(env, type.type, addr)
end

function type_reader.typedef(env, type, addr)
   assert(type.type)
   -- Unpack the typedef
   return read_type(env, type.type, addr)
end

function type_reader.base_type(env, type, addr)
   -- log_desc(type)
   assert(type.byte_size)

   -- FIXME: Assume it's an uint.
   -- log_desc({type=type})
   return read_le_word(env, addr, type.byte_size)
end

function type_reader.pointer_type(env, type, addr)
   assert(type)
   assert(type.byte_size)

   -- log_desc({type_reader_pointer_type = type})
   local pointed_addr = read_le_word(env, addr, type.byte_size)
   -- log_desc({addr=addr,pointed=pointed})

   if env.lazy then
      local function read_it()
         local val = read_type(env, type.type, pointed_addr)
         return val
      end
      return pointed_addr, read_it
   else
      -- If not lazy, we return the pointer value.  Pointers can
      -- introduce reference loops so we cannot just read it here.
      --
      -- FIXME: Should this return type also?
      -- return pointed, type
      return pointed_addr
   end
end



local function maybe_bitfield_unpack(type, element_val)
   -- log_desc({type=type})
   if type.bit_size == nil then
      assert(type.bit_offset == nil)
      return element_val
   else
      -- log_desc({type=type})
      assert(type.bit_offset ~= nil)
      assert(element_val >= 0) -- FIXME: Implement signed
      -- logf("unpack bitfield offset=%d size=%d element_val=%d\n", type.bit_offset, type.bit_size, element_val)

      -- DW_AT_bit_offset: The bit offset attribute describes the
      -- offset in bits of the high order bit of a value of the given
      -- type from the high order bit of the storage unit used to
      -- contain that value.  ( WTF! )
      local mask  = bit.lshift(1, type.bit_size) - 1
      local shift = 8 * type.byte_size - type.bit_size - type.bit_offset
      return bit.band(mask, bit.rshift(element_val, shift))
   end
end


function type_reader.structure_type(env, type, addr)
   assert(addr)

   if type.tag ~= "structure_type" then
      log_desc(type)
      error("unexpected tag: " .. type.tag)
   end
   local children = type.children or {}
   local struct = {}
   for i,member in ipairs(children) do
      assert(member.tag == "member")
      assert(member.type)
      assert(member.name)

      local function reader()
         local element_addr = addr + member.data_member_location
         local element_val
         if env.prune_member and env.prune_member[member.name] then
            element_val = env.prune_member[member.name]
         else
            element_val = read_type(env, member.type, element_addr)
            element_val = maybe_bitfield_unpack(member, element_val)
         end


         -- log(string.format("struct 0x%x %d %s\n", element_addr, member.data_member_location, member.name))
         -- The choice here is to preserve the order and have a more
         -- verbose representation, or just use an unordered Lua table.
         -- The latter seems much more useful.
         --
         -- table.insert(struct, {member.name, element_val})
         return element_val
      end
      if env.lazy then
         struct[member.name] = reader
      else
         struct[member.name] = reader()
      end
   end

   return struct
end

function type_reader.union_type(env, type, addr)
   assert(type.tag == "union_type")
   assert(type.children)
   --log(#type.children)
   -- log_desc(type)
   local union = {}

   for i,member in ipairs(type.children) do
      assert(member)
      assert(member.tag == "member")
      assert(member.type)
      assert(member.name)
      local function reader()
         local element_addr = addr
         local element_val = read_type(env, member.type, element_addr)
         return element_val
      end
      if env.lazy then
         union[member.name] = reader
      else
         union[member.name] = reader()
      end
   end
   return union
end

function type_reader.array_type(env, type, addr)
   assert(env)
   assert(type.tag == "array_type")
   local upper_bound = elfutils.array_upper_bound(type)
   assert(upper_bound)
   local nb_el = upper_bound + 1
   assert(type.type)
   return elfutils.array_read_elements(env, addr, type.type, nb_el)
end




-- Structure found from log_desc() inspection, not manual.
function elfutils.array_upper_bound(type)
   assert(type.tag == "array_type")
   -- FIXME: This is likely not universal.  Works for now.
   -- FIXME: access this by type instead of index=1
   local t1 = type.children[1]
   if t1.tag == "subrange_type" then
      if t1.upper_bound then
         return t1.upper_bound
      else
         -- Array has no size.
         return 0
      end
   elseif t1.type and t1.type.name == "sizetype" then
      local upper_bound = t1.upper_bound
      assert(upper_bound)
      return upper_bound
   else
      log_desc(type)
      error("array_upper_bound_bad_type")
   end
end

-- The question here is do we return 1-base or 0-base array.  From
-- experience, working with 0-base arrays in lua is a royal pain, so
-- don't bother.

function elfutils.array_read_elements(env, array_addr, element_type, nb_el)
   assert(array_addr)
   element_type = flatten_type(element_type)
   local element_size = element_type.byte_size
   -- while nil == element_size do
   --    if element_type.tag == "typedef" then
   --       element_type = element_type.type
   --    else
   --       element_size = element_type.byte_size
   --       assert(element_size)
   --    end
   -- end
   assert(element_type.tag)
   local tag = element_type.tag -- FIXME: this should probably be collapsed
   local array = {}
   for i=0,nb_el-1 do
      function reader()
         local element_addr = array_addr + i * element_size
         -- log(string.format("array %d 0x%x\n", i, element_addr))
         local element_val = read_type(env, element_type, element_addr)
         assert(element_val)
         return element_val
      end
      if env.lazy then
         array[i+1] = reader
      else
         array[i+1] = reader()
      end
   end
   return array
end




-- This can read both actual array_type, but also pointer_type
-- interpreted as array, where user needs to provide nb_el.
function elfutils.read_array(env, name, nb_el)
   -- log_desc({read_array=name, nb_el=nb_el})
   assert(env)
   assert(env.elf)
   local die = C.die_find_variable(env.elf, name)
   local node = elfutils.die_unpack(die)
   local array_addr
   --log_desc(node.type)
   if node.type.tag == "array_type" then
      -- Plain array
      array_addr = node.location
      -- log_desc({array_node=node})
      local upper_bound = elfutils.array_upper_bound(node.type)
      assert(upper_bound)
      if not nb_el then
         nb_el = upper_bound + 1
      else
         assert(upper_bound + 1 >= nb_el)
      end
   elseif node.type.tag == "pointer_type" then
      -- Pointer to array.  We only know it is an array because caller
      -- tells us it is.  Caller needs to provide size
      assert(nb_el)
      array_addr = read_le_word(env, node.location, 4)
   else
      log_desc(node.type)
      error("elfutils.read_array bad type: " .. node.type.tag)
   end
   local element_type = node.type.type
   local function read_it()
      return elfutils.array_read_elements(env, array_addr, element_type, nb_el)
   end
   if env.lazy then
      return read_it
   else
      return read_it()
   end
end


local function find_variable_node(elf, name)
   assert(elf)
   assert(name)
   local dies = {C.die_find_variable(elf, name)}
   -- log_desc({find_variable_node_dies = dies})
   for i=1,#dies do
      local die = dies[i]
      local node = elfutils.die_unpack(die)
      if nil ~= node.location then
         return node
      else
         -- Probably node.external = 1
         -- It's in a different compilation unit.
      end
   end
   return nil
end

function elfutils.find_structure_type(env, name)
   assert(env)
   assert(env.elf)
   local dies = {C.die_find_structure_type(env.elf, name)}
   for i=1,#dies do
      local die = dies[i]
      local node = elfutils.die_unpack(die)
      -- log_desc({find_structure_type_node=node})
      return node
   end
end


function elfutils.read_variable(env, name)
   assert(env)
   assert(env.elf)
   local node = find_variable_node(env.elf, name)
   if not node then
      error('Node "' .. name .. '" not found')
   end

   assert(node.type)
   assert(node.location)

   local function read_it()
      local val = read_type(env, node.type, node.location)
      return val
   end
   if env.lazy then
      return read_it
   else
      return read_it()
   end
end




return elfutils



