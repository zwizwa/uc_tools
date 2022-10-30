#!./lua.sh

-- Figure out a way to represent a nested C data type in Lua in the
-- same way that it would be done in C, which means to work around the
-- idea that the primitive array in C is not sized, but ARRAY_SIZE is
-- available as long as things are referenced by name.
--
-- The main issue is that an array cannot be represented by a pointer
-- alone, so it either needs to tag the size either at the pointer
-- end, or using a sized wrapper.
--
-- To keep it simple, it might be best to always use a sized wrapper.


-- So what is a C datatype?
--
-- Base types are integers
-- Composite types are structs
-- These can go into arrays
-- Arrays can be null-terminated
-- Arrays are sized when referenced by name
-- Arrays can be wrapped with a size struct
-- Composition is either by inlining or pointing
--
-- How to simplify this mess?
--
-- Focus on modeling C exactly, then create an abstraction on top of it.

-- Where do I want to go with this?  Generate struct definitions and
-- instantiations, e.g. to use with quickcheck, or generating tables
-- for interpreters.

require 'lure.log'


local ex1 = function(t)
   return t.struct(
      {'a', t.pointer(t.array(t.int,6))},
      {'b', t.int}
   )
end

local ex2 = function(t)
   local tx = t.array(t.int)
   return t.struct(
      {'a', t.int},
      {'b', t.array(t.int)},
      {'c', t.struct(
          {'left',  t.int},
          {'right', t.int}
      )},
      {'d', t.pointer(tx)}
   )
end


-- Erlang's iolist
local function w_thing(thing)
   if type(thing) == 'table' then
      for _, subthing in ipairs(thing) do
         w_thing(subthing)
      end
   else
      io.stdout:write(thing .. "")
   end
end
local function w(...) w_thing({...}) end


local tab = "    "

-- Type declaration printer.
-- Generates a typedef for each node in the tree.

local function counter(init)
   local nb = 0
   return function()
      local rv = nb
      nb = nb + 1
      return rv
   end
end

function gen_decl(def, w)

   -- Anonymous types
   local next_nb = counter(0)
   -- This map keeps track of all primitive and generated types.
   -- Initialized with primitives
   local types = {"int"}

   -- Create typedef wrapper.
   local function typedef(suffix, gen)
      return function(base, size)
         -- Precondition: base is a type name that is either
         -- primitive, or one we've generated.
         local new = base .. suffix
         if types[new] ~= nil then return new end
         types[new] = true
         w({"typedef ", gen(new, base, size),";\n"})
         return new
      end
   end

   -- Semantics passed to the definition.
   local sem = {}

   -- Each primitive is represented in the semantics.
   for _,typ in ipairs(types) do sem[typ] = typ end


   -- Type composition functions

   -- Arrays and pointers are treated as isomorphic, because C does
   -- as well.
   sem.array =
      typedef("_arr",
              function(new, base, size)
                 return {base, " ", new, "[", size or "", "]"}
              end)
   sem.pointer =
      typedef("_ptr",
              function(new, base)
                 return {base, " *", new}
              end)
   -- Structs are always unique, i.e. two isomorphic definitions are
   -- different types.
   function sem.struct(...)
      local new = 'struct_' .. next_nb()
      types[new] = true;
      w("typedef struct {\n")
      for _, field in ipairs({...}) do
         local name, typ = unpack(field)
         assert(name)
         assert(typ)
         w(tab, typ, " ", name, ";\n")
      end
      w("} ", new, ";\n")
      return new
   end

   local top = def(sem)
   w("// top:", top, "\n")
end

-- Test
gen_decl(ex1, w)
gen_decl(ex2, w, s)


-- Now what I really want to do is instantiation.  I can tink of a
-- couple of ways that would be useful
-- . C const initializers for inclusion in Firmware flash
-- . binary image generation with properly linked pointers
-- . ser/deser generation
--
-- In any case, some lua representation is necesary.  That
-- representation could just be the one used here.  Let's start with a
-- type checker.

function type_checker(def, c)
   local sem = {}
   function sem.int(data)
      c.assert(type(data) == 'number')
   end
   function sem.array(check_el, nb_el)
      return function(data)
         c.assert(type(data) == 'table')
         if nil ~= nb_el then
            c.assert(nb_el == #data)
         end
         for _, el in ipairs(data) do
            -- log_desc({el=el,check_el=check_el})
            check_el(el)
         end
      end
   end
   -- In Lua, pointers are transparent.
   function sem.pointer(check_el)
      return check_el
   end
   function sem.struct(...)
      local fields = {...}
      return function(data)
         c.assert(type(data) == 'table')
         for _, field in ipairs(fields) do
            local name, check_el = unpack(field)
            local val = data[name]
            c.assert(val)
            check_el(val)
         end
      end
   end
   return def(sem)
end
function type_check(def, data)
   local c = { assert = function(bool) assert(bool) end }
   local check = type_checker(def, c)
   -- check(data)
   local ok = pcall(check, data)
   return ok
end

local function ex3(t) return t.int end
local function ex4(t) return t.array(t.int) end
local function ex5(t) return t.pointer(ex4(t)) end
local function ex6(t) return t.struct({'a',t.int},{'b',t.int}) end
local function ex7(t) return t.array(t.array(t.int,2),3) end

log_desc({
      a = type_check(ex3, {}),
      b = type_check(ex3, 123),
      c = type_check(ex4, {1,2,3}),
      d = type_check(ex4, {1,2,'abc'}),
      e = type_check(ex5, {1,2,3}),
      f = type_check(ex6, {a=1,b=2}),
      g = type_check(ex6, {a=1,c=2}),
      h = type_check(ex7, {{1,2},{3,4},{5,6}}),
      i = type_check(ex7, {{1,2},{3,4}}),
})

-- Alright so next is a serializer for C initializers.  It might be
-- enough to do just this, then let the C compiler + linker generate
-- linked binaries for upload to RAM.  Communication overhead is
-- always going to be the bottleneck so running the compiler is
-- probably ok.

-- This seems to be a merge of the type generator and the type
-- checker.  I don't really see a way to do that automatically.

-- Precondition: data typechecks.
function serializer(def, c)
   local next_nb = counter(0)

   local sem = {}
   -- int, array and struct are fairly straightforward: C initializer
   -- can use nested syntax, so this just recurses.
   function sem.int(data, w)
      w(data)
   end
   function sem.array(w_el)
      return function(data, w)
         w("{ ")
         for _, el in ipairs(data) do
            w_el(el,w)
            w(", ")
         end
         w("}")
      end
   end
   function sem.struct(...)
      local fields = {...}
      return function(data, w)
         w("{ ")
         for _, field in ipairs(fields) do
            local name, w_el = unpack(field)
            local val = data[name] or error('nil')
            w(".", name, " = ")
            w_el(val,w)
            w(", ")
         end
         w("}")
      end
   end
   -- pointers are special. a node needs to be serialized then
   -- referenced by name
   function sem.pointer(w_el)
      -- 

      -- FIXME
      return w_el
   end
   return def(sem)
end

function serialize(def, data)
   local c = { assert = function(bool) assert(bool) end }
   local ser = serializer(def, c)
   ser(data, w)
   w("\n")
end

local function ex8(t)
   local arr = t.array(t.int, 2)
   return t.struct({'a',arr},{'b',arr})
end


serialize(ex3, 123)
serialize(ex4, {1,2,3})
serialize(ex6, {a=1,b=2})
serialize(ex7, {{1,2},{3,4},{5,6}})
serialize(ex8, {a = {1,2}, b = {3,4}})
