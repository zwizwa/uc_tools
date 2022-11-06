#!./lua.sh


-- Rework: this is a proof of concept interpretation model + compiler.
-- It is best to switch from this approach to Lure intermediate
-- language.

-- Split into:
-- . lazy list lib (done)
-- . compile 'unfold into array' (done)
-- . compile from hoas to Scheme syntax (+- done)
-- . compile Lure IR extended with vector loops to CPS (return slot pointer only)


-- SYNTAX

-- The first argument contains the library of signal operations.
-- The signals support overloading for arithmetic operations.
-- Library functions will auto-lift numbers to signals.

require 'lure.log'

-- FIXME: curry the context argument.
-- FIXME: in should be a single argument

-- It's more convenient to wrap the semantics 'c' around a collection
-- of functions, thant to have a collection of functions with a 'c'
-- parameter, curried or not.
local function progs(c)

   local m = {}
   function m.prog1(i)
      local function counter_sm(s) return s+1, s end
      local counter = c.close(0, counter_sm)
      return counter + i
   end
   function m.prog2()
      function update(s) return c.add1(s), s end
      return c.close(0, update)
   end
   function m.prog3(a)
      return c.add1(a)
   end
   function m.prog4(a, b)
      return c.add(a, b)
   end
   function m.prog5(a)
      return a + 1
   end
   function m.prog6()
      return c.vec(3, function(i) return i + 1 end)
   end
   function m.prog7(a)
      local a = c.vec(
         -- Vector size needs to be known at compile time (for now).
         13,
         function(
               -- Index into array
               i,
               -- Dereference for elements already computed.  ref(j)
               -- is valid for j<i.  If this is not used, the
               -- computation is not causally linked so it can be
               -- performed in parallel.
               ref)
            -- The return value is what goes into slot i
            return i + i + 1
      end)
      -- Later, implement auto-lifting of simple constructs.
      -- FIXME: This might be a bad idea...
      return a + 1
   end
   function m.prog8(a)
      return c.vec(13, function(i)
      return c.vec(17, function(j)
      return i + j + 1
      end)
      end)
   end
   function m.prog9(a)
      return c.vec(13, function(i)
      return c.vec(17, function(j)
      local counter = c.close(0, function(s) return s+1, s end)
      return i + j + counter
      end)
      end)
   end
   function m.prog10(a)
      return c.vec(13, function(i)
      return c.vec(17, function(j)
      return c.vec(29, function(k)
      local counter = c.close(0, function(s) return s+1, s end)
      return i + j + k + counter
      end)
      end)
      end)
   end
   return m
end



-- SEMANTICS: SIGNALS AS LAZY LISTS

-- FIXME: c.vec is not supported for these.  Probably not necessary as
-- a Lua implementation can be constructed via Lure once the compiler
-- is done.

-- Use lazy lists to represent signals.
local ll_metatable = {}
local ll = require('lure.lazylist')(ll_metatable)

local signal_c = {
   add1  = ll.lift(1, function(a)   return a+1 end),
   add   = ll.lift(2, function(a,b) return a+b end),
   close = ll.close
}

-- Patch the metatable for operator support.
ll_metatable.__add = signal_c.add

llp = progs(signal_c)

local f1 = llp.prog1(ll.pure(100))
local f2 = llp.prog2()

log_desc({
      f1 = ll.take(3,f1),
      f2 = ll.take(3,f2)
})

-- FIXME: old api
-- log_desc({
--       one = c.one,
--       add1_one = c.add1(c.one),
--       one_tail = c.one.tail(),
--       add1_one_tail = c.add1(c.one).tail(),
--       f2 = f2,
--       f2_tail = f2.tail(),
--       f2_tail_tail = f2.tail().tail(),
--       f1 = f1,
--       f1_tail = f1.tail(),
-- })



-- SEMANTICS: COMPILER

local seq = require('lure.seq')
local function compile(name, nb_args)
   seq.compile(
      function (c)
         return progs(c)[name]
      end,
      nb_args)
end

compile('prog3', 1)
compile('prog4', 2)
compile('prog5', 1)
compile('prog2', 1)
compile('prog1', 1)
compile('prog6', 0)
compile('prog7', 1)
compile('prog8', 0)
compile('prog9', 0)
compile('prog10', 0)




-- TODO:
-- . C code generator
-- . Lua (Lure) code generator for LuaJIT
-- . Combinators

