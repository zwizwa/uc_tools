-- Dumping ground for an idea to add an imperative interface.

local qctools = require('lib.qctools')

local m = {}

-- I miss Haskell for the safety net that types provide when stringing
-- together higher order functions.  I miss the Monad interface.
--
-- I wonder if part the difficulty of doing this in Lua can be
-- mitigated by using an imperative interface, to somewhat emulate the
-- use of Monads in Haskell QC.
--
-- Just as an interface to perform the state threading locally while
-- still creating genrators as pure functions -- in Lua the semantics
-- is actual mutability, so some care is needed that it does not leak.

-- Let's try this: create a new generator that can refer to (pure)
-- generators defined in a table t.  Then run a function body
-- imp_gen() in a context where these functional generators are
-- translated to imperative generators (as a user interface) provided
-- as a table it.  The user is then expected to not leak the it table
-- outside of the function.

-- TODO: Create some wrapper to make it possible to combine a couple
-- of dictionaries into a single t = environment of primitive and
-- composite generators.

-- EDIT: The basic idea works works, but it is a bit as I expected: it
-- is too confusing to mix worlds without type system support.  There
-- are many ways to comine and having both functional and imperative
-- constructs just adds more complexity than it removes.  But it is
-- good to know that this approach works in general.  If all
-- generators are written in this style, it might make sense.

function m.imp(imp_gen, t)
   local function index(it, key)
      -- Actual members e.g. seed, size
      local val = rawget(it, key)
      if val ~= nil then return val end

      -- All the other keys refer to lifted generator functions.
      local gen = t[key]
      assert(gen)

      -- Note that this only works for primitive (non-parameterized)
      -- generators.  We do not have a way to distinguish those to do
      -- 2 separate lifting cases.  E.g. gen = qctools.nat works, but
      -- qctools.list does not.

      return function(...)
         local val, next_seed = gen(it.seed, it.size)
         it.seed = next_seed
         return val
      end
   end

   -- The result is a fuctional generator.
   return function(seed, size)
      -- The it is just used to thread the seed, size throughout the
      -- evaluation of imp_gen() body.
      local it = { seed = seed, size = size }
      setmetatable(it, { __index = index })
      local val = imp_gen(it)
      return val, it.seed
   end
end

-- EXAMPLE:
-- function typs.imp1(t)
--    return qctools.imp(
--       function(it)
--          local lst = {}
--          table.insert(lst, {'head', it.nat()})
--          for i=1,it.nat() do
--             table.insert(lst, {'tail', it.nat()})
--          end
--       end, t)
-- end


local typs = {}

function typs.imp1(t)
   return qctools.imp(
      function(it)
         local lst = {}
         table.insert(lst, {'head', it.nat()})
         for i=1,it.nat() do
            table.insert(lst, {'tail', it.nat()})
         end
      end, t)
end

typs.imp2 = qctools.impf(
   function(it)
      local lst = {}
      table.insert(lst, {'head', it.nat()})
      for i=1,it.nat() do
         table.insert(lst, {'tail', it.nat()})
      end
   end)

m.typs = typs
return m
