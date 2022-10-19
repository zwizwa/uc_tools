local m = {}

-- Example test for the associativity of the plus operator.
m.plus_assoc = {
   -- Types are represented as functions.  These are evaluated with
   -- t=gen or t=shrink to produce a generator or a shrinker for this
   -- type.
   typ = function(t)
      return { a = t.nat, b = t.nat }
   end,
   -- Properties take an environment 'env' containing whatever we
   -- might want to inject into the tests instead of importing it from
   -- this module, and a table 'arg' instantiated from the generator
   -- derived from the type above.  Properties return true or false.
   -- Exceptions are mapped to false.
   run = function(env, arg)
      env:log_desc({arg=arg})
      return arg.a + arg.b == arg.b + arg.a
   end
}


-- Wrapper for a test written in C.  See linux/mod_test_lua51.c for
-- the Lua wrapper and linux/mod_test_heap.c for the C function.
m.heap_test1 = {
   typ = function(t)
      return {
         max_nb = t.nat,
         mul    = t.nat,
         mod    = t.nat1,
         log    = t.nat
      }
   end,
   run = function(e,a)
      return 0 == e.c.heap_test1(a.max_nb, a.mul, a.mod, a.log)
   end
}


return m
