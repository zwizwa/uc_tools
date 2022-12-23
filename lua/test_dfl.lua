#!./lua.sh

local prompt = require('prompt')
local function log(str) io.stderr:write(str) end
local function log_desc(thing) log(prompt.describe(thing)) end

log("test_dfl.lua begin\n")

local dfl = require('lib.dfl')

function test()

   -- This program syntax, represented abstractly as a function.  Note
   -- that the function has no intrinsic meaning.  All the meaning
   -- comes from the 's' objects which implements add and mul methods.
   local function f(s, a, b)
      local c = s:mul(a, a)
      local d = s:mul(b, b)
      return s:add(c, d)
   end

   -- Instantiate eval semantics.  In this case we can just pass the
   -- meaning directly to the function.
   log("- eval:\n" .. f(dfl.eval, 1, 2) .. "\n")

   -- Compilation semantics requires some more setup, which is done
   -- inside the dfl.compile function.  Lua is variadic so arity needs
   -- to be passed in.
   local dag = dfl.compile(f, 2)
   log("- dag:\n")
   log_desc(dag)
   log("\n")

   -- The concrete syntax (a representation of a directed a-cyclic
   -- graph), can be printed as C code.
   local code = dfl.print(dag, "testfun")
   log("- code:\n")
   io.stdout:write(code)

end

test()
log("test_dfl.lua end\n")

