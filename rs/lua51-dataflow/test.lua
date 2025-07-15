local prompt = require('prompt')
local function log(str)
   io.stderr:write(str)
end
local function log_desc(thing)
   log(prompt.describe(thing))
   log("\n")
end
local m = require('dataflow_rs')
log_desc(m)

-- Run the rust-only test function.
m.test_internal()


-- FIXME: implement
function test_compiler()
   -- Compiler test.
   -- The central object is the compiler, so make an explicit constructor.
   local c = m.new_compiler()
   -- It will need to present node objects.
   local i1 = c:input()
   local i2 = c:input()
   -- And operators
   local o = c:add(i1, i2)

   -- Then dump the program as C code.
   -- local code = c:compile()
   -- log(code)
   log_desc(c.code)
   log_desc(c.inputs)

end

test_compiler()




