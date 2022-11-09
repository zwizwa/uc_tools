#!./lua.sh

-- Quick & Dirty Interpreter for Literal Lua.

require('lure.log')

local m = {}

-- Input is in Lua syntax
function m.run_lua(file)
   local f = io.open(file, "rb")
   assert(f)
   local next_line = io.lines(file)
   next_line() -- skip shebang

   local code = {}
   for line in next_line do
      -- Run code before end of code tag.
      if line == '-- ```' then  -- FIXME: dont' assume exact formatting
         local c = table.concat(code) ; code = {}
         local f = loadstring(c)
         f()
      end
      -- Collect anything that's not commented as code.
      if line:byte(1) ~= 45 then -- FIXME: more liberal match
         table.insert(code, line)
         table.insert(code, '\n')
         io.write(line)
      else
         io.write(line:sub(4)) -- FIXME: don't assume exact formatting
      end
      io.write('\n')
   end
end


-- Input is in doc syntax
function m.run_doc(file)

   -- Read the entire file so we can overwrite it.
   local f = io.open(file, "rb")
   assert(f)
   local next_line = io.lines(file)

   local code = nil
   function eval()
      local c = table.concat(code) ; code = {}
      local f = loadstring(c)
      f()
      code = {}
   end

   -- FIXME: This doesn't need "modes".  It's probably simpler to
   -- factor it out in functions.
   for line in next_line do
      if line == '```c' then
         io.write(line)
         io.write('\n')
         local f = loadstring("print_c()")
         f()
         -- Ignore the existing block
         while '```' ~= next_line() do end
      elseif line == '```lua' then
         code = {} -- Enter code mode
      elseif line == '```' then
         eval()
         code = nil -- Leave code mode
      elseif line:byte(1) == 61 then
         -- Dont' print lines containing dash, but trigger evaluation
         -- instead.
         line = nil
         eval()
      else
         -- Recorde code when in code mode
         if code ~= nil then
            table.insert(code, line)
            table.insert(code, '\n')
         end
      end
      -- Write it out if it wasn't cancelled.
      if line ~= nil then
         io.write(line)
         io.write('\n')
      end
   end
end

-- function test()
--    -- m.run_lua('doc_seq.lua')
--    m.run_doc('../doc/seq_lua.md')
-- end
-- test()

return m

