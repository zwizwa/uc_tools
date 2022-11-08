-- Quick & Dirty Interpreter for Literal Lua.

require('lure.log')

local m = {}

function m.run(file)
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

function test()
   m.run('doc_seq.lua')
end

test()

return m

