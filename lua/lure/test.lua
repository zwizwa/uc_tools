local test = {}

-- First make sure all the modules load properly.
-- lure.meta is generated from files in uc_tools/lua/lure/*.lua
local meta = require('lure.meta')
local mod = {}
local test = {}
for name in pairs(meta.modules) do
   if name ~= 'test' then
      mod[name] = require('lure.' .. name)
   else
      -- We are in test, avoid loop.
   end
end

-- Print out the modules list for the specs file.
local w = mod.iolist.io_writer(io.stdout)
local function print_modules()
   for k in pairs(mod) do
      w(k,"\n")
   end
end

-- This is the one advertised on the luarocks page.
function test.run()
   w("Running Lure Tests\n")
   for k in pairs(mod) do
      if k:sub(1,5) == 'test_' then
         w("*** ",k,"\n")
         mod[k].run(w)
      end
   end
   -- For loarocks file
   w("  modules = {\n")
   for k in pairs(meta.modules) do
      w("    ['lure.",k,"'] = '",k,".lua',","\n")
   end
   w("  }\n")

end
return test
