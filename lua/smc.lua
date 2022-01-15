#!/usr/bin/env lua
package.path = package.path .. ";./?.lua"
local smc = require('lib.smc')
local function compile(file)
   assert(file)
   local forms = {
      -- extensions
      require('lib.smc_cspc'),
      require('lib.smc_co'),
   }
   local comp = smc.new({forms = forms})
   comp.write = function(self, str) io.stdout:write(str) end
   comp:compile_module_file(file)
end

-- local file = "test.sm"
compile(arg[1])

