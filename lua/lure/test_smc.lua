-- Scheme files are wrapped as strings inside this module.
local asset = require('lure.asset_scm')
local smc   = require('lure.smc')
local se    = require('lure.se')
local l = se.list

require('lure.log')

local function compile(filename)
   local forms = {
      -- extensions
      require('lure.smc_cspc'),
      require('lure.smc_co'),
   }
   local comp = smc.new({forms = forms})
   -- This is for 'import'.  Implemented in scheme-macros.lua and
   -- passed up through scheme.lua and smc.lua to here: we need to
   -- configure this or set it to se.read_file_multi if it's ok to
   -- just read a file.  Here we get it from the asset file.
   comp.import_read_multi = function(filename)
      local str = asset[filename]
      return se.read_string_multi(str)
   end
   comp.write = function(self, str) io.stdout:write(str) end
   comp:compile_module_file(filename, asset)
end

local function run(w)
   compile('test1.sm')
   compile('test2.sm')
   compile('test_co.sm')
end

return { run = run }
