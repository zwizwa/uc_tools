-- Write Lua modules in Scheme.  See lib_scm.lua for a usage example.
--
-- Example module body:
--
-- return require('lure').slc2([[
-- (define (f x) x)
-- (define (g x) (f (f x)))
-- ]])
--
-- Adding {verbose = true} config argument prints out IR of the compiler passes.

local se     = require('lure.se')
local tab    = require('lure.tab')
local slc2   = require('lure.slc2')
local l = se.list

local function trace(ir, pass, config)
   local s = io.stderr
   if config.verbose then
      local pretty = require('lure.scheme_pretty')

      s:write("\n" .. pass .. ":\n")
      assert(type(ir) == 'table')
      if ir.class then
         log_w(se.iolist(ir))
      else
         pretty.log_pp(ir)
      end
   end
end

function compile_module_slc2(str, config)
   config = config or {}
   tab.copy({trace = trace}, config)
   tab.copy(slc2.default_config, config)
   local exprs = se.read_string_multi(str) ; assert(exprs)
   local c = slc2.new(config)
   local lua = c:compile({'module-begin',exprs})
   return slc2.eval(lua)
end

return {
   slc2 = compile_module_slc2
}
