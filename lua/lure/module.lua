-- Write Lua modules in Scheme.  See lib_scm.lua for a usage example.
local se   = require('lure.se')
local slc2 = require('lure.slc2')

local pretty = require('lure.scheme_pretty')
local pprint = pretty.new()

local function trace(ir, pass, config)
   local s = io.stderr
   if config.verbose then
      s:write("\n" .. pass .. ":\n")
      assert(type(ir) == 'table')
      if ir.class then
         log_w(se.iolist(ir))
      else
         pprint:pprint_to_stream(s,ir)
      end
      -- log_se(ir)
   end
end

function compile_module(str, config)
   config = config or {}
   config.trace = trace
   local exprs = se.read_string_multi(str) ; assert(exprs)
   local c = slc2.new(config)
   local lua = c:compile({'module-begin',exprs})
   return slc2.eval(lua)
end

return compile_module
