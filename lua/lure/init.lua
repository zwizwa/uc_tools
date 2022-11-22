-- Write Lua modules in Scheme.  See lib_scm.lua for a usage example.
--
-- Example module body:
--
-- return require('lure').slc([[ ;; -*- scheme -*-
-- (define (f x) x)
-- (define (g x) (f (f x)))
-- ]])
--
-- Adding {verbose = true} config argument prints out IR of the compiler passes.
--
-- Use

local function trace(ir, pass, config)
   local s = io.stderr
   if config.verbose then
      local pretty = require('lure.scheme_pretty')
      local se     = require('lure.se')

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
   local se     = require('lure.se')
   local tab    = require('lure.tab')
   local slc2   = require('lure.slc2')
   local defm   = require('lure.scheme_defmacro')
   config = config or {}
   tab.copy1({trace = trace}, config)
   tab.copy1(slc2.default_config, config)
   local exprs = se.read_string_multi(str) ; assert(exprs)

   -- Extract macro definitions and compile them to lua functions.
   local macro_exprs, rest_exprs = defm.split(exprs)
   local mc = slc2.new(mconfig)
   local mlua = mc:compile({'module-begin',macro_exprs}) -- ; log_se(mlua)
   config.macros = slc2.eval(mlua)

   -- Compile the rest of the module
   local c = slc2.new(config)
   local lua = c:compile({'module-begin',rest_exprs})
   return slc2.eval(lua)
end

function compile_module_file_slc2(name, config)
   -- For luarocks bundling all SCM files are contained as strings
   -- inside the asset_scm module.
   local str = require('lure.asset_scm')[name]
   assert(str)
   return compile_module_slc2(str, config)
end

function compile_file_gdb(filename, stream)
   local se     = require('lure.se')
   local comp   = require('lure.comp')
   local exprs  = se.read_file_multi(filename)
   local expr   = {'begin',exprs}
   local c_new  = comp.make_multipass_new(
      {
         'lure.scheme_frontend',
         'lure.scheme_flatten',
         'lure.backend_gdb',
      })
   local c = c_new({primitive_letrec = 'primitive-letrec'})
   local out = c:compile(expr)
   se.write(out, stream)
end

return {
   slc = compile_module_slc2,
   slc_file = compile_module_file_slc2,  -- FIXME: rename to asset
   gdb = compile_file_gdb,
}
