
package = "lure"
version = "0.1-1"

source = {
  url = "https://github.com/zwizwa/lure-lua/archive/v0.1.zip",
  dir = "lure-lua-0.1",
}

description = {
  summary    = "Lua library for writing Scheme interpreters/compilers",
  homepage   = "https://github.com/zwizwa/lure-lua",
  license    = "MIT/X11",
  maintainer = "Tom Schouten",
  detailed   = "Lua wrappers for writing Scheme interpreters and compilers.\n",
}

dependencies = {
  "lua >= 5.1"
}

build = {
  type = "builtin",
  modules = {
    ['lure.asset_scm'] = 'asset_scm.lua',
    ['lure.backend_c'] = 'backend_c.lua',
    ['lure.backend_erlang'] = 'backend_erlang.lua',
    ['lure.backend_gdb'] = 'backend_gdb.lua',
    ['lure.backend_js'] = 'backend_js.lua',
    ['lure.backend_lua'] = 'backend_lua.lua',
    ['lure.comp'] = 'comp.lua',
    ['lure.index'] = 'index.lua',
    ['lure.init'] = 'init.lua',
    ['lure.iolist'] = 'iolist.lua',
    ['lure.iter'] = 'iter.lua',
    ['lure.lib_scm'] = 'lib_scm.lua',
    ['lure.log'] = 'log.lua',
    ['lure.log_se'] = 'log_se.lua',
    ['lure.match'] = 'match.lua',
    ['lure.meta'] = 'meta.lua',
    ['lure.mon'] = 'mon.lua',
    ['lure.rvm_debug'] = 'rvm_debug.lua',
    ['lure.rvm'] = 'rvm.lua',
    ['lure.scheme_blockval'] = 'scheme_blockval.lua',
    ['lure.scheme_escape'] = 'scheme_escape.lua',
    ['lure.scheme_eval'] = 'scheme_eval.lua',
    ['lure.scheme_flatten'] = 'scheme_flatten.lua',
    ['lure.scheme_frontend'] = 'scheme_frontend.lua',
    ['lure.scheme_liveness'] = 'scheme_liveness.lua',
    ['lure.scheme'] = 'scheme.lua',
    ['lure.scheme_macros'] = 'scheme_macros.lua',
    ['lure.scheme_match'] = 'scheme_match.lua',
    ['lure.scheme_pretty'] = 'scheme_pretty.lua',
    ['lure.scheme_sm'] = 'scheme_sm.lua',
    ['lure.se'] = 'se.lua',
    ['lure.se_match'] = 'se_match.lua',
    ['lure.set'] = 'set.lua',
    ['lure.slc2'] = 'slc2.lua',
    ['lure.slc'] = 'slc.lua',
    ['lure.slc_runtime'] = 'slc_runtime.lua',
    ['lure.smatch'] = 'smatch.lua',
    ['lure.smc_co'] = 'smc_co.lua',
    ['lure.smc_cspc'] = 'smc_cspc.lua',
    ['lure.smc'] = 'smc.lua',
    ['lure.string_dsl'] = 'string_dsl.lua',
    ['lure.tab'] = 'tab.lua',
    ['lure.test_backend_c'] = 'test_backend_c.lua',
    ['lure.test_backend_erlang'] = 'test_backend_erlang.lua',
    ['lure.test_backend_gdb'] = 'test_backend_gdb.lua',
    ['lure.test_backend_js'] = 'test_backend_js.lua',
    ['lure.test_backend_lua'] = 'test_backend_lua.lua',
    ['lure.test_hoas_match'] = 'test_hoas_match.lua',
    ['lure.test'] = 'test.lua',
    ['lure.test_mon'] = 'test_mon.lua',
    ['lure.test_scheme_eval'] = 'test_scheme_eval.lua',
    ['lure.test_scheme_liveness'] = 'test_scheme_liveness.lua',
    ['lure.test_scheme_macros'] = 'test_scheme_macros.lua',
    ['lure.test_scheme_pass'] = 'test_scheme_pass.lua',
    ['lure.test_scheme_rvm'] = 'test_scheme_rvm.lua',
    ['lure.test_scheme_sm'] = 'test_scheme_sm.lua',
    ['lure.test_scm'] = 'test_scm.lua',
    ['lure.test_se_match'] = 'test_se_match.lua',
    ['lure.test_slc'] = 'test_slc.lua',
    ['lure.test_smc'] = 'test_smc.lua',
  }
}
