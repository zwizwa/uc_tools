
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
    ['lure.test_smc'] = 'test_smc.lua',
    ['lure.iolist'] = 'iolist.lua',
    ['lure.test_slc'] = 'test_slc.lua',
    ['lure.slc'] = 'slc.lua',
    ['lure.log'] = 'log.lua',
    ['lure.test_scheme_pass'] = 'test_scheme_pass.lua',
    ['lure.test'] = 'test.lua',
    ['lure.test_se_match'] = 'test_se_match.lua',
    ['lure.test_scheme_macros'] = 'test_scheme_macros.lua',
    ['lure.match'] = 'match.lua',
    ['lure.scheme'] = 'scheme.lua',
    ['lure.asset_scm'] = 'asset_scm.lua',
    ['lure.scheme_luapp'] = 'scheme_luapp.lua',
    ['lure.scheme_flatten_blocks'] = 'scheme_flatten_blocks.lua',
    ['lure.smc_cspc'] = 'smc_cspc.lua',
    ['lure.smc'] = 'smc.lua',
    ['lure.smc_co'] = 'smc_co.lua',
    ['lure.smatch'] = 'smatch.lua',
    ['lure.test_scheme_luapp'] = 'test_scheme_luapp.lua',
    ['lure.se'] = 'se.lua',
    ['lure.slc_runtime'] = 'slc_runtime.lua',
    ['lure.test_hoas_match'] = 'test_hoas_match.lua',
    ['lure.se_match'] = 'se_match.lua',
    ['lure.string_dsl'] = 'string_dsl.lua',
    ['lure.scheme_match'] = 'scheme_match.lua',
    ['lure.meta'] = 'meta.lua',
    ['lure.set'] = 'set.lua',
    ['lure.log_se'] = 'log_se.lua',
    ['lure.scheme_macro_anf'] = 'scheme_macro_anf.lua',
    ['lure.scheme_macros'] = 'scheme_macros.lua',
    ['lure.scheme_pretty'] = 'scheme_pretty.lua',
    ['lure.comp'] = 'comp.lua',
  }
}
