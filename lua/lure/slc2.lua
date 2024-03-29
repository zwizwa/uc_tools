#!/usr/bin/env lua
-- Scheme to Lua compiler, version 2.

-- This one is a multi-pass design built on the "block language" that
-- is also intended to be compiled to C, JavaScript, ...

-- All compiler passes including this composite pass are represented
-- as a module with a 'new' method, taking a config file, which then
-- produces an object with a 'compile' method that takes input ir to
-- output ir.  We take Scheme to lua code represented as iolist.
-- There is no intermediate representation of Lua code other than the
-- "block language".

local comp       = require('lure.comp')
local iolist     = require('lure.iolist')
local string_dsl = require('lure.string_dsl')

local mod = { }

mod.new =
   comp.make_multipass_new(
      {
         'lure.scheme_frontend',
         'lure.scheme_flatten',
         'lure.scheme_blockval',
         'lure.backend_lua',
      })

mod.default_config = {
   -- Until this as completely clean semantics, it is not ok to
   -- configure the frontend to always convert named let (i.e. "let
   -- loop") into a trampoline.
   -- letrec_loop = 'letrec-loop',
   debug_lua_output = '/tmp/test.lua',
}

-- The compiler produces a data structure representing lua code.  This
-- evaluates it to a lua module.
mod.eval = function(lua_iol)
   local lua_str = iolist.to_string(lua_iol.iolist)
   -- io.stderr:write(lua_str)
   local lua_mod = string_dsl.lua_eval(lua_str)
   assert(lua_mod)
   return lua_mod
end

return mod
