-- SLC2: Scheme to Lua compiler, version 2.
-- See also slc.lua for version 1, currently still present for archiving purposes.
-- This version is a multipass compiler, with passes based on match.lua infrastructure.

local comp = require('lure.comp')
local mod = comp.make_multipass({
      -- Convert block form to Lua
      'lure.scheme_luapp',
})
return mod
