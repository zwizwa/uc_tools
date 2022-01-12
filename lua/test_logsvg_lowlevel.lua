#!/usr/bin/lua

-- Low level test for for the logsvg module.  Mostly intended to debug
-- robustness against bad log formatting.

local uv        = require('lluv')
local webserver = require('lib.webserver')
local mixin     = require('lib.mixin')
local actor     = require('lib.actor')
local logsvg    = require('lib.logsvg')

local logfile = arg[1]
local max_lines = tonumber(arg[2])
assert(logfile)
logsvg.read_log(logfile, { max_lines = max_lines })
