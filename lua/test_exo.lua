#!/usr/bin/lua
local uv        = require('lluv')
local packet4   = require('lib.packet4')
local mixin     = require('lib.mixin')
local actor     = require('lib.actor')

-- behaviors
local testconn = {}
function testconn:serve(uri)
end

local testserv = {}
function testserv:connection()
   local c = {}
   mixin.add(c, testconn)
   mixin.add(c, packet4)
   return c
end

-- instance
local s = {ip = '0.0.0.0', port = 1234 }
mixin.add(s, testserv)


local scheduler = actor.scheduler.new()
packet4.start(scheduler, s)
uv.signal():start(uv.SIGINT, function() uv.stop() end)
uv.run()
