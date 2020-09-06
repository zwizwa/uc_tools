#!/usr/bin/lua
local uv        = require('lluv')
local mixin     = require('lib.mixin')
local actor     = require('lib.actor')
local actor_uv  = require('lib.actor_uv')

-- behaviors
local testconn = {}
function testconn:connect()
   while true do
      local msg = self:recv()
      io.stderr:write(#msg[2])
   end
end

local testserv = {}
function testserv:connection()
   local c = {}
   mixin.add(c, testconn)
   return c
end

-- instance
local s = {ip = '0.0.0.0', port = 1234 }
mixin.add(s, testserv)

function start(scheduler, serv_obj)
   serv_obj.mode = {'packet',4}
   actor_uv.spawn_tcp_server(scheduler, serv_obj)
end

local scheduler = actor.scheduler.new()
start(scheduler, s)
uv.signal():start(uv.SIGINT, function() uv.stop() end)
uv.run()
