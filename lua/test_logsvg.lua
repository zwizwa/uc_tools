#!/usr/bin/lua

-- Test for the logsvg module, which converts a list of time-stamped
-- logs to an SVG with proportional time display.  For now this is
-- exposed as a web service to allow simpler incremental development.


local uv        = require('lluv')
local webserver = require('lib.webserver')
local mixin     = require('lib.mixin')
local actor     = require('lib.actor')
local logsvg    = require('lib.logsvg')

local stamped_log = {
   {1, 'Time 1'},
   {2, 'Time 2'},
   {4, 'Time 4'},
}

-- behaviors
local conn = {}
function conn:serve(uri)
   io.stderr:write("serve: " .. uri .. "\n")
   if uri == "/" then
      self:response_svg(
         logsvg.render(stamped_log))
   else
      self:response_404()
   end
end

local testserv = {}
function testserv:connection()
   local c = {}
   mixin.add(c, conn)
   mixin.add(c, webserver)
   return c
end

-- server instance
local s = {ip = '0.0.0.0', port = 8000 }
mixin.add(s, testserv)


local scheduler = actor.scheduler.new()
webserver.start(scheduler, s)
uv.signal():start(uv.SIGINT, function() uv.stop() end)
uv.run()
