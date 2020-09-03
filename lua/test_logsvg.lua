#!/usr/bin/lua

-- Test for the logsvg module, which converts a list of time-stamped
-- logs to an SVG with proportional time display.  For now this is
-- exposed as a web service to allow simpler incremental development.


local uv        = require('lluv')
local webserver = require('lib.webserver')
local mixin     = require('lib.mixin')
local actor     = require('lib.actor')
local logsvg    = require('lib.logsvg')

local stamped_log1 = {
   {1, 'Time 1'},
   {2, 'Time 2'},
   {4, 'Time 4'},
   {10, 'Time 10'}
}

local stamped_log2 = {
   {5, 'Time 5'},
   {6, 'Time 6'},
   {7, 'Time 7'},
}

-- behaviors
local conn = {}
function conn:serve(uri, q)
   io.stderr:write("serve: " .. uri .. "\n")
   local e = {
      repel = 1, y_offset = 200, width = 500, height = 500,
      time_to_y   = function(time) return 10*time end,
      column_to_x = function(column_nb) return (column_nb-1) * 100 end
   }
   webserver.q_tonumber(q, {'width','height','repel'}, e)

   if uri == "/" then
      self:response_svg(logsvg.render(e, {stamped_log1, stamped_log2}))
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
