#!/usr/bin/lua

-- Test for the logsvg module, which converts a list of time-stamped
-- logs to an SVG with proportional time display.  For now this is
-- exposed as a web service to allow simpler incremental development.


local uv        = require('lluv')
local webserver = require('lib.webserver')
local mixin     = require('lib.mixin')
local actor     = require('lib.actor')

local function svg()
   return
      {'svg',
       {xmlns='http://www.w3.org/2000/svg',
        width=200,
        height=2000},{
          {'style',{},
           {".small { font-family: monospace; font-size: 10px }\n"}},
          {'rect',
           {width=100,
            height=100,
            stroke='white',
            fill='grey'}},
          {'rect',
           {transform='translate(10,10)',
            width=50,
            height=50,
            stroke='white',
            fill='blue'}},
          {'text',
           {width=50,
            height=50,
            transform='translate(0,20)',
            class='small',
            stroke='black'},
           {'Test 123'}}
       }
      }
end


-- behaviors
local conn = {}
function conn:serve(uri)
   io.stderr:write("serve: " .. uri .. "\n")
   if uri == "/" then
      self:response_svg(svg())
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
