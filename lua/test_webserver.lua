#!/usr/bin/lua
local uv        = require('lluv')
local webserver = require('lib.webserver')
local mixins    = require('lib.mixins')

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


-- behavior
local testserv = {}
function testserv:serve(uri, hdr)
   io.stderr:write("serve: " .. uri .. "\n")
   if uri == "/" then
      self:response_html({'h1',{},{'Hello1'}})
   elseif uri == "/img" then
      self:response_svg(svg())
   else
      self:response_404()
   end
end

-- instance
local s = {ip = '0.0.0.0', port = 8000, mode = 'line'}
mixins.add(s, testserv)

webserver.start(s)
uv.signal():start(uv.SIGINT, function() uv.stop() end)
uv.run()
