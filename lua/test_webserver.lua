#!/usr/bin/lua
local uv = require('lluv')
local webserver = require('lib.webserver')

local testserv = {}

function testserve:serve(uri, hdr)
   if uri == "/" then
      self:response_html({'h1',{},{'Hello1'}})
   elseif uri == "/img" then
      self:response_svg(svg())
   else
      self:response_404()
   end
end

webserver.start(8000, testserve)
uv.signal():start(uv.SIGINT, function() uv.stop() end)
uv.run()
