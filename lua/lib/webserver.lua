-- Minimalistic webserver.
--
-- This is obviously not intended as a serious web server, but might
-- be useful as a plumbing tool in addition to being a test for the
-- actor and actor_uv libraries.

local actor_uv  = require('lib.actor_uv')
local lxml      = require('lib.lxml')
local mixin     = require('lib.mixin')
local webserver = {}

local function log(str)
   io.stderr:write(str)
end

function webserver:response_html(lxml_element)
   log("->200\n")
   self.socket:write("HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n")
   self.socket:write(lxml.elements_to_string({lxml_element}))
end

function webserver:response_svg(lxml_element)
   log("->200\n")
   self.socket:write("HTTP/1.1 200 OK\r\nContent-Type: image/svg+xml\r\n\r\n")
   self.socket:write(lxml.elements_to_string({lxml_element}))
end

function webserver:response_404()
   log("->404\n")
   self.socket:write("HTTP/1.1 404 Not Found\r\n\r\n404\r\n")
end

function webserver:handle()
   -- First line is request
   local req = self:recv()
   -- Rest is headers up to empty line.  Collect those in an array.
   local hdrs = {}
   while true do
      local line = self:recv()
      if line == '\r\n' then break end
      table.insert(hdrs, line)
   end
   -- Handle request
   log("req: " .. req)
   local uri = string.match(req, "GET (.*) HTTP/1.1\r\n")
   log("uri: " .. uri .. "\n")
   self:serve(uri, hdrs)
end

-- This adds webserver, actor_uv, actor.task behaviors to obj.
-- User needs to add method or mixin that implements :serve(uri,hdr)
function webserver.start(scheduler, obj)
   mixin.add(obj, webserver)
   obj.mode = 'line' -- needed by webserver:handle()
   actor_uv.spawn_tcp_server(scheduler, obj)
end

return webserver
