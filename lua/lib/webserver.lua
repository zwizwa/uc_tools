-- Minimalistic webserver.
--
-- This is obviously not intended as a serious web server, but might
-- be useful as a plumbing tool in addition to being a test for the
-- actor and actor_uv libraries.

local actor_uv  = require('lib.actor_uv')
local lxml      = require('lib.lxml')
local mixin     = require('lib.mixin')

local function log(str)
   io.stderr:write(str)
end


-- Behavior associated to a single connection.
local webserver = {}

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

-- Called as the body of a new task actor_uv, to handle the tcp
-- connection.  recv() produces lines, as we are configured in line
-- mode.
function webserver:connect()
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
   -- Pass it to delegate
   self:serve(uri, hdrs)
end

function webserver.start(scheduler, serv_obj)
   serv_obj.mode = 'line' -- needed by webserver:handle()
   actor_uv.spawn_tcp_server(scheduler, serv_obj)
end

return webserver
