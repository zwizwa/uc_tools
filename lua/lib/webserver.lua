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

function webserver:write(str)
   return self:write_socket(self.socket, str)
end

function webserver:response_html(lxml_element)
   -- local function write_cb() log("write_cb\n") end
   log("->200\n")
   self:write("HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n")
   self:write(lxml.elements_to_string({lxml_element}))
end

function webserver:response_svg(lxml_element)
   log("->200\n")
   self:write("HTTP/1.1 200 OK\r\nContent-Type: image/svg+xml\r\n\r\n")
   self:write(lxml.elements_to_string({lxml_element}))
end

function webserver:response_404()
   log("->404\n")
   self.socket:write("HTTP/1.1 404 Not Found\r\n\r\n404\r\n")
end


-- Small pure tools.
function webserver.q_tonumber(q, keys, nums)
   for i,key in ipairs(keys) do
      if q[key] then
         nums[key] = tonumber(q[key])
      end
   end
end


-- Select socket packet.
function webserver:recv_socket(s)
   s = s or self.socket  -- Default is 'the' socket
   local tagged = self:recv(function(msg) return msg[1] == s end)
   return tagged[2]
end

-- Called as the body of a new task actor_uv, to handle the tcp
-- connection.  recv() produces lines, as we are configured in line
-- mode.
function webserver:connect()
   -- First line is request
   local req = self:recv_socket()
   -- Rest is headers up to empty line.  Collect those in an array.
   local hdrs = {}
   while true do
      local line = self:recv_socket()
      if line == '\r\n' then break end
      table.insert(hdrs, line)
   end
   -- Handle request
   log("req: " .. req)
   local uri = string.match(req, "GET (.*) HTTP/1.1\r\n")
   local uri1, query = string.match(uri, "(.*)?(.*)")
   local q = {}
   if query then
      -- log("query: " .. query .. "\n")
      for key, val in string.gmatch(query .. "&", "(.-)=(.-)&") do
         -- log("kv: " .. key .. "='" .. val .. "'\n")
         q[key] = val
      end
      uri = uri1
   end
   -- log("uri: " .. uri .. "\n")
   -- Pass it to delegate
   self:serve(uri, q, hdrs)
end

function webserver.start(scheduler, serv_obj)
   serv_obj.mode = 'line' -- needed by webserver:handle()
   actor_uv.spawn_tcp_server(scheduler, serv_obj)
end

return webserver
