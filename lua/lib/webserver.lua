-- Minimalistic webserver.
--
-- This is obviously not intended as a serious web server, but might
-- be useful as a plumbing tool in addition to being a test for the
-- actor and actor_uv libraries.

local actor_uv  = require('lib.actor_uv')
local lxml      = require('lib.lxml')
local mixin     = require('lib.mixin')

local webserver_lua51 = require('webserver_lua51')

local function log(str)
   io.stderr:write(str)
end


-- Behavior associated to a single connection.
local webserver = {}

function webserver:write(str)
   return self:write_socket(self.socket, str)
end


-- FIXME: This is mostly for dynamically generated content, so maybe
-- make "Cache-Control: no-store" a default?
-- https://stackoverflow.com/questions/866822/why-both-no-cache-and-no-store-should-be-used-in-http-response

function webserver:response_html(lxml_element)
   -- local function write_cb() log("write_cb\n") end
   log("->200\n")
   self:write("HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n")
   self:write(lxml.elements_to_string({lxml_element}))
end

function webserver:response_xhtml(lxml_element)
   -- local function write_cb() log("write_cb\n") end
   log("->200\n")
   self:write("HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n")
   self:write("<!DOCTYPE html>\n")
   self:write(lxml.elements_to_string({lxml_element}))
end

function webserver:response_svg(lxml_element)
   log("->200\n")
   self:write("HTTP/1.1 200 OK\r\nContent-Type: image/svg+xml\r\n\r\n")
   self:write(lxml.elements_to_string({lxml_element}))
end

function webserver:response_javascript(script)
   log("->200\n")
   self:write("HTTP/1.1 200 OK\r\nContent-Type: text/javascript\r\n\r\n")
   self:write(script)
end

function webserver:response_css(stylesheet)
   log("->200\n")
   self:write("HTTP/1.1 200 OK\r\nContent-Type: text/css\r\n\r\n")
   self:write(stylesheet)
end

function webserver:response_content_type(content_type, content)
   log("->200\n")
   self:write("HTTP/1.1 200 OK\r\nContent-Type: " .. content_type .. "\r\n\r\n")
   self:write(content)
end

local extensions = {
   ['.js'] = 'text/javascript',
   ['.css'] = 'text/css',
   ['.html'] = 'text/html',
}
function extension_match(filename, extension)
   local ext = string.sub(filename, 1 + #filename - #extension)
   -- log_desc({ext=ext, extension=extension})
   return extension == ext
end

function type_from_extension(filename)
   for extension, content_type in pairs(extensions) do
      if extension_match(filename, extension) then
         return content_type
      end
   end
   return 'text/plain'
end

function webserver:response_file(filename)
   local file = io.open(filename, 'r')
   if file then
      log("->200\n")
      -- local content_type = "text/plain"
      -- local content_type = "text/html"
      local content_type = type_from_extension(filename)
      local contents = file:read("*all")
      self:write("HTTP/1.1 200 OK\r\nContent-Type: " .. content_type .. "\r\n\r\n")
      self:write(contents)
      file:close()
   else
      self:response_404()
   end
end

function webserver:response_404()
   log("->404\n")
   self.socket:write("HTTP/1.1 404 Not Found\r\n\r\n404\r\n")
end

function webserver:response_upgrade_websocket(hdrs)
   log("->websocket\n")
   assert(hdrs["Upgrade"] == "websocket")
   local key = hdrs["Sec-WebSocket-Key"]
   assert(key)
   local hash = webserver_lua51.websocket_sha1(key)
   -- log_desc({hash=hash})
   local resp =
      "HTTP/1.1 101 Switching Protocols\r\n" ..
      "Upgrade: websocket\r\n" ..
      "Connection: Upgrade\r\n" ..
      "Sec-WebSocket-Accept: " .. hash .. "\r\n\r\n"
   -- log(resp)
   self.socket:write(resp)
end

function webserver:upgrade_websocket(hdrs)

   -- Before sending the reply, change the input mode by changing the
   -- libuv callback.  This calls into C code to convert the websocket
   -- protocol to regular binary messages, then delivers those to the
   -- mailbox of the task so they show up in the mail loop along side
   -- messages from other tasks.
   local ws = webserver_lua51
   local wsp = ws.websocket_parse_new()
   self.push = function(data)
      -- To test in browser console:
      -- w = new WebSocket("ws://zoe:8800")
      -- enc = new TextEncoder();
      -- w.send(enc.encode("asdf"))

      -- log_desc({websocket_push = data})
      local msgs = {ws.websocket_parse_push_chunk(wsp, data)}
      for _, msg in ipairs(msgs) do
         self:send_and_schedule({self.socket,msg})
      end
   end

   -- Send response header
   self:response_upgrade_websocket(hdrs)

   function dbg_message(msg)
      local encoded = ws.websocket_encode(wsp, msg)
      self.socket:write(encoded)
   end
   dbg_message("hello\n")
   dbg_message("123\n")

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
   local method, uri = string.match(req, "(%S+) (.*) HTTP/1.1\r\n")
   -- log_desc({method=method, uri=uri})

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

   -- Note: I do not remember if hdrs is actually used anywhere, so I
   -- am going to change the API here to send a map.
   self:serve(uri, q, webserver.parse_headers(hdrs), method)
end

function webserver.start(scheduler, serv_obj)
   serv_obj.mode = 'line' -- needed by webserver:handle()
   return actor_uv.spawn_tcp_server(scheduler, serv_obj)
end

-- FIXME: This seems to work but is not matching the spec exactly.
local function header_match(line)
   return string.gmatch(line, "(%S+): (.+)\r\n")
end

-- Split the header lines into a dictionary.
function webserver.parse_headers(hdr_lines)
   local tab = {}
   for _, line in ipairs(hdr_lines) do
      -- Regexp seems to work, but I didn't think this through.
      for key, value in header_match(line) do
         tab[key] = value
      end
   end
   return tab
end

-- Parse a form response containing a file.  I'm not really clear on
-- what exactly this should be, so just hack it so it works for
-- current firefox and chromium/webkit.  Also this assumes that the
-- file encoding is just plaintext that can be split into lines.
function webserver.parse_post_response(in_lines)
   local out_lines = {}
   local hdr_lines = {}
   -- HACKS: assume the boundary markers are on the first and last
   -- lines so we can just skip those, assume there is only one
   -- message (e.g. no multipart).
   local in_header = true
   for i=2,#in_lines-1 do
      local line = in_lines[i]
      if line == '\r\n' then
         in_header = false
      elseif in_header then
         table.insert(hdr_lines, line)
      else
         table.insert(out_lines, line)
      end
   end
   -- log_desc({hdr_lines = hdr_lines})
   local hdr = webserver.parse_headers(hdr_lines)
   hdr.contents = out_lines
   return hdr
end



return webserver
