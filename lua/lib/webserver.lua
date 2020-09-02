-- Minimalistic webserver.
--
-- This is obviously not intended as a serious web server, but might
-- be useful as a plumbing tool in addition to being a test for the
-- actor and actor_uv libraries.

local actor     = require('lib.actor')
local actor_uv  = require('lib.actor_uv')
local scheduler = actor.scheduler.new()

local lxml      = require('lib.lxml')
local xml       = lxml.elements_to_string

local mixins    = require('lib.mixins')

local webserver = {}

local function log(str)
   io.stderr:write(str)
end

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

function webserver:response_html(lxml_element)
   log("->200\n")
   self.socket:write("HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n")
   self.socket:write(xml({lxml_element}))
end

function webserver:response_svg(lxml_element)
   log("->200\n")
   self.socket:write("HTTP/1.1 200 OK\r\nContent-Type: image/svg+xml\r\n\r\n")
   self.socket:write(svg({lxml_element}))
end

function webserver:response_404()
   log("->404\n")
   self.socket:write(
      "HTTP/1.1 404 Not Found\r\n\r\n404\r\n")
end

function webserver:handle()
   -- First line is request
   local req = self:recv()
   -- Rest is headers up to empty line.  Collect those in an array.
   local hdr = {}
   while true do
      local line = self:recv()
      if line == '\r\n' then break end
      table.insert(hdr, line)
   end
   -- Handle request
   -- log("req: " .. req)
   local uri = string.match(req, "GET (.*) HTTP/1.1\r\n")
   log("uri: " .. uri .. "\n")
   self:serve(uri, hdr)
end

-- This adds webserver, actor_uv, actor.task behaviors to obj.
-- User needs to add method or mixin that implements :serve(uri,hdr)
function webserver.start(port, obj)
   mixins.add(obj, webserver)
   return
      actor_uv.spawn_tcp_server(
         scheduler,
         {ip = '0.0.0.0', port = port, mode = 'line'},
         obj)
end

return webserver
