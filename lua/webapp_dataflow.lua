#!./lua.sh

-- This is for an application that displays a couple of coefficient
-- matrices and needs to send out messages to a device.

local uv        = require('lluv')
local webserver = require('lib.webserver')
local mixin     = require('lib.mixin')
local actor     = require('lib.actor')

require('lure.log')



-- behaviors
local testconn = {}

function testconn:websocket_loop()
   while true do
      local from, msg = unpack(self:recv())
      log_desc({from,msg})
      if from == self.socket then
         -- From websocket, packetized by push method.
      else
         -- From other sender
      end
   end
end

function body()
   local inputs = {}
   for row=1,8 do
      for col=1,8 do
         table.insert(
            inputs,
            -- document.getElementById('/matrix/1/1').valueAsNumber
            {'input', {
                type = 'number',
                id = '/matrix/' .. row .. '/' .. col,
                min = 0,
                max = 5,
                step = 0.1,
                value = 0,
            }})
      end
   end
   return {{'div',
           {class = 'image-grid'},
           inputs}}
end


local index =
   {'html',{},
    {{'head',{},
      {{'link', {rel="stylesheet", href="style.css"}},
       {'script', {type='module',src='./main.js',defer=true}}}},
     {'body',{},body()}}}

---- This is now a file in document_root
-- local main_js = [[
-- var proto = ({'https:': 'wss://', 'http:': 'ws://'})[window.location.protocol];
-- window.ws = new WebSocket(proto + location.host);
-- ]]

local document_root = arg[1]
assert(document_root)

-- https://www.reddit.com/r/javascript/comments/xeywrq/why_the_number_input_is_the_worst_input/
function testconn:serve(uri, q, hdrs)
   log_desc({uri=uri,q=q,hdrs=hdrs})
   if hdrs["Upgrade"] == "websocket" then
      self:upgrade_websocket(hdrs)
      return self:websocket_loop()
   end
   if uri == "/" then
      self:response_xhtml(index)
   --elseif uri == "/main.js" then
   --   self:response_javascript(main_js)
   else
      -- Try actual file and send 404 when it fails.
      -- Note that we are not in any way attempting to be secure here!
      -- Don't connect this thing to the open internet.
      self:response_file(document_root .. uri)
   end
end

local testserv = {}
function testserv:connection()
   local c = {}
   mixin.add(c, testconn)
   mixin.add(c, webserver)
   return c
end


-- instance
local s = {ip = '0.0.0.0', port = 8000 }
log_desc(s)
mixin.add(s, testserv)


local scheduler = actor.scheduler.new()
webserver.start(scheduler, s)
uv.signal():start(uv.SIGINT, function() uv.stop() end)
uv.run()
