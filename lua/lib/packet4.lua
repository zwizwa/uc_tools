-- {packet,4} TCP bridge
-- Adapted from webserver.lua

local actor_uv  = require('lib.actor_uv')
local lxml      = require('lib.lxml')
local mixin     = require('lib.mixin')

local function log(str)
   io.stderr:write(str)
end


-- Behavior associated to a single connection.
local packet4 = {}

function packet4:write(str)
   return self:write_socket(self.socket, str)
end



-- Called as the body of a new task actor_uv, to handle the tcp
-- connection.  recv() produces lines, as we are configured in line
-- mode.
function packet4:connect()
   while true do
      local msg = self:recv()
      log("#msg = " .. #msg .. "\n")
   end
end

function packet4.start(scheduler, serv_obj)
   serv_obj.mode = {'packet',4}
   actor_uv.spawn_tcp_server(scheduler, serv_obj)
end

return packet4
