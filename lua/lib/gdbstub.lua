local actor_uv      = require('lib.actor_uv')
local actor         = require('lib.actor')
local gdbstub_lua51 = require('gdbstub_lua51')

local m = {}

-- Note that I wrote this a long time ago and only used it for
-- webserver.lua which has been used in a logsvg viewer tool.  The
-- code below is cloned from that tool.

function m.start(scheduler, tcp_port)
   assert(scheduler)
   assert(tcp_port)
   local stub = gdbstub_lua51.stub_new()
   local serv_obj = {
      ip = '0.0.0.0',
      port = tcp_port,
      mode = 'raw',
      stub = stub,
   }
   function serv_obj:connection()
      local c = {}
      function c:connect()
         while true do
            local msg = self:recv()
            local from, req = unpack(msg)
            assert(from == self.socket)
            log_desc({req=req})
            gdbstub_lua51.stub_write(stub, req)
            local rpl = gdbstub_lua51.stub_read(stub)
            if rpl then
               log_desc({rpl=rpl})
               self.socket:write(rpl)
            end
         end
      end
      -- mixin.add(c, mixin_class)
      return c
   end
   return actor_uv.spawn_tcp_server(scheduler, serv_obj)
end


return m
