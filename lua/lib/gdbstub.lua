local actor_uv      = require('lib.actor_uv')
local actor         = require('lib.actor')
local gdbstub_lua51 = require('gdbstub_lua51')

local m = {}

function m.start(scheduler, tcp_port)
   assert(scheduler)
   assert(tcp_port)
   local stub = gdbstub_lua51.stub_new()

   -- Memory access callbacks
   local mem = {}
   --function mem.flash_erase(addr, size) return 0 end
   --function mem.flash_write(addr, bytes) return 0 end
   --function mem.write(addr, byte) end
   --function mem.write32(addr, word) end
   function mem.read(addr) return 0xAA end

   local serv_obj = {
      ip = '0.0.0.0',
      port = tcp_port,
      mode = 'raw',
   }
   function serv_obj:connection()
      local c = {}
      function c:connect()
         while true do
            local msg = self:recv()
            local from, req = unpack(msg)
            assert(from == self.socket)
            -- log_desc({req=req})
            gdbstub_lua51.stub_write(stub, mem, req)
            local rpl = gdbstub_lua51.stub_read(stub, mem)
            if rpl then
               -- log_desc({rpl=rpl})
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
