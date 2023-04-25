local actor_uv      = require('lib.actor_uv')
local actor         = require('lib.actor')

local m = {}

local gdbstub_lua51 = require('gdbstub_lua51')


-- Note that you can't have coroutine yield from inside a C call, so
-- memory read command 'm' is implemented separately.  To this end
-- some of the internals are exposed in gdbstub_lua51

local parse = {}
function parse.m(packet)
   -- E.g. m5555560d,8 Memory read
   if packet:byte(1) ~= 109 then return end -- m
   assert(packet:byte(10) == 44) -- ,
   local addr_str = packet:sub(2,9) ; local addr = tonumber(addr_str, 16)
   local nb_str   = packet:sub(11)
   local nb       = tonumber(nb_str, 10)
   -- log_desc({addr_str = addr_str, nb_str = nb_str})
   -- log_desc({addr = addr, nb = nb})
   return nil
end

function m.start(scheduler, tcp_port, callbacks)
   assert(scheduler)
   assert(tcp_port)
   local C = gdbstub_lua51
   local stub = C.stub_new()

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

            -- We're not using stub_write here, but instead feed the
            -- decoder directly so we can implement certain packets
            -- separately.
            if false then
               C.stub_write(stub, callbacks, req)
            else
               for i=1,#req do
                  local status, packet = C.stub_putchar(stub, req:byte(i))
                  local parsed
                  if status == 0 then
                     -- We get a copy to do some local dispatch.
                     log_desc({packet=packet})
                     local parsed
                     if parse.m(packet) then
                        --
                     else
                        -- Let it handle internally.
                        C.stub_interpret(stub, callbacks)
                     end
                  else
                     -- FIXME: Handle errors
                  end
               end
            end
            local rpl = C.stub_read(stub, callbacks)
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

