local actor_uv      = require('lib.actor_uv')
local actor         = require('lib.actor')

local m = {}

local gdbstub_lua51 = require('gdbstub_lua51')

-- Note that the whole point of this is producing a wrapper around
-- target memory read, where target is not directly accessible so we
-- need to use some kind of RPC mechanism to retrieve the memory
-- contents.  In my setup, the wait-for-reply is implemented using Lua
-- coroutines.  However, coroutine yield does not work from a C call,
-- so the implementation that used Lua callbacks has been removed, in
-- favor of exposing the packet decoder and handling some packets in
-- Lua.

function m.start(scheduler, tcp_port, mem)
   assert(scheduler)
   assert(tcp_port)
   local C = gdbstub_lua51
   local stub = C.stub_new()

   local serv_obj = {
      ip = '0.0.0.0',
      port = tcp_port,
      mode = 'raw',
   }

   local function hex_sub(packet,from,to)
      local str = packet:sub(from, to)
      return tonumber(str, 16)
   end
   local function rpl_bytes(bytes, nb)
      C.rpl_begin(stub)
      for i=1,nb do
         -- FIXME: Implement error handling.  For now just return
         -- obvious junk.  Not 0x00 and not 0xFF.
         local byte = bytes[i] or 0x55
         C.rpl_hex_cs(stub, byte)
      end
      C.rpl_end(stub)
   end

   -- If these return true, a response has been generated.
   local parse = {}
   function parse.m(p)
      -- E.g. m5555560d,8 Memory read
      if p:byte(1) ~= 109 then return false end -- m
      assert(p:byte(10) == 44) -- ,
      local addr  = hex_sub(p,2,9)
      local nb    = hex_sub(p,11)
      local bytes = mem.read(addr, nb) or { error = 'read failed' }
      -- log_desc({bytes = bytes})
      rpl_bytes(bytes, nb)
      return true
   end
   local function do_parse(packet)
      for name, fun in pairs(parse) do
         if fun(packet) then
            -- One of our handlers produced a reply.
            break
         end
      end
      -- Note of the handlers took it. Let the C code handle it
      C.stub_interpret(stub)
   end

   function serv_obj:connection()
      local c = {}
      function c:connect()
         while true do
            local msg = self:recv()
            local from, req = unpack(msg)
            assert(from == self.socket)
            for i=1,#req do
               local status, packet = C.stub_putchar(stub, req:byte(i))
               if status == 0 then
                  do_parse(packet)
               else
                  -- FIXME: error handling
               end
            end
            local rpl = C.stub_read(stub)
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

