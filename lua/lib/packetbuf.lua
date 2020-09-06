-- Packet buffering.  push() takes arbitrarily chunked data, and
-- push_packet() is called for each packet.

local packetbuf = {}

-- default implementation for convenience. override!
function packetbuf:push_packet(data)
   io.stderr:write("packet: " .. data)
end


-- -- Buffered read, exact number of bytes.
-- function actor_uv:read_exact(socket, buf, nb_bytes)
--    while true do
--       if #buf.buf >= nb_bytes then
--          local chunk = buf.buf:sub(1,nb_bytes)
--          buf = buf.buf:sub(nb_bytes+1, buf.buf:len())
--          return chunk
--       end
--       socket:read(
         
--       local chunk = 
         
         
   


function packetbuf:push(data)
   self.buf = self.buf .. data
   while true do
      local packet = string.match(self.buf, '([^\n]+\n)')
      if not packet then return end
      self:push_packet(packet)
      self.buf = self.buf:sub(packet:len()+1, self.buf:len())
   end
end

function packetbuf.new(tag)
   local obj = {tag = tag or "", buf = ""}
   setmetatable(obj, {__index = packetbuf})
   return obj
end

return packetbuf
