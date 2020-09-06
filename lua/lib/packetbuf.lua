-- Packet buffering.  push() takes arbitrarily chunked data, and
-- push_packet() is called for each packet.

local packetbuf = {}

-- default implementation for convenience. override!
function packetbuf:push_packet(data)
   io.stderr:write("packet: " .. data)
end

local function read_bigendian(str, n)
   local acc = 0
   for i=1,n do acc = acc * 256 + str:byte(i) end
   return acc
end

function packetbuf:push(data)
   self.buf = self.buf .. data
   while true do
      if self.size_bytes > #self.buf then return end
      local len = read_bigendian(self.buf, self.size_bytes)
      local total = self.size_bytes + len
      if total > #self.buf then return end
      local packet = self.buf:sub(1+self.size_bytes, total)
      self.buf = self.buf:sub(total+1, self.buf:len())
      self:push_packet(packet)
   end
end

function packetbuf.new(size_bytes)
   local obj = {buf = "", size_bytes = size_bytes}
   setmetatable(obj, {__index = packetbuf})
   return obj
end

return packetbuf
