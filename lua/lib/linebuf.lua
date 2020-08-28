-- Line buffering.  push() takes arbitrarily chunked data, and line()
-- is called for each line.

local linebuf = {}

-- default implementation for convenience. override!
function linebuf:push_line(data)
   io.stderr:write("line: " .. data)
end

function linebuf:push(data)
   self.buf = self.buf .. data
   while true do
      local line = string.match(self.buf, '([^\n]+\n)')
      if not line then return end
      self:push_line(line)
      self.buf = self.buf:sub(line:len()+1, self.buf:len())
   end
end

function linebuf.new(tag)
   local obj = {tag = tag or "", buf = ""}
   setmetatable(obj, {__index = linebuf})
   return obj
end

return linebuf
