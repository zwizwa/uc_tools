-- Erlang style iolists.

-- An iolist is a string, a thing that can be converted to string by
-- io.write() and friends, or an array of iolists.  It is used to
-- represent strings without using string concatenation.

local function write(w,iol)
   local typ = type(iol)
   if (typ == 'table') then
      for _,sub in ipairs(iol) do
         write(w,sub)
      end
   else
      w(iol)
   end
end
local function to_string(iol)
   local out = {}
   write(function(atom) table.insert(out, atom) end, iol)
   return table.concat(out)
end
local function stream_writer(write_string)
   return function(...) write(write_string,{...}) end
end

local iolist = {
   write = write,
   stream_writer = stream_writer,
   to_string = to_string,
}
return iolist
