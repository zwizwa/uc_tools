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
   -- Lua streams do not support writing booleans.
   elseif iol == true  then w('true')
   elseif iol == false then w('false')
   else
      if not (typ == 'string' or typ == 'number') then
         error('iolist.write: type=' .. typ)
      end
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
local function io_writer(stream)
   return function(...)
      write(
         function(str)
            stream:write(str)
         end,
         {...})
   end
end

local iolist = {
   write = write,
   stream_writer = stream_writer,
   io_writer = io_writer,
   to_string = to_string,
}
return iolist
