-- Erlang style iolists.

-- An iolist is a string, a thing that can be converted to string by
-- io.write() and friends, or an array of iolists.  It is used to
-- represent strings without using string concatenation.

-- FIXME: Rename functions.  This is too confusing.

local iolist = { }

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
iolist.write = write

local function to_string(iol)
   local out = {}
   write(function(atom) table.insert(out, atom) end, iol)
   return table.concat(out)
end
iolist.to_string = to_string

local function stream_writer(write_string)
   return function(...) write(write_string,{...}) end
end
iolist.stream_writer = stream_writer

local function io_writer(stream)
   return function(...)
      write(
         function(str)
            stream:write(str)
         end,
         {...})
   end
end
iolist.io_writer = io_writer

function iolist.write_to_file(filename, iol)
   local f = io.open(filename, 'w')
   assert(f)
   local w = io_writer(f)
   w(iol)
   f:close()
end



return iolist
