-- Erlang style iolists.

-- An iolist is a string, a thing that can be converted to string by
-- io.write() and friends, or an array of iolists.  It is used to
-- represent strings without using string concatenation.

-- FIXME: Rename functions.  This is too confusing.

local list = require('lib.tools.list')
local map = list.map

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

function iolist.write_to_stream(f, iol)
   local w = io_writer(f)
   w(iol)
   f:flush()
end

function iolist.join(connect_el, arr)
   local out_arr = {}
   local n = #arr
   for i,el in ipairs(arr) do 
      table.insert(out_arr, el)
      if i<n then table.insert(out_arr, connect_el) end
   end
   return out_arr
end

function iolist.w(iol, maybe_filename)
   if (type(maybe_filename) == 'string') then

      local file = io.open(maybe_filename, "w")
      iolist.write_to_stream(file, iol)
      file:close()
   else
      iolist.write_to_stream(io.stdout, iol)
   end
end

function iolist.prefix(sep, list, start)
   return map(function(p) return({sep,p}) end, list, start)
end
function iolist.postfix(list, sep)
   return map(function(p) return({p,sep}) end, list, start)
end



return iolist
