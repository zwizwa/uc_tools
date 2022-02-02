local log_parse = {}

local C = require('log_parse_lua51')

-- Exposed mostly for testing.
-- This this is not intended to be a stable API
log_parse.C = C

function log_parse.new(config)
   local bin_to_string = config and config.bin_to_string
   local obj = { parse = C.new_log_parse() }
   local push_chunk = C.to_string_mv
   if bin_to_string then
      push_chunk = C.to_raw_mv
   end
   function obj:to_strings(chunk)
      -- FIXME: C routine just pushes to Lua stack.  Can that actually
      -- overflow?  Maybe it's best to have C code push to array
      -- instead?
      local function convert(...)
         local in_strs = {...}
         local out_strs = {}
         for _,str in ipairs(in_strs) do
            if str:byte(9) == 0 then
               -- FIXME: This is not a great interface.  Again, have
               -- to go fast so yet another hack...  The byte after
               -- the 8 hex digits is 0 when the rest of the message
               -- is binary
               local hex = str:sub(1,8)
               local bin = str:sub(10, #str)
               str = hex .. " " .. bin_to_string(bin) .. "\n"
            end
            table.insert(out_strs, str)
         end
         return out_strs
      end
      return convert(push_chunk(self.parse, chunk))
   end
   return obj
end


function log_parse.messages(config)
   assert(config)
   assert(config.next)
   assert(config.file)
   local filename = config.file
   local nxt = config.next
   -- FIXME: C code only supports a single byte prefix atm.
   local wind_prefix = config and config.wind and config.wind[1]
   local file
   if filename == '-' then
      file = 0 -- iterator supports file descriptor numbers
   else
      file = C.new_log_file(filename)
   end
   log_desc({filename = filename, file = file})
   local parse = C.new_log_parse()
   if wind_prefix then
      local offset = C.wind_prefix(parse, file, wind_prefix)
      -- io.stderr:write(filename .. ": wind offset = " .. offset .. "\n")
   end
   return function() return nxt(parse, file) end
end

-- Expose these so they can be passed as .messages({next = ...})
log_parse.next_string    = C.next_string
log_parse.next_raw       = C.next_raw
log_parse.next_ts_string = C.next_ts_string
log_parse.next_ts_raw    = C.next_ts_raw

-- Use {next = ...} in config
-- function log_parse.lines_string(config)
--    config.next = C.next_string
--    return log_parse.messages(config)
-- end
-- function log_parse.ts_messages(config)
--    config.next = C.next_ts_string
--    return log_parse.messages(config)
-- end
-- function log_parse.ts_lines_bin(config)
--    config.next = C.next_ts_raw
--    return log_parse.messages(config)
-- end
-- function log_parse.ts_raw(config)
--    config.next = C.next_bin
--    return log_parse.messages(config)
-- end

function log_parse.indices(file)
   local file = C.new_log_file(file)
   local parse = C.new_log_parse()
   local function gen() return C.next_index(parse, file) end
   return gen
end

return log_parse
