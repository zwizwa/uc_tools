local log_parse = {}

local C = require('log_parse_lua51')

-- Exposed mostly for testing.
-- This this is not intended to be a stable API
log_parse.C = C

function log_parse.new()
   local obj = { parse = C.new_log_parse() }
   function obj:to_strings(chunk)
      -- FIXME: C routine just pushes to Lua stack.  Can that actually
      -- overflow?  Maybe it's best to have C code push to array
      -- instead?
      local function pack(...) return {...} end
      return pack(C.to_string_mv(self.parse, chunk))
   end
   return obj
end


function log_parse.lines_with_next(filename, config, nxt)
   assert(filename)
   assert(nxt)
   -- FIXME: C code only supports a single byte prefix atm.
   local wind_prefix = config and config.wind and config.wind[1]
   local file = C.new_log_file(filename)
   local parse = C.new_log_parse()
   if wind_prefix then
      local offset = C.wind_prefix(parse, file, wind_prefix)
      -- io.stderr:write(filename .. ": wind offset = " .. offset .. "\n")
   end
   return function() return nxt(parse, file) end
end

function log_parse.lines(filename, config)
   return log_parse.lines_with_next(filename, config, C.next_string)
end
function log_parse.ts_lines(filename, config)
   return log_parse.lines_with_next(filename, config, C.next_ts_string)
end
function log_parse.ts_lines_bin(filename, config)
   return log_parse.lines_with_next(filename, config, C.next_ts_bin)
end

function log_parse.indices(file)
   local file = C.new_log_file(file)
   local parse = C.new_log_parse()
   local function gen() return C.next_index(parse, file) end
   return gen
end

return log_parse
