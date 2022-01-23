local log_parse = {}

local C = require('log_parse_lua51')
log_parse.C = C -- Exposed, but this is not intended to be stable


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

-- FIXME: C.reset is not needed?
-- FIXME: Remove some duplication?

function log_parse.lines(file)
   local file = C.new_log_file(file)
   local parse = C.new_log_parse()
   local function gen() return C.next_string(parse, file) end
   return gen
end

function log_parse.ts_lines(filename, config)
   -- FIXME: C code only supports a single byte prefix atm.
   local wind_prefix = config and config.wind and config.wind[1]
   local file = C.new_log_file(filename)
   local parse = C.new_log_parse()
   if wind_prefix then
      local offset = C.wind_prefix(parse, file, wind_prefix)
      -- io.stderr:write(filename .. ": wind offset = " .. offset .. "\n")
   end

   local function gen() return C.next_ts_string(parse, file) end
   return gen
end

function log_parse.ts_bin(filename, config)
   -- FIXME: C code only supports a single byte prefix atm.
   local wind_prefix = config and config.wind and config.wind[1]
   local file = C.new_log_file(filename)
   local parse = C.new_log_parse()
   if wind_prefix then
      local offset = C.wind_prefix(parse, file, wind_prefix)
      -- io.stderr:write(filename .. ": wind offset = " .. offset .. "\n")
   end

   local function gen() return C.next_ts_bin(parse, file) end
   return gen
end

function log_parse.indices(file)
   local file = C.new_log_file(file)
   local parse = C.new_log_parse()
   local function gen() return C.next_index(parse, file) end
   return gen
end

return log_parse
