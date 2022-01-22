local log_parse = {}

local C = require('log_parse_lua51')

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

-- Create iterator.  Allow parser to be re-used, e.g. to first wind
-- parser to a certain offset, then use it as Lua iterator.
function log_parse.lines(file, parse)
   local file = C.new_log_file(file)
   if not parse then
      parse = C.new_log_parse()
   end
   local function gen()
      return C.next_string(parse, file)
      -- FIXME: Test end condition
   end
   return gen
end

function log_parse.indices(file, parse)
   local file = C.new_log_file(file)
   if not parse then
      parse = C.new_log_parse()
   end
   local function gen()
      return C.next_index(parse, file)
      -- FIXME: Test end condition
   end
   return gen
end

return log_parse
