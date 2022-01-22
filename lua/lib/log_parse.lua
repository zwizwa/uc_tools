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

function log_parse.lines(file, parse)
   local file = C.new_log_file(file)
   if parse then
      -- Re-use parser.  FIXME: Clean up semantics.  What we want to
      -- do is proably to implement certain start conditions, such as
      -- scanning up to sync point in C code before passing things to
      -- Lua.  For now just call reset, which resets offset to 0.
      C.reset(parse)
   else
      parse = C.new_log_parse()
   end
   local function gen()
      return C.next(parse, file)
      -- FIXME: Test end condition
   end
   return gen
end

return log_parse
