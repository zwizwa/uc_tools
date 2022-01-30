-- Tools for string DSLs
-- Make up for lack of macros using interned strings and reflection.
require ('lib.log')
local function trace(tag, expr)
   log(tag) ; log(": ") ; log_desc(expr)
end

local lib = {}

-- Alternative lambda syntax.
function lib.lambda(fragment, state)
   local s = state or {}
   local ctx_var = s.var or '_'
   trace("EXPAND", fragment)
   local lcode =
      table.concat(
         {"return function(",ctx_var,") return (",fragment,") end"},
         "")
   trace("LCODE", lcode)
   local f = loadstring(lcode)
   if s.env then setfenv(f, s.env) end
   assert(f)
   local fun = f()
   return fun
end

function lib.memo_eval(compile, str, s)
   local memo = s.memo
   if memo then
      -- Strings are interned.
      local val = memo[str]
      if val then
         trace("MEMO",str)
         return val
      end
   end
   local val = compile(str, s)
   if memo then
      memo[str] = val
   end
   return val
end

return lib
