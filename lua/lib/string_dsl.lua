-- Tools for string DSLs
-- Make up for lack of macros using interned strings and reflection.
require ('lib.log')
local function trace(tag, expr)
   -- log(tag) ; log(": ") ; log_desc(expr)
end

local lib = {}

function lib.memo_compile(fragment, s)
   local memo = s.memo
   if memo then
      -- Strings are interned.
      local fun = memo[fragment]
      if fun then
         trace("MEMO",fragment)
         return fun
      end
   end
   local ctx_var = s.ctx_var or '_'
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
   if memo then
      memo[fragment] = fun
   end
   return fun
end

return lib
