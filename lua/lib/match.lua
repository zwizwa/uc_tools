-- Encoding algebraic types and pattern matching in Lua seems to be
-- simplest to do using tagged arrays.  See explanation in
-- test_match.lua
local match = {}
function match.match(data, pattern)
   local cons = data[1]
   local case_fun = pattern[cons]
   return case_fun(unpack(data,2))
end
return match
