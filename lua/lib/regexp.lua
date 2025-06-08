-- Ad hoc collection of regexp matches and iterators.  This serves
-- mostly as an example.  Lua's regexp syntax is not something I seem
-- to be able to remember.
local m = {}

-- Unpack a line into key and value
function m.space_separated_kv(line)
   return string.match(line, "(%S+) (%S+)")
end


return m
