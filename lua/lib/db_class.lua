-- require('lib.tools.log')
local m = { }
local C = require('sqlite3_lua51') 

-- While test db is a singleton, keep the interaction code abstract
-- wrt which DB it is acting on, so later we can split it up easier if
-- it gets complicated.
local m = { }

-- Generic DB code
function m.new(db_file)
   local obj = { }
   setmetatable(obj, {__index = m})
   obj.db = C.db_new()
   obj:open(db_file)
   return obj
end
function m:open(db_file)
   C.db_open(self.db, db_file)
end
function m:query(q)
   return C.db_query(self.db, q)
end

-- Perform query, return array of tables with named fields.
function m:tab_select(fields, clause, ...)
   local q = 'select ' .. table.concat(fields, ',') .. ' ' .. clause
   local rows = self:query({q, ...})
   -- log_desc({tab_seqlect_rows=rows, q=q})
   for r, row in ipairs(rows) do
      local named = {}
      for c, field in ipairs(fields) do
         named[field] = row[c]
      end
      rows[r] = named -- Just replace it...
   end
   return rows
end

-- Wrapper to check for exactly one row result.
function m.one(rows, default)
   if #rows == 0 then
      -- This might be an error depending on caller.
      if default == nil then
         error('one: no result from db query')
      else
         return default
      end
   end
   -- #rows >1 is always an error: internal DB consistency issue
   assert(#rows == 1)
   return rows[1]
end


return m
