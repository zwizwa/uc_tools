-- Quasi-quoting matcher.
-- How to best put this together?
-- There are patterns and there are match results.

-- I don't want to take it as far as generating lua code for the match
-- handling, so use a table to represent the bindings.  I don't really
-- care for speed much.

-- As for the matching pattern, this is probably best done as a DSL
-- that takes strings.  The parsing can be memoized.

-- This is written as a DSL so it can be used from Lua
-- See test_se_match.lua

-- The simplest way seems to memoize the patterns.  For most practical
-- uses this is going to be enough. The memo table can go into the
-- state object.  I've been cursing at Lua's interned strings for a
-- while but I guess they are good for something!

local se = require('lib.se')
require('lib.log_se')
local car = se.car
local cdr = se.cdr

local class = {}

local function trace(tag, expr)
   log_se_n(expr, tag .. ": ")
end

local function unquote_var(pat)
   if not se.is_expr(pat, 'unquote') then return nil end
   local _, var = se.unpack(pat, {n = 2})
   return var
end

local function match_pattern(top_expr, top_pat)
   local m = {}
   local function mp(expr, pat)
      trace("ME", expr)
      trace("MP", pat)
      local texpr = type(expr)
      local tpat  = type(pat)
      local var = unquote_var(pat)
      if var then
         -- Binding a variable
         -- Check for duplicates
         assert(m[var] == nil)
         trace("BIND",var)
         m[var] = expr
         return true
      else
         -- Checking literal match
         if texpr ~= tpat then
            -- Types don't match
            return false
         else
            -- Types match
            if texpr ~= 'table' then
               -- Primitive value
               return texpr == tpat
            else
               -- Pair matching
               return
                  mp(car(expr),car(pat)) and
                  mp(cdr(expr),cdr(pat))
            end
         end
      end
   end
   if mp(top_expr, top_pat) then
      log_desc({m = m})
      return m
   else
      return nil
   end
end

function class:match(expr, pats)
   for _,clause in ipairs(pats) do
      local spat, handle = unpack(clause)
      local pat = self.memo[spat]
      if not path then
         pat = se.read_string(spat)
         trace("PAT",pat)
         self.memo[spat] = pat
      end
      local m = match_pattern(expr, pat)
      if m then return handle(m) end
   end
   return nil
end
function class.new()
   local obj = { memo = {} }
   setmetatable(obj, { __index = class })
   return obj
end

return class
