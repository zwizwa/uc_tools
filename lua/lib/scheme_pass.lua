-- I'd like to isolate these passes:
--
-- MACRO-EXPAND
-- A-NORMAL FORM
-- OUTPUT CLEANUP

local se = require('lib.se')


-- Bind macros to state object for gensym.
local macro = {} ; do
   for name, m in pairs(require('lib.scheme_macros')) do
      log("MACRO: " .. name .. "\n")
      macro[name] = function(s, expr)
         return m(expr, { state = s })
      end
   end
end

-- Define types
local function expr_type(e)
   local typ = type(e)
   if typ == 'table' then
      if e[1] ~= nil and e[2] ~= nil then
         return 'pair'
      else
         log_desc(e)
         error('bad table')
      end
   end
   return typ
end

local function s_id(s, thing) return thing end

local expander = {
   ['string'] = s_id,
   ['number'] = s_id,
   ['pair'] = function(s, expr)
      local car, cdr = unpack(expr)
      local m = s.macro[car]
      if m ~= nil then
         return m(s, expr), true
      else
         return expr
      end
   end
}


local function expand_step(s, expr)
   local typ = expr_type(expr)
   local f = expander[typ]
   if f == nil then error('bad type ' .. typ) end
   return f(s, expr)
end

local function expand(s, expr)
   local again = true
   while again do
      log_se_n(se.car(expr), "STEP: ")
      expr, again = expand_step(s, expr)
   end
   return expr
end

local class = {
   expand_step = expand_step,
   expand = expand,
   macro = macro,
}

local function new()
   local obj = { }
   setmetatable(obj, { __index = class })
   return obj
end

class.new = new
return class

