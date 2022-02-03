local set   = require('lure.set')

-- Constructor inversion matcher.
local match = {}

-- Convert constructor (table -> data) into pattern matcher object.
function match.compile(pat_fun, opt_env)
   local vars  = {} -- set of variables
   local probe = {} -- empty object, metatable used to capture refereneces
   setmetatable(
      probe,
      {__index = function(_, var_name)
          -- Create var object.  Object identity will be used to
          -- distinguish between data and variables in a pattern.
          local var = { var = var_name }
          vars[var] = true
          return var
      end})
   local pat = pat_fun(probe)
   return { vars = vars, pat = pat }
end

local function trace(tag, thing)
   -- log_desc({trace = {tag, thing}})
end

-- Apply pattern matcher object to data structure, returning match
-- table or nil.
function match.apply(pattern_obj, top_expr)
   local top_pat = pattern_obj.pat
   local vars    = pattern_obj.vars

   local m = {}
   local function mp(expr, pat)
      trace("ME", expr)
      trace("MP", pat)
      local texpr = type(expr)
      local tpat  = type(pat)
      local var   = vars[pat]
      if var then
         -- Binding a variable
         -- Check for duplicates
         local var_name = pat.var
         trace("BIND",{var_name,m})
         assert(m[var_name] == nil)
         m[var_name] = expr
         return true
      else
         -- Perform literal match
         if texpr ~= tpat then
            -- Types don't match
            trace("!TYPE",{texpr,tpat})
            return false
         else
            -- Types match
            if texpr ~= 'table' then
               -- Primitive value
               trace("PRIM",{texpr,tpat})
               return expr == pat
            else
               -- Substructure matching uses ipairs (1).
               if #expr ~= #pat then
                  -- Need to be same size
                  trace("NARG",{texpr,tpat})
                  return false
               end
               for i=1,#expr do
                  local sub_expr = expr[i]
                  local sub_pat  = pat[i]
                  if not mp(sub_expr, sub_pat) then
                     -- Subexpression mismatch
                     return false
                  end
               end
               return true
            end
         end
      end
   end
   if mp(top_expr, top_pat) then
      -- log_desc({m = m})
      return m
   else
      return false
   end
end

-- This is not as efficient as we can't re-use compiled patterns this
-- way.  We can't memoize either as Lua will create fresh closures.
function match.match(expr, clauses)
   for _,clause in ipairs(clauses) do
      local pat, handle = unpack(clause)
      local cpat = match.compile(pat)
      local m = match.apply(cpat, expr)
      if m then return handle(m) end
   end
   return false
end


-- (1) The reason this library exists is to match nested
-- s-expressions.  Comparing by generic keys is currently not needed.
-- Probably we do want subset matching in that case (e.g. ignore keys
-- in the input pattern that are not in the matcher pattern).

return match
