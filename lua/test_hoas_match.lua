-- Apparently is possible to turn a constructor into a destructor.
-- See example at the bottom.

require('lib.log')

-- That pattern generator function can be compiled to a datastructure
-- that can be used for matching and binding.

local function compile_pattern(pat_fun, nb)
   local args = {}      -- tracks argument order
   local bindings = {}  -- used for is_var()
   for i=1,nb do
      -- The table object's identity is used to refer to a variable.
      -- The content of the table is just for debugging.
      local arg = {i}
      args[i] = arg
      bindings[arg] = true
   end
   local pat = pat_fun(unpack(args))
   return { args = args, bindings = bindings, pat = pat}
end

local function trace(tag, thing)
   log_desc({trace = {tag, thing}})
end

local function match_pattern(top_expr, pat_bundle, bindings)
   local top_pat  = pat_bundle.pat
   local bindings = pat_bundle.bindings

   local m = {}
   local function mp(expr, pat)
      trace("ME", expr)
      trace("MP", pat)
      local texpr = type(expr)
      local tpat  = type(pat)
      if bindings[pat] then
         -- Binding a variable
         -- Check for duplicates
         trace("BIND",{pat,m})
         assert(m[var] == nil)
         m[pat] = expr
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
               -- Substructure matching.  Here we assume all tables
               -- are arrays (through ipairs iterator and # operator).
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
      return nil
   end
end

local function apply_match(pat_bundle, match_result, handler)
   assert(match_result)
   local args = {}
   for i=1,#pat_bundle.args do
      local var = pat_bundle.args[i]
      args[i] = match_result[var]
      assert(args[i])
   end
   return handler(unpack(args))
end


-- 1. Represent the pattern as a constructor.
local pattern_syntax =
   function(a, b)
      return {'add',a,b}
   end
-- 2. Compile the constructor into a data structure that can be used
--   for matching.
local pattern_obj = compile_pattern(pattern_syntax, 2)
log_desc({pattern_obj = pattern_obj})
-- 3. Perform the match.
local expr = {'add',100,200}
local match = match_pattern(expr, pattern_obj)
log_desc({expr = expr, match = match})
-- 4. Apply handler
function handler(a,b)
   log_desc({handler = {a = a, b = b}})
end
apply_match(pattern_obj, match, handler)
