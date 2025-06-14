-- Functions for working with data structure differences.

local tab = require('lib.tools.tab')

local m = {}
-- Produce a nested table with differences.
--
-- This assumes the structure of a and b is the same, so no support
-- for node insertion/deletion.
function m.diff(a, b, mark)
   -- log_desc({a=a,b=b})
   if nil == mark then mark = function(a,b) return {a,b} end end
   -- FIXME: We do not check that a and b have the same structure.
   -- All keys come from a.
   local out = {}
   for k,va in pairs(a) do
      -- log_desc({k=k})
      local vb = b[k]
      assert(nil ~= vb)
      if type(va) == 'table' then
         local diff_sub = m.diff(va, vb, mark)
         if not tab.is_empty(diff_sub) then
            out[k] = diff_sub
         else
            -- Don't add empty nodes
         end
      else
         if va ~= vb then
            out[k] = mark(va, vb)
         end
      end
   end
   return out
end

-- In some cases coordiantes / indices are easier to work with than
-- explicit diff data structures.  Provide some iterators.

-- Two dimensional iteration.
function m.for2(cfg)
   return function(new, old)
      -- i: outer index/key
      for i, new_i in cfg.outer(new) do
         local old_i = old[i]
         -- j: inner index/key
         for j, new_ij in cfg.inner(new_i) do
            local old_ij = old_i[j]
            if new_ij == old_ij and cfg.no_change then
               cfg.no_change(i, j)
            elseif cfg.change then
               cfg.change(i, j, new_ij, old_ij)
            end
         end
      end
   end
end

-- Iterate over an array of tables.
function m.for_ipairs_pairs(cfg)
   return m.for2({
         outer = ipairs,
         inner = pairs,
         change = cfg.change,
         no_change = cfg.no_change
   })
end



return m
