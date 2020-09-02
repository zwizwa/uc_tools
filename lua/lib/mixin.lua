-- I am not a fan of multiple-inheritance style abstractions, but this
-- one seems to present itself quite naturally in the implementation
-- of tasks.  Typically the user wants to create a task that abstracts
-- over functionality implemented by the task abstraction:
-- send/recv/sleep etc..  Doing this in Lua with explicit deligation
-- seems just very cumbersome, so just embrace the big soup of methods
-- paradigm.

local mixin = {}

-- Add mixin support to an object.
function mixin:add(m)
   assert(self)
   assert(m)
   if not self.mixins then
      mixin.init_(self)
   end
   assert(self.mixins)
   -- FIXME: Probably best to always let the new mixin override the
   -- old ones.
   table.insert(self.mixins, 1, m)
end

function mixin:init_()
   assert(self)
   self.mixins = {}
   setmetatable(
      self,
      {__index =
          function(o,k)
             -- 1. Actual members
             local m = rawget(o, k)
             if m then return m end
             -- 2. Scan through the list of behaviors
             for i, tab in ipairs(self.mixins) do
                local m = tab[k]
                if m then return m end
             end
             return nil
          end})
end

return mixin
