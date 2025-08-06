require ('lure.log')

local diff = require('lure.diff')
local tree = require('lure.tree')

local function run()
   local function test(a, b, expect)
      local d = diff.diff(a, b)
      if expect ~= nil then
         assert(tree.is_equal(d, expect))
      else
         log_desc(d)
      end
   end

   test(
      {a=1},
      {a=2},
      {a={1,2}}
   )
   test(
      {a={b=1}},
      {a={b=2}},
      {a={b={1,2}}}
   )
   test(
      {c=123,a={b=1}},
      {c=123,a={b=2}},
      {a={b={1,2}}}
   )

   -- FIXME: Use equality asserts in this test


end

return { run = run }
