-- Apparently is possible to turn a constructor into a destructor.

require('lure.log')

local match = require('lure.match')
local smatch = require('lure.smatch')

local function test0()
   -- 1. Represent the pattern as a constructor parameterized by a table.
   local pattern_syntax =
      function(m)
         return {'add', {'sub', m.a, m.b}, m.c}
      end

   -- 2. Compile the constructor into a data structure that can be used
   --    for matching.
   local pattern_obj = match.compile(pattern_syntax)
   log_desc({pattern_obj = pattern_obj})

   -- 3. Perform the match.
   local expr = {'add',{'sub',300,200},100}
   local match_result = match.apply(pattern_obj, expr)
   log_desc({expr = expr, match_result = match_result})
end


-- Usage examples.  These use the se.lua library, which is array-based
-- so works with the current array index matching.
local se = require('lure.se')
local l = se.list

local function test1()
   local function pat(_)
      return l('add',l('sub',_.a,_.b),_.c)
   end
   local env = {a = 1, b = 2, c = 3}
   -- use constructor to construct
   local expr = pat(env)
   -- use constroctor to create destructor
   local cpat = match.compile(pat)
   -- apply destructor to get original env == match
   local match = match.apply(cpat, expr)
   log_desc(match)
end

local function test_match(do_match)
   return function (expr)
      log("TEST:")
      log_desc({expr, do_match(expr)})
   end
end
local function test_expr(do_match)
   local _ = test_match(do_match)
   _('a')
   _(l('add',100,200))
   _(l('add',l('sub',100,200),300))
end

local function test2()
   -- Non-sugared matching.
   -- Lua's lack of macros makes this is a little awkward.
   local function do_match(expr)
      return match.match(
         expr,
         {
            {function(_) return l('add',l('sub',_.a,_.b),_.c) end,
               function(m) return {a = m.a} end},
            {function(_) return l('add',_.a,_.b) end,
                  function(m) return {a = m.a} end},
         })
   end
   test_expr(do_match)
end

local function trace(tag, thing)
   log_desc({trace = {tag, thing}})
end
local ins = table.insert


local function test3()
   -- Sugared matching using a "string DSL"

   -- The syntax inside the strings is a Lua function body
   -- parameterized by a single object, which we configure to be '_'
   -- here.  Additional symbols can be passed in via env, e.g. the
   -- list constructor l.

   local mtch = smatch.smatcher(
      { env = {l = se.list},
        var = '_' }
   )
   local function do_match(expr)
      return mtch(
         expr,
         {
            {"l('add',l('sub',_.a,_.b),_.c)", "{a = _.a}"},
            {"l('add',_.a,_.b)",              "{b = _.b}"},
         })
   end
   test_expr(do_match)
end

local function run(w)
-- test0()
-- test1()
test2()
-- test3()
end

return { run = run }


