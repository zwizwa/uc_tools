#!/usr/bin/lua

-- Note: this was an old attempt.
-- See test_se_match.lua and test_hoas_match.lua for better versions.

local prompt = require('prompt')
local function log(str) io.stderr:write(str) end
local function log_desc(thing) log(prompt.describe(thing)) end

-- Pattern matching / algebraic types abstraction.

-- The basic idea behind implementing something resembling pattern
-- matching in a language that doesn't have it, is to use a
-- generalized fold.  E.g. for each constructor (alternative, case) in
-- a sum type, there will be a separate function that handles it in a
-- pattern matching application.  The match clauses can then be
-- represented in a dictionary of functions.

-- After trying once using table, it is apparent that arrays are a
-- much better abstraction.  That allows to use function argument
-- binding through unpack(), and is supposed to be more efficient.

-- An example.

-- Constructor functions.
local function empty()   return {'empty'} end
local function pair(a,b) return {'pair',a,b} end

-- Deconstructor application
local function match(data, pattern)
   local cons = data[1]
   local case_fun = pattern[cons]
   return case_fun(unpack(data,2))
end

-- Deconstructor cases
local example_pattern = {
   empty = function()    log("empty\n") end,
   pair  = function(a,b) log("pair " .. a .. "," .. b .. "\n")  end,
}

local function test_match()
   -- Functional test
   match(empty(),   example_pattern)
   match(pair(1,2), example_pattern)

   -- This is what a typical use would look like.
   local var = empty()
   match(
      var, {
         empty = function()
            return 0
         end,
         pair = function(a,b)
            return a + b
         end
      })

end

test_match()





