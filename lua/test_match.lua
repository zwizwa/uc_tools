#!/usr/bin/lua

local prompt = require('prompt')
local function log(str) io.stderr:write(str) end
local function log_desc(thing) log(prompt.describe(thing)) end

-- Pattern matching abstraction.

-- The basic idea behind implementing something resembling pattern
-- matching in a language that doesn't have it, is to replace it with
-- generatlized fold.  E.g. for each constructor (alternative) in a
-- sum type, there will be a separate function that handles it.  We
-- assume that all objects are tables.  That meshes better with Lua's
-- structure.

-- Here's an example for a list.  Note that 'nil' is a reserved word
-- so use pair, empty instead of cons, nil.

local example_pattern1 = {
   pair  = function(obj) log("pair\n")  end,
   empty = function(obj) log("empty\n") end
}

local function match1(data, pattern)
   -- 'type' is not a great name for the constructor field, as this
   -- conflates sum types (set of alternatives) with a single
   -- instance, so we rerve that for something else.  use 'case'
   -- instead.
   local case_fun = pattern[data.case]
   return case_fun(data)
end


local function test_match1()
   match1({case = 'empty'}, example_pattern1)
   match1({case = 'pair', car = 1, cdr = 2}, example_pattern)
end

test_match1()

-- So it's not really that different from:
-- if o.case == 'empty' then ... else ... end



-- Let's try a different approach, using arrays.  That allows to
-- actually use variable binding through unpack(), and arrays aren't
-- so inefficient either.

-- Constructor functions.
local function empty()   return {'empty'} end
local function pair(a,b) return {'pair',a,b} end

-- Deconstructor application
local function match2(data, pattern)
   local cons = data[1]
   local case_fun = pattern[cons]
   return case_fun(unpack(data,2))
end

-- Deconstructor cases
local example_pattern2 = {
   empty = function()    log("empty\n") end,
   pair  = function(a,b) log("pair " .. a .. "," .. b .. "\n")  end,
}

local function test_match2()
   match2(empty(),   example_pattern2)
   match2(pair(1,2), example_pattern2)

   match2(
      empty(), {
         empty = function()    return 0   end,
         pair  = function(a,b) return a+b end })

end

-- That's the one that goes into lib/match.lua


test_match2()



