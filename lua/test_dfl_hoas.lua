#!./lua.sh

-- Higher order syntax for a simple dataflow language
--
-- Design simplifications:
--
-- 1. Types are not specified explicitly, but inferred by abstract
--    interpretation of the inputs.  There is no polymorphism: all
--    polymorphic construct are implemented as macros that generate
--    monomorphic language constructs.
--
-- 2. The binding environment (i.e. library functions) is always
--    explicit, and no currying is used to keep the Lua notation
--    simple.  The Lua 'self' construct can be used, but in the code
--    below we use 'e' as shorthand.  The flat syntax hopefully makes
--    it easier to learn: sneak in parameterized semantics for people
--    focused on the "language is syntax" idea.
--
-- 3. Functions are multi in, multi out.

local m = {}

function m.accu(e, s, i)
   local s1 = e:add(s, i)
   return s1, s1
end

-- Semantics: combined type inference and checking.
local e_type = {}
function e_type.assert_eq(e, a, b)
   assert(a == b)
end
function e_type.add(e, a, b)
   e:assert_eq(a, b)
   return a
end

-- Semantics: Lua evaluation
local e_eval = {}
function e_eval.add(e, a, b)
   return a + b
end


require 'lure.log'
function test()
   local out_type = {m.accu(e_type, "int", "int")}
   local out_eval = {m.accu(e_eval,  1,     2)}
   log_desc({out_type=out_type, out_eval=out_eval})
end

test()
