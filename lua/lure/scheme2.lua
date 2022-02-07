-- Variant of Scheme2
-- This is an interpreter for the block language.
-- Idea is to keep this as simple as possible.
--
-- Question is: should this be in Scheme or not?  First: what is the
-- purpose?  I need this to support the partial evaluation in SMC.
-- Eventually I would like to write the SMC in Scheme.  Also, it would
-- be a good test of the SLC to use it to write something non-trivial
-- as soon as possible.

local class = {}

-- The evaluator is a loop to properly handle tail calls and
-- continuations.
local function ins(name)
   return { class = 'instruction', ins = name }
end
local ret = ins("ret")
function class.eval_loop(s,expr)
   -- Stack contains continuations, which are represented as regular
   -- Lambda expressions.
   local stack = { ret }
   return "FIXME"
end


function class.eval(s,expr)
   return s:eval_loop(expr)
end

function class.new()
   local s = {}
   setmetatable(s, {__index = class})
   return s
end

return class
