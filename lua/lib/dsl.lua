-- Attempt to map the monadic DSL style to Lua.
--
-- The basic idea is to implement a "pure" DSL that represents a
-- dataflow network.  I've used two approaches for this in the past: a
-- Monadic DSL in Haskell, and an Erlang implementation with an effect
-- handler.  For Lua it seems the simplest approach is to duplicate
-- the effect handler approach on top of actor.lua tasks.

-- See erl_tools/src/dsl.erl

-- EDIT: This would work for dataflow language, but not for anything
-- that needs flow control. It's better to use a concrete syntax,
-- e.g. lisp, and then map that to an interpreter/compiler.

local dsl = {}

return dsl
