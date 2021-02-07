-- Attempt to map the monadic DSL style to Lua.
--
-- The basic idea is to implement a "pure" DSL that represents a
-- dataflow network.  I've used two approaches for this in the past: a
-- Monadic DSL in Haskell, and an Erlang implementation with an effect
-- handler.  For Lua it seems the simplest approach is to duplicate
-- the effect handler approach on top of actor.lua tasks.

-- See erl_tools/src/dsl.erl

local dsl = {}
return dsl
