#!./lua.sh

-- Tested with:
-- LuaJIT 2.0.4 -- Copyright (C) 2005-2015 Mike Pall. http://luajit.org/

-- The goal here is to:

-- 1. Model protocol messages abstractly, as a type described in a Lua
--    data structure, to be used for code generation.  It should be
--    done in a way that is easy to convert to other representations
--    later (e.g. Erlang).
--
-- 2. Standardize on a C / C preprocessor representation for the type,
--    and generate it from the Lua representation.  Keep in mind a
--    conversion to Rust ADTs.
--
-- 3. Generate run time C support code, e.g. a serializer and a
--    deserializer based on a bump allocator.
--
-- 4. Generate a LuaJIT wrapper based on the C representation.
--


-- To test this, do it in two steps:
-- . just a struct
-- . a tagged union

synth_adt = require('synth_adt')
