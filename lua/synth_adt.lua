-- An example is the control protocol for a synthesizer.

-- This is an example of a regular type: all have the same

-- It is important to distinguish between types (strictly compile time
-- entities) and data (entities such as initializers that are used at
-- compile time and run time).
local meta = {struct = {}, init = {}}


-- The main usability constraint is to allow low level code to use C
-- structs that do not feel convoluted.  So we stick as close as
-- possible to constructs that map directly to a C struct or union,
-- with an eye on a Rust mapping as well. The order of the fields is
-- important, so we encode it as Lua arrays.

-- The microcontroller needs two things: a struct containing the
-- parameter values, and a struct with metadata, necessary for message
-- dispatch, user interface and human-readable log messages.
meta.struct.param = {
   -- name     type     storage type     comment
   { "value", "u32",    "var",           "Parametar value"},
}

-- Note that the main trick in this approach is to realize that
-- metadata can be partially compiled, but will not be eliminated
-- completely.  E.g. to generate C structs, our Lua code generator
-- will need metadata.  But to perform dispatch, the microcontroller
-- will also need some metadata.  These two forms of metadata are not
-- the same.  The microcontroller needs "less".  E.g. the metadata
-- needed by the uC is a projection of what the Lua generator needs.
-- The main difficulty in designing an approach like this is to figure
-- out the phase transitions between different compilation stages, and
-- what type information is moved ("reified") from compile time to run
-- time.  This "mixing" can be confusing.  An example here is that
-- name can be represented both as a string, and a variable name.  The
-- convention we use is that the first entry in a meta description is
-- always the name.

meta.struct.param_meta = {
   -- name     type     storage type     comment
   { "tag",   "string"  "const"          "Parameter tag"},
   { "index", "u32"     "const",         "Parameter index"},
   { "desc",  "string", "const",         "Parameter description"},
}



-- Initializers for structs.
meta.init.param = {
   {   "tag",          "index", "unit",  "desc"},{
      {"vco_setpoint",  0,      "u5.27", "Main oscillator setpoint, in 5.27 nlog2 (period_cycles)"},
      {"vcf_setpoint",  1,      "u5.27"
      
      
}

return typ
