#!./lua.sh
local rle = require('lib.rle')
require('lure.log')
log_desc(rle.compile({10,1}))

-- FIXME: Write some more tests for the multi-delay compilation.


