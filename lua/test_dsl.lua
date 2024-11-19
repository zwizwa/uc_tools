#!/usr/bin/env lua
local dsl = require('lib.dsl')

-- The 's' parameter is semantics.
local prog = function(s)
   s.recv(
      function(value)
         s.send(s.add(value, 1))
   end)
end



