local prompt = require('prompt')
local se     = require('lib.se')
local mod = {}
function log(str)
   io.stderr:write(str)
end
function log_w(thing)
   if type(thing) == 'table' then
      for _,thing1 in ipairs(thing) do log_w(thing1) end
   else
      log(thing .. "")
   end
end
function log_desc(thing)
   log(prompt.describe(thing))
   log("\n")
end
function log_se(thing)
   log_w(se.iolist(thing))
end