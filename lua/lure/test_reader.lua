require ('lure.log')
local se = require('lure.se')
local function run()
   local function check(str)
      log_desc(se.read_string(str))
   end
   check("((a . b) (c . d))")
end

return { run = run }
