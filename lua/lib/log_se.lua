local se = require('lib.se')
local iolist = require('lib.iolist')
require('lib.log')
function log_w(...)      iolist.write(log, {...}) end
function log_se(e)       log_w(se.iolist(e)) end
function log_se_n(e,tag) if(tag) then log(tag) end ; log_se(e) ; log('\n') end


