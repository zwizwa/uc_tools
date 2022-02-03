local se = require('lure.se')
local iolist = require('lure.iolist')
require('lure.log')
function log_w(...)      iolist.write(log, {...}) end
function log_se(e)       log_w(se.iolist(e)) end
function log_se_n(e,tag) if(tag) then log(tag) end ; log_se(e) ; log('\n') end


