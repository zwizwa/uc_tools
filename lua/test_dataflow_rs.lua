#!./lua.sh
local os = require('os')
os.execute("cd ../rs/dataflow ; cargo build")
require('lure.log')
local d = require('dataflow_rs')
log_desc({d=d})
