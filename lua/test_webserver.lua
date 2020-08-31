#!/usr/bin/lua
local uv = require('lluv')
local webserver = require('lib.webserver')
webserver.start(8000)
uv.signal():start(uv.SIGINT, function() uv.stop() end)
uv.run()
