#!/usr/bin/lua
local uv = require('lluv')
local websocket = require('lib.websocket')
websocket.start(8000)
uv.signal():start(uv.SIGINT, function() uv.stop() end)
uv.run()
