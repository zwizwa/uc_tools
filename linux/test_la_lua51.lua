#!/usr/bin/lua
la = require("la_lua51")
la.start("/i/exo/logan/dev/saleae.sh")

--local signal = require("posix.signal")
--signal.signal(signal.SIGINT, function(signum)
--  io.write("SIGINT\n")
--end)

while true do
   la.status()
   la.send("asdf")
end

