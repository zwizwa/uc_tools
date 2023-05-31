.load ./logparse
CREATE VIRTUAL TABLE temp.lp USING logparse("dev1.bin");
.schema
SELECT id,val from temp.lp
