.load ./logparse

-- CREATE VIRTUAL TABLE temp.lp USING logparse('/i/tom/rdm-bridge/uc_trace/all.20230531-151752.dev1.uc.trace');

CREATE VIRTUAL TABLE temp.lp USING logparse('test_logparse.trace');

.schema
SELECT id,val from temp.lp
