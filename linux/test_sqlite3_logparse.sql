.load ./logparse

-- tom@carpo:/i/tom/rdm-bridge/uc_tools/linux$ /i/tom/rdm-bridge/tools/sqlite3.sh < test_sqlite3_logparse.sql

CREATE VIRTUAL TABLE lp0 USING logparse(/ssd/enc/c8-logs/new/20260717-060731/t2/usb.002.bin);

select ts,line from lp0;

.quit





-- OLD BITROTTED -- TO PORT

CREATE VIRTUAL TABLE temp.lp USING logparse('test_logparse.trace');

-- CREATE VIRTUAL TABLE temp.lp USING logparse('/i/tom/rdm-bridge/uc_trace/all.20230531-151752.dev1.uc.trace');
CREATE VIRTUAL TABLE temp.lp0 USING logparse('/i/tom/rdm-bridge/uc_trace/console.20230418-131539.dev8.uc.trace'); -- large one: 86057254 byes 86M

CREATE VIRTUAL TABLE temp.lp1 USING logparse('/i/tom/rdm-bridge/uc_trace/console.20230414-173256.dev9.uc.trace'); -- similar size


.schema

-- SELECT * from temp.lp
-- SELECT * from temp.lp limit 100;

SELECT * from temp.lp;

SELECT count(*) from temp.lp0; -- half a second on 86M,
SELECT count(*) from temp.lp1;


-- Using a temporary table makes it maybe 3 times as fast.
--CREATE TEMPORARY TABLE temp_lp AS SELECT * FROM temp.lp;
--SELECT count(*) from temp_lp;
--SELECT count(*) from temp_lp;

-- example C function
SELECT inc(123);
