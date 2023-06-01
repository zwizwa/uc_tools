.load ./logparse

-- CREATE VIRTUAL TABLE temp.lp USING logparse('test_logparse.trace');
-- CREATE VIRTUAL TABLE temp.lp USING logparse('/i/tom/rdm-bridge/uc_trace/all.20230531-151752.dev1.uc.trace');
CREATE VIRTUAL TABLE temp.lp USING logparse('/i/tom/rdm-bridge/uc_trace/console.20230418-131539.dev8.uc.trace'); -- large one: 86057254 byes 86M

CREATE VIRTUAL TABLE temp.lp1 USING logparse('/i/tom/rdm-bridge/uc_trace/console.20230414-173256.dev9.uc.trace'); -- similar size


.schema

-- SELECT * from temp.lp
-- SELECT * from temp.lp limit 100;

--SELECT count(*) from temp.lp; -- half a second on 86M,
--SELECT count(*) from temp.lp1;


-- Using a temporary table makes it maybe 3 times as fast.
--CREATE TEMPORARY TABLE temp_lp AS SELECT * FROM temp.lp;
--SELECT count(*) from temp_lp;
--SELECT count(*) from temp_lp;

SELECT broem(123);
