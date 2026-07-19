-- Use the sqlite3 binary from the same pacakge as the lib used in the build:
-- ./sqlite3-build.sh <./test_Sqlite3_ilog.sql
-- Run it on carpo which has that log file.


.load ./ilog

CREATE VIRTUAL TABLE packets USING ilog('/ssd/sigrok/20260610-120952/packets.ilog');

select count(*) from packets;

-- The index 1 is the message lookup by rowid.
-- QUERY PLAN
-- `--SCAN TABLE packets VIRTUAL TABLE INDEX 1:
explain query plan select rowid,tag,hex(bin) from packets where rowid = 32;


select rowid,tag,hex(bin) from packets where rowid = 32;

select rowid,tag,hex(bin) from packets where rowid = 15;


