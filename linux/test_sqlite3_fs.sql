-- Use the sqlite3 binary from the same pacakge as the lib used in the build:
-- ./sqlite3-build.sh <./test_Sqlite3_ilog.sql
-- Run it on carpo which has that log file.


.load ./fs

CREATE VIRTUAL TABLE dir USING fs(.,2);

select rowid,name from dir;
select rowid,name from dir;

