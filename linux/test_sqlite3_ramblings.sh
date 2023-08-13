#!/bin/sh
cat <<EOF | sqlite3
.load ./ramblings
CREATE VIRTUAL TABLE r USING ramblings("/i/exo/exo.txt");
select count(*) from r;
select date,title from r;
EOF
