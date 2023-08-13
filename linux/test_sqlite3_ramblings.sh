#!/bin/sh
cat <<EOF | sqlite3
.load ./ramblings
CREATE VIRTUAL TABLE r USING ramblings("/i/exo/exo.txt");
select * from r;
EOF
