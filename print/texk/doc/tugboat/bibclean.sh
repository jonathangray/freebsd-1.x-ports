#!/bin/sh
# This seems to do an adequate job of reconstructing the TUG bbl.
wcat -i 17 -nq -w 72 | 
sed -e 's;=;= ;' \
    -e 's;AmS;AMS;' \
    -e 's;@pre;@Pre;' \
    -e 's;@str;@Str;' \
    -e 's;`\\\([^\]\);`\\\\\1;g' \
    -e '/pages/s;\([0-9][0-9]*\)\-\([0-9]*\);\1--\2;'
