#! /bin/sh
/usr/local/bin/faxmail | /usr/local/bin/sendfax -n -d "$1@$2" -f "$3"
