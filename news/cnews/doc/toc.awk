#! /bin/sh
# turn raw index data into tbl input for a table of contents.
# record format: toc:page:title:section #:chapter|majorsection
exec awk '
BEGIN { FS=":"; sep="\t" }
$5 == "chapter"      {           print $4 sep $3 sep $2 }
$5 == "majorsection" { print ""; print $4 sep $3 sep $2 }
' $*
