#!/usr/bin/awk -f
# SYNTAX
# 	discard gt|lt num [file...]
# DESCRIPTION
# 	The `discard' program discards lines whose lengths meet a 
# 	threshold criterium, as specified on the command line.
#	If the first argument is `lt', then lines whose lengths are
#	less than `num' are discarded.  Similarly, a first argument of
#	`gt' discards lines longer than `num'.
# AUTHOR
# 	Andrew Moore (alm@netcom.com) -- for public domain

BEGIN {	if (ARGV[1] !~ /^l/ && ARGV[1] !~ /^g/ || ARGV[2] !~ /^[0-9]+$/) {
		printf "usage: discard gt|lt num [file...]\n"
		exit 1
	}
	name = ARGV[1] ""
	thresh = ARGV[2] + 0
	ARGV[1] = ARGV[2] = ""
      }

      name ~ /^l/ && length($0) >= thresh
      name ~ /^g/ && length($0) <= thresh
