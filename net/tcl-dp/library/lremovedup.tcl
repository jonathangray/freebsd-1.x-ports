# lremovedup.tcl --
#
# This file contains a list remove duplicate elements utility procedure.
#
# Copyright 1992 Regents of the University of California
# Permission to use, copy, modify, and distribute this
# software and its documentation for any purpose and without
# fee is hereby granted, provided that this copyright
# notice appears in all copies.  The University of California
# makes no representations about the suitability of this
# software for any purpose.  It is provided "as is" without
# express or implied warranty.
#

#
# return a list with duplicates removed
#

proc lremovedup {l} {
  set l [lsort $l];
  set s [llength $l];
  set r {};
  for {set i 0} {$i < $s} {} {
    set first [lindex $l $i];
    incr i;
    while {[string compare $first [lindex $l $i]] == 0} {
      incr i;
    }
    lappend r $first;
  }
  return $r;
}

