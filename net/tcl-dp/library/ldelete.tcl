# ldelete.tcl --
#
# This file contains a list element deletion utility procedure.
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
# ldelete 	- search a list for an item and delete the first one found;
#		- returns new list;
#
# 	example: ldelete {a b c a} a ==> b c a
#

proc ldelete {list elt} {
  set index [lsearch $list $elt];
  if {$index >= 0} {
    return [lreplace $list $index $index];
  }
  return $list;
}


