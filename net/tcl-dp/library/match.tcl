# match.tcl --
#
# This file contains string matching utilites.
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
# string matching (comparing) utilities;
#

###########################################################################

proc matchExact {x y} {
  if {[string compare $x $y]} {
    return 0;
  }
  return 1;
}

proc matchNoCaseExact {x y} {
  return [matchExact [string tolower $x] [string tolower $y]];
}
    
proc matchPattern {x y} {
  if {[string match $x $y]} {
    return 1;
  }
  return 0;
}

proc matchNoCasePattern {x y} {
  return [matchPattern [string tolower $x] [string tolower $y]];
}

###########################################################################
    
proc match {x y} {
  if [matchExact         $x $y] {return 1};
  if [matchNoCaseExact   $x $y] {return 1};
  if [matchPattern       $x $y] {return 1};
  if [matchNoCasePattern $x $y] {return 1};
  return 0;
}

