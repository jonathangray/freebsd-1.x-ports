# msg.tcl
#
# Print simple status messages from Tcl code
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

if {[catch "set MSGStr"]} {
  # Use didn't specify prompt.  Create one from app name
  set app [lindex $argv 0]
  if {$app != "" } {
     if {![regexp {.*/(.+)\.tcl} $app dummy MSGStr]} {
         set MSGStr $app
     }
  } else {
     set MSGStr "Msg"
  }
}

proc errmsg { str } {
    global MSGStr
    puts stderr "$MSGStr>  ERROR: $str"
}

proc msg { str } {
    global MSGStr
    puts stderr "$MSGStr>  $str"
}
