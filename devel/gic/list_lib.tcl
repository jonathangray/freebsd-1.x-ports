#
#  (c) Copyright 1992 Department of Computer Science, University of
#      Calgary, Calgary, Alberta, Canada.  All rights reserved.
#    
#  Permission to use, copy, modify, and distribute this software and its
#  documentation for any purpose and without fee is hereby granted, provided
#  that the above copyright notice appears in all copies.  The University
#  of Calgary makes no representations about the suitability of this
#  software for any purpose.  It is provided "as is" without express or
#  implied warranty.
#

# Written by David Marwood

proc Insert {ListName Item} {
    upvar 1 $ListName List
    
    lappend List $Item
}


proc Remove {ListName Item} {
    upvar 1 $ListName List
    
#    puts stdout "Remove: removing $Item from $List"
    set Pos [lsearch $List $Item]
#    puts stdout "Remove: item pos is $Pos"
    Assert "$Pos >= 0"
    set List [lreplace $List $Pos $Pos]
#    puts stdout "Remove: new list is $List"
}


proc Reverse List {
    set Ans {}

    while {[llength $List] > 0} {
        set Item [lindex $List [expr [llength $List]-1]]
        Insert Ans $Item
        Remove List $Item
#       puts stdout "Reverse: moved $Item from $List to $Ans."
    }
    return $Ans
}


proc Assert Cond {
    if "$Cond" {
    } else {
	tkerror "Assertion failed"
    }	
}
