# 
# sound.tcl
#
# Audio feedback
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Sound_Off {} {
    global sound
    set oldValue $sound(enabled)
    set sound(enabled) 0
    return $oldValue
}
proc Sound_On {} {
    global sound
    if {$sound(cmd) != {}} {
	set sound(enabled) 1
    }
}
proc Sound_Feedback { delta } {
    global sound exmh
    if {!$sound(enabled) || ($sound(cmd) == {})} {
	return
    }
    set num 0
    while {$delta > 0} {
	incr num
	set delta [expr $delta/4]
    }
    set cmd [concat exec $sound(cmd)]
    if {$sound(multifile)} {
	for {set i 0} {$i < $num} {incr i} {
	    lappend cmd $sound(newMsg)
	}
	if [catch {
	    eval $cmd &
	} err] {
	    Exmh_Debug "Sound_Feedback: $err"
	}
    } else {
	for {set i 0} {$i < $num} {incr i} {
	    if [catch {
		eval $cmd $sound(newMsg)
	    } err] {
		Exmh_Debug "Sound_Feedback: $err"
	    }
	}
    }
}

proc Sound_Error {  } {
    global sound
    if {!$sound(enabled) || ($sound(cmd) == {})} {
	return
    }
    if [catch {
	eval exec $sound(cmd) $sound(error)
    } err] {
	Exmh_Debug "Sound_Error: $err"
    }
}


