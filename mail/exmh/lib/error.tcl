# error.tcl
#
# tkerror for exmh
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

#
# tkerror --
#	This is the handler for background errors that arise
#	from commands bound to keystrokes and menus.  A
#	toplevel message widget is used to display $errorInfo

proc tkerror { msg } {
    global errorInfo
    global exmh

    set font fixed
    set base ".errorInfo"
    set title "Error Info"
    if [info exists errorInfo] {
	set savedErrorInfo $errorInfo
    } else {
	set savedErrorInfo {no errorInfo}
    }
    # Create a toplevel to contain the error trace back
    if [catch {
	# Choose a unique name by testing for the associated error variable
	# Use the string ".errorInfo-N" as the name of the toplevel
	# and as the name of a variable holding the current errorInfo
	for {set x 1} {$x<10} {set x [expr $x+1]} {
	    global $base-$x
	    if {! [info exists $base-$x]} {
		break
	    }
	}
	global $base-$x ; set $base-$x $errorInfo
	set title $title-$x
	set name $base-$x

	set wx [expr ($x%20)*10]
	Widget_Toplevel $name Error $wx $wx

	wm title $name $title
    
	frame $name.buttons
	pack append $name $name.buttons {top fill expand}
    
	button $name.buttons.quit -text "Dismiss" -command [list destroy $name]
	pack append $name.buttons $name.buttons.quit {left}
	if [info exists exmh(maintainer)] {
	    button $name.buttons.mailto -text "Mail to $exmh(maintainer)" -command [list ExmhMailError $name $errorInfo]
	    pack append $name.buttons $name.buttons.mailto {right}
	}
	global widgetText TextType

	text $name.user -font $font -width 60 -bd 2 -relief raised
	$name.user configure -height 3
	$name.user insert end "User notes: "
	pack append $name $name.user {top expand}
	set widgetText($name.user,extend) 0
	set widgetText($name.user,geo) {}
	set TextType($name.user) text

	text $name.msg -font $font -width 60 -bd 2 -relief raised
	set numLines [llength [split $errorInfo \n]]
	if {$numLines > 20} {
	    set numLines 20
	}
	$name.msg configure -height $numLines
	$name.msg insert end $errorInfo
	pack append $name $name.msg {top expand}
	set widgetText($name.msg,extend) 0
	set widgetText($name.msg,geo) {}
	set TextType($name.msg) text

    } oops] {
	set msg [concat $msg "($name: " $oops ")" ]
   }

    if {[string length $msg] > 20} {
	set msg [concat [string range $msg 0 30] ...]
    }
    if [catch {Exmh_Status "tkerror: $msg" purple}] {
	puts stderr "tkrror: $msg"
	puts stderr "*** TCL Trace ***"
	puts stderr $savedErrorInfo
    }
}

proc ExmhMailError { w errInfo } {
    global exmh
    if [catch {open /tmp/exmhErrorMsg w} out] {
	Exmh_Status "Cannot open /tmp/exmhErrorMsg" purple
	return
    }
    if [catch {
	global env tk_version
	puts $out "To: $exmh(maintainer)"
	puts $out "Subject: exmh error"
	puts $out ""
	puts $out "Date [exec date]"
	if [info exists env(USER)] {
	    puts $out "$env(USER) got an error"
	}
	puts $out "Exmh version $exmh(version)"
	puts $out "TK version $tk_version"
	puts $out "TCL version [info tclversion]"
	catch {exec uname -a} uname
	puts $out "$uname"
	puts $out ""
	puts $out [$w.user get 1.0 end]
	puts $out ""
	puts $out $errInfo
	close $out
    } msg] {
	Exmh_Status "Cannot write /tmp/exmhErrorMsg :msg" purple
	return
    }
    if [catch {
	exec send /tmp/exmhErrorMsg
    } msg] {
	Exmh_Status "Send error: $msg" purple
	return
    } else {
	Exmh_Status "Mailed report to $exmh(maintainer)"
	destroy $w
    }
}

proc Exmh_Error { msg } {
    global errorInfo
    set errorInfo {}
    set level [info level]
    for {set l 0} {$l <= $level} {incr l} {
	append errorInfo [info level $l]
	append errorInfo \n
    }
    tkerror $msg
}
