# find.tcl
#
# Find tool.
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Find_Setup {} {
    global find

    set find(dir) forw
    set find(line) 1
    set find(lasthit) {}

    if [Exwin_Toplevel .find "Exmh Find Tool" Find] {
	set t .find
	set f $t.but	;# from Exwin_Toplevel

	Widget_AddBut $f next "Next" {Find_It forw} {left padx 1}
	Widget_AddBut $f prev "Prev" {Find_It prev} {left padx 1}
	set find(choice) FTOC
	Widget_RadioBut $f ftoc "FTOC" find(choice) {left padx 1}
	Widget_RadioBut $f msg "Msg" find(choice) {left padx 1}

	set f [Widget_Frame $t rim Rim]
	$f configure -bd 10
	set f [Widget_Frame $f rim Rim]
	$f configure -bd 2 -relief raised
	Widget_Label $f label {left fill} -text "Pattern: "
	set find(entry) [Widget_Entry $f entry {right fill expand} -bg white]
	Bindings_Search $find(entry)
	focus $find(entry)
    } else {
	# window already exists
	Find_It forw
    }
}
proc FindDestroy {} {
    global find
    set find(geometry) [wm geometry .find]
    wm withdraw .find
    Exmh_Focus
}
proc Find_It { {dir _default_} } {
    global find
    if ![info exists find(entry)] {
	Find_Setup
	return
    }
    if [catch {$find(entry) configure}] {
	unset find(entry)
	Find_Setup
	return
    }
    if {$dir == "_default_"} {
	set find(line) 1
	set find(lasthit) {}
	catch {unset find(curline)}
	set dir forw
    }
    set find(dir) $dir
    if {$find(choice) == "FTOC"} {
	global ftoc
	Find_Inner [$find(entry) get] $dir $ftoc(curLine) $ftoc(numMsgs) Ftoc_FindMatch
	return
    }
    if {$find(choice) == "Msg"} {
	global exwin
	Find_Inner [$find(entry) get] $dir $find(line) [lindex [split [$exwin(mtext) index end] .] 0] Msg_FindMatch
	return
    }
}
proc Find_Inner { string dir start max matchProc {feedback yes} } {
    global exwin find
    set verbose [expr {$feedback == "yes"}]
    if {$string == {}} {
	if {$verbose} {Exmh_Status "No search string" red}
	return -1
    }
    set find(line) $start
    if {$find(line) == {}} {
	set find(line) 1
    }
    if [info exists find(curline)] {
	set L $find(curline)
    } else {
	set L $find(line)
    }
    Exmh_Debug "Find_Inner line $L"
    if {$dir == "forw"} {
	set wrap [expr $L==$max]
	for { } 1 {incr L} {
	    set match [FindMatch $matchProc $L $string]
	    if {$match < 0} {
		break
	    }
	    set wrap 1
	    if {$match} {
		return 1
	    }
	}
	if {$wrap} {
	    if {$verbose} {Exmh_Status "Find miss: <Control-s> to wrap" blue}
	    set find(curline) $L
	    return 0
	}
	for {set L 0} {$L < $find(line)} {incr L} {
	    if {[FindMatch $matchProc $L $string] == 1} {
		return 1
	    }
	}
    } else {
	set wrap [expr $L==1]
	for { } {$L >= 1} {incr L -1} {
	    set match [FindMatch $matchProc $L $string]
	    if {$match < 0} {
		break
	    }
	    set wrap 1
	    if {$match} {
		return 1
	    }
	}
	if {$wrap} {
	    if {$verbose} {Exmh_Status "Find miss: <Control-r> to wrap" blue}
	    set find(curline) $max
	    return 0
	}
	for {set L $max} {$L > $find(line)} {incr L -1} {
	    if [FindMatch $matchProc $L $string] {
		return 1
	    }
	}
    }
    catch {unset find(curline)}
    if {$verbose} {Exmh_Status "No match" red}
    return -1
}
proc FindMatch { hook L string } {
    global find
    set match [$hook $L $string]
    if {$match == 1} {
	set find(line) $L
	catch {unset find(curline)}
	# HACK
	if {! [string match Sedit* $hook]} {
	    Exmh_Focus
	    Exmh_Status "Find hit: <Control-s> next, <Control-r> prev" blue
	}
    }
    return $match
}
proc FindTextMatch {t L string} {
    global find
    if [catch {$t get $L.0 $L.end} text] {
	return -1
    }
    if {$L == $find(lasthit)} {
	# Look for more strings on the same line
	# This behaves wrong during Previous searches...
	set text [string range $text $find(lastchar2) end]
    } else {
	set find(lastchar2) 0
    }
    if [regexp -nocase -indices $string $text match] {
	global msg
	set range [$t tag ranges sel]
	if {$range != {}} {
	    eval {$t tag remove sel} $range
	}
	set char1 [expr $find(lastchar2)+[lindex $match 0]]
	set char2 [expr $find(lastchar2)+[lindex $match 1]+1]
	$t tag add sel $L.$char1 $L.$char2
	$t tag raise sel
	WidgetTextYview $t -pickplace $L.$char1
	set find(lasthit) $L
	set find(lastchar2) $char2
	return 1
    }
    return 0
}

