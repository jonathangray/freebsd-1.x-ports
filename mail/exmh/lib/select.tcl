# select.tcl
#
# Keystroke bindings to select for message or folder
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

# Select_Bindings creates bindings on the message and ftoc windows
# that in turn warp focus to the Status window for folder & message selection
proc Select_Bindings { w } {
    global select
    set select(sel) {}
    set select(prompt) {}
    foreach digit {0 1 2 3 4 5 6 7 8 9} {
	bind $w <Key-$digit> {SelectDigit %W %A}
    }
    bind $w <Key-plus> {SelectPlus %W}
    set select(toggle) { Change Target }
}
proc SelectPlus { w } {
    global select
    $select(entry) configure -state normal
    focus $select(entry)
    set select(sel) {}
    set select(prompt) "[lindex $select(toggle) 0] Folder:"
    set select(folder) 1
    Exmh_Status "$select(prompt) "
}
proc SelectDigit {w digit} {
    global select
    $select(entry) configure -state normal
    focus $select(entry)
    set select(sel) $digit
    set select(prompt) Msg:
    catch {unset select(folder)}
    Exmh_Status "Select Msg: $select(sel)"
    Msg_Change $select(sel) noshow
}

# These are the bindings on the status entry widget
proc Select_EntryBind { w } {
    global select
    set select(entry) $w
    bind $w <Any-Key>	{SelectTypein %W %A}
    bind $w <Key-plus>	{SelectToggle %W }
    bind $w <space>	{SelectComplete %W}
    bind $w <Return>	{SelectReturn %W}
    bind $w <Control-h>	{SelectBackSpace %W}
    bind $w <Delete>	{SelectBackSpace %W}
    bind $w <Control-c>	{SelectCancel %W}
    bind $w <Control-g>	{SelectCancel %W}
    bind $w <Control-u>	{SelectClear %W}
}
proc SelectTypein {w {a {}}} {
    global select
    if {$a == {}} {
	return
    }
    if [info exists select(match)] {
	set select(sel) $select(match)
	unset select(match)
	catch {unset select(allfolders)}
    }
    append select(sel) $a
    Exmh_Status "$select(prompt) $select(sel)"
    if ![info exists select(folder)] {
	Msg_Change $select(sel) noshow
    }
}
proc SelectBackSpace { w } {
    global select
    if [info exists select(match)] {
	set select(sel) $select(match)
	unset select(match)
	catch {unset select(allfolders)}
    }
    set select(sel) [string range $select(sel) 0 [expr [string length $select(sel)]-2]]
    Exmh_Status "$select(prompt) $select(sel)"
    if ![info exists select(folder)] {
	Msg_Change $select(sel) noshow
    }
}
proc SelectToggle {w} {
    global select
    if [info exists select(folder)] {
	set select(toggle) [list [lindex $select(toggle) 1] [lindex $select(toggle) 0]]
	set select(prompt) "[lindex $select(toggle) 0] Folder:"
    } else {
	catch {
	    incr select(sel)
	    Msg_Change $select(sel) noshow
	}
    }
    Exmh_Status "$select(prompt) $select(sel)"
}
proc SelectComplete { w } {
    global select
    if [info exists select(folder)] {
	global flist
	if ![info exists select(allfolders)] {
	    set select(allfolders) $flist(allfolders)
	}
	set ix 0
	foreach f $select(allfolders) {
	    incr ix
	    if [string match $select(sel)* $f] {
		set select(allfolders) [lrange $select(allfolders) $ix end]
		if {$select(allfolders) == {}} {
		    unset select(allfolders)
		}
		set select(match) $f
		Exmh_Status "$select(prompt) $f"
		return
	    }
	}
	unset select(allfolders)
	catch {unset select(match)}
	Exmh_Status "$select(prompt) $select(sel)"
    }
}
proc SelectReturn { w } {
    global select
    if [info exists select(folder)] {
	if [info exists select(match)] {
	    set select(sel) $select(match)
	    unset select(match)
	}
	if [string length $select(sel)] {
	    Folder_[lindex $select(toggle) 0] $select(sel)
	}
	unset select(folder)
	catch {unset select(allfolders)}
    } else {
	Msg_Change $select(sel) show
    }
    $select(entry) configure -state disabled
    Exmh_Focus
}
proc SelectCancel { w } {
    global select
    if [info exists select(folder)] {
	set select(toggle) { Change Target }
	unset select(folder)
    }
    $select(entry) configure -state disabled
    Exmh_Focus
}
proc SelectClear { w } {
    global select
    set select(sel) {}
    Exmh_Status "$select(prompt) $select(sel)"
}
