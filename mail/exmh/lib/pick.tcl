# pick.tcl
#
# Interface to MH pick functionality
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Pick {} {
    global pick
    if [Exwin_Toplevel .pick "Pick Messages" Pick] {
	set t .pick
	set f .pick.but

	.pick.but.quit configure -command {Exwin_Dismiss .pick nosize}
	Widget_AddBut $f clear "Clear" { PickClear }
	Widget_AddBut $f mark "Mark Seen" {PickMarkSeen}
	Widget_AddBut $f pick "Pick" {Pick_It} {left padx 1}
	set pick(addtosel) 0
	Widget_CheckBut $f add "Add to Sel" pick(addtosel) {left padx 1}
	set pick(project) 0
	Widget_CheckBut $f project "New FTOC" pick(project) {left padx 1}
    
	set pick(panes) 0
	PickButtons
    }
}
proc PickButtons {} {
    global pick

    incr pick(panes)
    set f [Widget_Frame .pick rim$pick(panes) Rim]

    $f configure -bd 10

    PickEntry $f subject Subject
    PickEntry $f from From
    PickEntry $f to To
    PickEntry $f cc Cc
    PickEntry $f before Before
    PickEntry $f after After
    PickEntry $f search Search
    set pad [Widget_Frame $f pad Pad]
    $pad configure -height 10 -width 10
    PickEntry $f msgs Messages

    Widget_AddBut $f or "Or" { PickOr }
}
proc PickOr {} {
    global pick
    global tk_version

    if {$tk_version >= 3.3} {
	pack forget .pick.rim$pick(panes).or
    } else {
	pack unpack .pick.rim$pick(panes).or
    }
    after 1 "destroy .pick.rim$pick(panes).or"
    Widget_Label .pick.rim$pick(panes) label {fill} -text "- Or -"

    PickButtons
}
proc PickEntry { frame name label } {
    global pick
    set f1 [Widget_Frame $frame $name $label]
    $f1 configure -bd 2 -relief raised
    Widget_Label $f1 label {left fill} -text ${label}:
    set pick(entry,$pick(panes),$name) [Widget_Entry $f1 entry {left expand fill}]
    bind $pick(entry,$pick(panes),$name) <Return> {Pick_It}
}

proc PickClear {} {
    global pick
    for {set pane 1} {$pane <= $pick(panes)} {incr pane} {
	destroy .pick.rim$pane
    }
    set pick(panes) 0
    PickButtons
}
proc Pick_It {} {
    global pick exmh
    set cmd [list exec pick +$exmh(folder) -list]
    set inpane 0
    set hadpane 0
    set msgs {}
    for {set pane 1} {$pane <= $pick(panes)} {incr pane} {
	set and 0
	foreach field {subject from to cc before after search} {
	    set text [$pick(entry,$pane,$field) get]
	    if {$text != {}} {
		if {$inpane != $pane} {
		    if {$hadpane} {
			lappend cmd -or
		    }
		    lappend cmd -lbrace
		    set inpane $pane
		    set hadpane 1
		}
	        if {$and} {
		    lappend cmd -and
	        }
		lappend cmd -${field} $text
	        set and 1
	    }
	}
	set m [$pick(entry,$pane,msgs) get]
	if {$m != {}} {
	    lappend msgs $m
	}
	if {$inpane == $pane} {
	    lappend cmd -rbrace
	}
    }
    Exmh_Debug Pick_It $cmd $msgs
    busy PickInner $cmd $msgs
    Exmh_Focus
}
proc PickInner {cmd msgs} {
    global pick
    Exmh_Status "$cmd $msgs" red
    if [catch [concat $cmd $msgs] ids] {
	Exmh_Status "Fail: $cmd $msgs" purple
	return
    }
    set pick(ids) [split $ids \n]
    Exmh_Debug Ftoc_PickMsgs $pick(ids)
    if {! $pick(addtosel)} {
	Ftoc_RangeUnHighlight
    }
    if {$pick(project)} {
	if {[Ftoc_Changes pick] == 0} {
	    Scan_ProjectSelection $pick(ids)
	}
    } else {
	Ftoc_PickMsgs $pick(ids)
    }
    Exmh_Status "Pick hit [llength $ids] msgs" blue
}
proc PickMarkSeen {} {
    global exmh pick
    if ![info exists pick(ids)] {
	return
    }
    Mh_MarkSeen $exmh(folder) $pick(ids)
    Ftoc_MarkSeen $pick(ids)
    foreach id $pick(ids) {
	Flist_MsgSeen $id
    }
}
proc Pick_MarkSeen {} {
    global exmh pick
    Exmh_Status "Clearing unseen sequence..." red
    set pick(ids) [Mh_Unseen $exmh(folder)]
    busy PickMarkSeen
    Exmh_Status ok blue
}
