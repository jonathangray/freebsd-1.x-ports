# textSelect.tcl
#
# Text selection support.
# Borrowed from Jupitor code written by Dave Nichols.
#
# This is imported by the widgetText routines.
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

#
# Selections: making, claiming, and handling requests for.
#
proc Text_HandleSelRequest { w offset maxBytes } {
    global tk_priv
    return [string range $tk_priv(lastsel) $offset [expr {$offset+$maxBytes}]]
}

proc Text_LoseSelection { w } {
    $w tag remove sel 0.0 end
}

proc Text_SelectTo {w index} {
    global tk_priv

     if [catch {$w index anchor}] {
	return
     }
    case $tk_priv(selectMode) {
	char {
	    if [$w compare $index == anchor] {
		set first $index
		set last $index
	    } elseif [$w compare $index < anchor] {
		set first $index
		set last anchor
	    } else {
		set first anchor
		set last [$w index $index]
	    }
	}
	word {
	    if [$w compare $index < anchor] {
		set first [$w index "$index wordstart"]
		set last [$w index "anchor wordend"]
	    } else {
		set first [$w index "anchor wordstart"]
		set last [$w index "$index wordend"]
	    }
	}
	line {
	    if [$w compare $index < anchor] {
		set first [$w index "$index linestart"]
		set last [$w index "anchor lineend + 1c"]
	    } else {
		set first [$w index "anchor linestart"]
		set last [$w index "$index lineend + 1c"]
	    }
	}
    }
    $w tag remove sel 0.0 $first
    $w tag add sel $first $last
    $w tag remove sel $last end
    $w tag raise sel
}

# Called when we're done doing a selection.
proc Text_SelectionEnd { w rotate } {
    global tk_priv
    set sel ""
    if {[catch {set sel [$w get sel.first sel.last]}]} {
	return
    }
    set tk_priv(lastsel) $sel
    selection own $w "Text_LoseSelection $w"
    if {$rotate} { cutbuffer rotate 1 }
    cutbuffer set 0 $sel
}

# The procedure below compares three indices, a, b, and c.  Index b must
# be less than c.  The procedure returns 1 if a is closer to b than to c,
# and 0 otherwise.  The "w" argument is the name of the text widget in
# which to do the comparison.
proc Text_IndexCloser {w a b c} {
    set a [$w index $a]
    set b [$w index $b]
    set c [$w index $c]
    if [$w compare $a <= $b] {
	return 1
    }
    if [$w compare $a >= $c] {
	return 0
    }
    scan $a "%d.%d" lineA chA
    scan $b "%d.%d" lineB chB
    scan $c "%d.%d" lineC chC
    if {$chC == 0} {
	incr lineC -1
	set chC [string length [$w get $lineC.0 $lineC.end]]
    }
    if {$lineB != $lineC} {
	return [expr {($lineA-$lineB) < ($lineC-$lineA)}]
    }
    return [expr {($chA-$chB) < ($chC-$chA)}]
}

# Start extending a selection.  Chooses the end farthest from the mouse hit.
proc Text_StartExtend { w index } {
    global tk_priv
    set tk_priv(delstate) {}
    if {[$w tag ranges sel] == ""} {
	set tk_priv(selectMode) char
	$w mark set anchor insert
    } else {
	if {[Text_IndexCloser $w $index sel.first sel.last]} {
	    $w mark set anchor sel.last
	} else {
	    $w mark set anchor sel.first
	}
	$w mark set insert anchor
    }
    Text_SelectTo $w $index
    if {[lindex [$w config -state] 4] == "normal"} {focus $w}
}

# Return the current selection (from any window) or cut buffer 0.
proc Text_Selection {} {
    set sel ""
    catch {set sel [selection get]}
    if {$sel == ""} {
	catch {set sel [cutbuffer get 0]}
    }
    return $sel
}

#
#
#

proc Text_Delete {w start {end {}} {addkill 0}} {
    global TextNames TextType tk_priv
    if {![info exists TextType($w)]} {
	set TextType($w) text
    }
    set st [$w index $start]
    if {$end == {}} {
	set e [$w index "$start + 1c"]
    } else {
	set e [$w index $end]
    }
    case $TextType($w) {
	text {
	    if {[info exists TextNames($w)]} {
		usertextdelete $TextNames($w) $st $e
	    }
	    Text_DoKill $w $st $e $addkill
	    $w delete $st $e
	}
	readonly {
	    return
	}
	typescript {
	    if {[$w compare $st < fence]} {
		set st fence
	    }
	    if {[$w compare $e < fence]} {
		set e fence
	    }
	    if {[$w compare insert < $st]} {
		$w mark set insert $st
	    }
	    Text_DoKill $w $st $e $addkill
	    $w delete $st $e
	}
    }
}

# Do kill buffer processing.  If addkill is true, then add to the kill ring
# if the start or end point lines up with the old delete point.  Otherwise,
# zap the delete point.
proc Text_DoKill {w start end addkill} {
    global tk_priv
    if {! $addkill} {
	set tk_priv(delstate) {}
	return
    }
    if ![info exists tk_priv(delstate)] {
	set tk_priv(delstate) {}
    }
    if [$w compare $start == $end] { return }
    set text [$w get $start $end]
    set oldwin [lindex $tk_priv(delstate) 0]
    set oldmode [lindex $tk_priv(delstate) 1]
    set oldpos [lindex $tk_priv(delstate) 2]
    if {$oldwin != $w || $oldmode != "killing" || $oldpos == ""} {
	cutbuffer rotate 1
	set tk_priv(lastsel) $text
    } elseif {[$w compare $start == $oldpos]} {
	set tk_priv(lastsel) "$tk_priv(lastsel)$text"
    } elseif {[$w compare $end == $oldpos]} {
	set tk_priv(lastsel) "$text$tk_priv(lastsel)"
    } else {
	cutbuffer rotate 1
	set tk_priv(lastsel) $text
    }
    cutbuffer set 0 $tk_priv(lastsel)
    selection own $w "Text_LoseSelection $w"
    set tk_priv(delstate) "$w killing $start"
}

proc Text_Insert {w place text} {
    global TextNames TextType tk_priv
    set tk_priv(delstate) {}
    if {![info exists TextType($w)]} {
	set TextType($w) text
    }
    set pl [$w index $place]
    case $TextType($w) {
	text {
	    if {[info exists TextNames($w)]} {
		usertextinsert $TextNames($w) $pl $text
	    }
	    $w insert $pl $text
	}
	readonly {
	    return
	}
	typescript {
	    # Insert at end if we're not the editable region.
	    if {[$w compare $place < fence]} {
		$w mark set insert end
		set place insert
	    }
	    # Trim at first newline (in case it's a paste).
	    if {[string first "\n" $text]} {
		# Save part of input after cursor and glue it onto
		# inserted text.
		set saveText [$w get insert end]
		Text_Delete $w insert end
	    } else {
		set saveText ""
	    }
	    while {$text != ""} {
		set firstNL [string first "\n" $text]
		if {$firstNL != -1} {
		    set t [string range $text 0 [expr $firstNL-1]]
		    set text [string range $text [expr $firstNL+1] end]
		} else {
		    set t $text
		    set text ""
		}
		set pl [$w index $place]
		# Have to save and reset fence because marks end up at the
		# end of inserted strings, and we want it at the beginning.
		set f [$w index fence]
		$w insert $pl $t
		$w mark set fence $f
		if {$firstNL != -1} {
		    ts_SendLine $w
		}
	    }
	    if {$saveText != ""} {
		# Now paste back in the saved text such that the insert
		# point ends up in the right place.
		set f [$w index fence]
		set i [$w index insert]
		$w insert insert $saveText
		$w mark set fence $f
		$w mark set insert $i
	    }
	}
    }
}

proc Text_Yank { w } {
    global tk_priv
    set sel [Text_Selection]
    if {$sel != ""} {
	set start [$w index insert]
	Text_Insert $w insert $sel
	$w yview -pickplace insert
	set end [$w index insert]
	set tk_priv(delstate) "$w yank $start $end"
    }
}

proc Text_YankPop { w } {
    global tk_priv
    set oldwin [lindex $tk_priv(delstate) 0]
    set oldmode [lindex $tk_priv(delstate) 1]
    set oldstart [lindex $tk_priv(delstate) 2]
    set oldend [lindex $tk_priv(delstate) 3]
    if {$w != $oldwin || $oldmode != "yank" || [$w compare insert != $oldend]} {
	set tk_priv(delstate) {}
	return
    }
    Text_Delete $w $oldstart $oldend
    cutbuffer rotate -1
    set start [$w index insert]
    Text_Insert $w insert [cutbuffer get 0]
    set end [$w index insert]
    set tk_priv(delstate) "$w yank $start $end"
}

proc Text_MoveInsert {w place} {
    global tk_priv
    set tk_priv(selectMode) char
    set tk_priv(delstate) {}
#    $w tag remove sel 0.0 end
    $w mark set insert $place
    $w yview -pickplace insert
}

proc Text_MoveToBOL {w} {
    global TextType
    if ![info exists TextType($w)] {
	set TextType($w) text
    }
    if {$TextType($w) == "typescript" && [$w compare insert > fence]} {
	Text_MoveInsert $w fence
    } else {
	Text_MoveInsert $w "insert linestart"
    }
}

proc Text_PrevWord {w index} {
    set cur $index
    while {[$w compare 1.0 < $cur]} {
	set text [$w get "$cur linestart" $cur]
	if {[regexp -indices {^(|.*[^a-zA-Z0-9])[a-zA-Z0-9]+[^a-zA-Z0-9]*$} $text ignore whitespace]} {
	    set end [expr [lindex $whitespace 1]+1]
	    return [$w index "$cur linestart + $end c"]
	}
	set cur [$w index "$cur linestart -1c"]
    }
    return 1.0
}

proc Text_NextWord {w index} {
    set cur $index
    while {[$w compare $cur < end]} {
	set text [$w get $cur "$cur lineend"]
	if {[regexp -indices {[^a-zA-Z0-9]*([a-zA-Z0-9]+)} $text ignore whitespace]} {
	    set end [expr [lindex $whitespace 1]+1]
	    return [$w index "$cur + $end c"]
	}
	set cur [$w index "$cur lineend +1c"]
    }
    return end
}

proc Text_KillSelection { w } {
    global tk_priv
    set res [expr {! [catch {
	Text_Delete $w sel.first sel.last
    }]}]
    $w tag remove sel 0.0 end
    return $res
}

proc Text_Backspace w {
    if {! [Text_KillSelection $w]} {
	Text_Delete $w insert-1c insert
    }
}

proc Text_DelRight w {
    if {! [Text_KillSelection $w]} {
	Text_Delete $w insert
    }
}

proc Text_DelWordLeft w {
    if {! [Text_KillSelection $w]} {
	Text_Delete $w [Text_PrevWord $w insert] insert 1
    }
}

proc Text_DelWordRight w {
    if {! [Text_KillSelection $w]} {
	Text_Delete $w insert [Text_NextWord $w insert] 1
    }
}

proc Text_KillRight w {
    if {! [Text_KillSelection $w]} {
	if {[$w index insert] == [$w index {insert lineend}]} {
	    Text_Delete $w insert insert+1c 1
	} else {
	    Text_Delete $w insert "insert lineend" 1
	}
    }
}

proc Text_KillLeft w {
    if {! [Text_KillSelection $w]} {
	if {[$w index insert] == [$w index {insert linestart}]} {
	    Text_Delete $w insert-1c insert 1
	} else {
	    Text_Delete $w "insert linestart" insert 1
	}
    }
}

# Get the fence (for typescripts) or 1.0.
proc Text_GetFence { w } {
    global TextType
    if ![info exists TextType($w)] {
	set TextType($w) text
    }
    if {$TextType($w) == "typescript"} {
	return [$w index fence]
    } else {
	return 1.0
    }
}

proc Text_TransposeChars w {
    if {[$w compare insert >= "[Text_GetFence $w] + 2c"]} {
	set c [$w get insert-1c insert]
	Text_Delete $w insert-1c insert
	Text_Insert $w insert-1c $c
    }
}

# Swap words on either side of insertion point.
proc Text_TransposeWords w {
    set start1 [Text_PrevWord $w insert]
    set end1   [Text_NextWord $w $start1]
    set end2   [Text_NextWord $w $end1]
    set start2 [Text_PrevWord $w $end2]
    if {[$w compare $end1 > $start2]
	|| [$w compare $start1 < [Text_GetFence $w]]} {
	return
    }
    set w1 [$w get $start1 $end1]
    set w2 [$w get $start2 $end2]
    $w mark set twMark $end2
    Text_Delete $w $start2 $end2
    Text_Insert $w $start2 $w1
    Text_Delete $w $start1 $end1
    Text_Insert $w $start1 $w2
    Text_MoveInsert $w twMark
}

proc Text_GotoLine { w } {
    set sel [Text_Selection]
    if {[regexp {^[0-9]+$} $sel]} {
	Text_MoveInsert $w $sel.0
    }
}

proc Text_SearchForward { w } {
    set sel [string tolower [Text_Selection]]
    if {$sel == {}} { return }
    set incr 1000
    set len [string length $sel]
    set pos insert+1c
    while {[$w compare $pos < end]} {
	set s [string tolower [$w get $pos "$pos + $len c + $incr c"]]
	set offset [string first $sel $s]
	if {$offset >= 0} {
	    Text_MoveInsert $w "$pos + $offset c"
	    return
	}
	set pos [$w index "$pos + $incr c"]
    }
}

proc Text_SearchBackward { w } {
    set sel [string tolower [Text_Selection]]
    if {$sel == {}} { return }
    set incr 1000
    set len [string length $sel]
    set pos insert-1c
    while {[$w compare 1.0 < $pos]} {
	set s [string tolower [$w get "$pos - $incr c" "$pos + $len c"]]
	set offset [string last $sel $s]
	if {$offset >= 0} {
	    Text_MoveInsert $w "$pos - $incr c + $offset c"
	    return
	}
	set pos [$w index "$pos - $incr c"]
    }
}

proc Text_SetInsert { w mark } {
    Text_MoveInsert $w $mark
    $w mark set anchor insert
    focus $w
}
proc Text_WordSelect { w mark } {
    global tk_priv
    set tk_priv(selectMode) word
    $w mark set insert "$mark wordstart"
    Text_SelectTo $w insert
}
proc Text_LineSelect { w mark } {
    global tk_priv
    set tk_priv(selectMode) line
    $w mark set insert "$mark linestart"
    Text_SelectTo $w insert
}
if {0} {

bind Text <1> {
    Text_MoveInsert %W @%x,%y
    %W mark set anchor insert
    if {[lindex [%W config -state] 4] == "normal"} {focus %W}
}
bind Text <Double-1> {
    set tk_priv(selectMode) word
    %W mark set insert "@%x,%y wordstart"
    Text_SelectTo %W insert
}
bind Text <Triple-1> {
    set tk_priv(selectMode) line
    %W mark set insert "@%x,%y linestart"
    Text_SelectTo %W insert
}
bind Text <B1-Motion> {
    Text_SelectTo %W @%x,%y
}
bind Text <ButtonRelease-1> { Text_SelectionEnd %W 1 }
bind Text <2> { Text_Yank %W }

bind Text <3> { Text_StartExtend %W @%x,%y }
bind Text <B3-Motion> { Text_SelectTo %W @%x,%y }
bind Text <ButtonRelease-3> { Text_SelectionEnd %W 0 }

bind Text <Any-KeyPress> {
    if {"%A" != "" && " " <= "%A" && "%A" <= "~"} {
	Text_KillSelection %W
	Text_Insert %W insert %A
	%W yview -pickplace insert
    }
}
bind Text <Return> {Text_Insert %W insert \n; %W yview -pickplace insert}
bind Text <Delete> {Text_Backspace %W; %W yview -pickplace insert}
bind Text <Control-a> {Text_MoveToBOL %W}
bind Text <Control-b> {Text_MoveInsert %W insert-1c}
bind Text <Control-d> {Text_DelRight %W; %W yview -pickplace insert}
bind Text <Control-e> {Text_MoveInsert %W "insert lineend"}
bind Text <Control-f> {Text_MoveInsert %W insert+1c}
bind Text <Control-h> {Text_Backspace %W; %W yview -pickplace insert}
bind Text <Control-j> {Text_Insert %W insert \n; %W yview -pickplace insert}
bind Text <Control-k> {Text_KillRight %W; %W yview -pickplace insert}
bind Text <Control-m> {Text_Insert %W insert \n; %W yview -pickplace insert}
bind Text <Control-n> {Text_MoveInsert %W insert+1l}
bind Text <Control-o> {
    Text_Insert %W insert \n
    Text_MoveInsert %W insert-1c
}
bind Text <Control-p> { Text_MoveInsert %W insert-1l }
bind Text <Control-r> { Text_SearchBackward %W }
bind Text <Control-s> { Text_SearchForward %W }
bind Text <Control-t> { Text_TransposeChars %W }
bind Text <Control-w> { Text_KillSelection %W }
bind Text <Meta-b> { Text_MoveInsert %W [Text_PrevWord %W insert] }
bind Text <Meta-d> { Text_DelWordRight %W }
bind Text <Meta-f> { Text_MoveInsert %W [Text_NextWord %W insert] }
bind Text <Meta-h> { Text_DelWordLeft %W }
bind Text <Meta-n> { Text_GotoLine %W }
bind Text <Meta-t> { Text_TransposeWords %W }
bind Text <Meta-less> { Text_MoveInsert %W 1.0 }
bind Text <Meta-greater> { Text_MoveInsert %W end }

bind Text <Control-y> { Text_Yank %W }
bind Text <Meta-y> { Text_YankPop %W }

bind Text <Delete> {Text_Backspace %W; %W yview -pickplace insert}
bind Text <BackSpace> {Text_Backspace %W; %W yview -pickplace insert}
bind Text <Meta-Delete> { Text_DelWordLeft %W }
bind Text <Meta-BackSpace> { Text_DelWordLeft %W }

}


