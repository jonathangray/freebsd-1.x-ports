# 
# ftoc.tcl
#
# Folder table of contents display.
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Ftoc_Init {} {
    global ftoc
    set ftoc(displayValid) 0		;# 1 => pick results, not full scan
    set ftoc(displayDirty) 0		;# 1 => display differs from cache
    if ![info exists ftoc(skipMarked)] {
	set ftoc(skipMarked) 1		;# Skip marked in Next, Prev
    }
    set ftoc(mono) [string match mono* [tk colormodel .]]
    # Parameters to the Next button
    Preferences_Add "Scan Listing" \
"These settings affect the behavior of Exmh as you move through the scan listing to view and mark messages.
While the default for Auto Commit is OFF, I suggest you try it out.
Messages are still temporarily marked, but the commit is done when you need it." {
    {ftoc(implied) impliedDirection ON "Implied Direction"
"If set, Exmh will remember your current direction,
next or previous, and go that way after you mark a
message for deletion or refiling."}
    {ftoc(nextGuard) nextGuard OFF "Next Guard"
"If set, Exmh will warn you that you are about to
change folders when you hit Next.  This means you
end up hitting Next twice to chain the to next
folder with unseen messages."}
    {ftoc(autoCommit) autoCommit OFF "Auto Commit"
"If set, Exmh will invoke the Commit operation to
commit deletions and refiles when it would otherwise
just complain that such a commit is required."}
    }
}
proc Ftoc_Reset { numMsgs msgid folder } {
    global ftoc exwin
    Exmh_Debug Ftoc_Reset $folder has $numMsgs msgs
    set ftoc(numMsgs) $numMsgs		;# num msgs in the scan listing
    set ftoc(changed) 0			;# Number of moves/deletes marked
    set ftoc(lineset) {}		;# set of selected messages
    set ftoc(pickone) 1			;# lineset is empty
    set ftoc(folder) $folder		;# Currently displayed folder
    set ftoc(direction) next		;# assumed next direction
    set ftoc(softChange) [expr {! $ftoc(nextGuard)}]
    set ftoc(lasthit) {}		;# search anchor
    if {$msgid == {}} {
	set ftoc(curLine) {}		;# current display line number
    } else {
	set ftoc(curLine) [Ftoc_FindMsg $msgid]
    }
}
proc Ftoc_Update { numMsgs folder } {
    # Update size of message list after inc'ing into current folder
    global ftoc
    Exmh_Debug Ftoc_Update $folder has $numMsgs msgs
    set ftoc(numMsgs) $numMsgs
}

proc Ftoc_Bindings { w } {
    # Bindings for the ftoc text widget
    Bindings_Main $w

    # Button-1 starts selection range
    bind $w <Button-1> {
	FtocRangeStart [lindex [split [%W index current] .] 0]
	focus %W
    }
    bind $w <Shift-Button-1> {
	FtocRangeAdd [lindex [split [%W index current] .] 0]
	focus %W
    }
    bind $w <B1-Motion> {
	FtocRangeExtendXY %x %y
    }
    bind $w <Shift-B1-Motion> {
	FtocRangeExtendXY %x %y
    }
    bind $w <Any-ButtonRelease-1> {
	FtocRangeEnd [lindex [split [%W index current] .] 0]
    }
    bind $w <Shift-ButtonRelease-1> {
	FtocRangeEnd [lindex [split [%W index current] .] 0] true
    }
    bind $w <Button-3> {
	set lineNumber [lindex [split [%W index current] .] 0]
	Msg_Pick $lineNumber noshow
	focus %W
    }
    bind $w <Double-Button-1> { }
    bind $w <Triple-Button-1> { }
}
proc FtocRangeStart { line } {
    # For normal button-down "start a selection"
    global ftoc
    Ftoc_RangeUnHighlight
    set ftoc(pickstart) $line
    set ftoc(pickend) $line
    set ftoc(pickstate) new
    set ftoc(extend) 0
    Ftoc_RangeHighlight $line $line
}
proc FtocRangeAdd { line } {
    # For shift-select "add to selection"
    global ftoc
    set ftoc(pickstart) $line
    set ftoc(pickend) $line
    set ftoc(pickstate) invert
    set ftoc(extend) 0
    FtocRangeInvert $line $line
}
proc FtocRangeEnd { {line {}} {addcurrent false} } {
    # For end of button sweep
    global ftoc exwin
    set ftoc(extend) 0
    if ![info exists ftoc(pickend)] {
	# Spurious button-release event
	return
    }
    if {$line != {}} {
	FtocRangeExtend $line
    }
    set lineset {}
    foreach range [concat \
	[FtocMakePairs [$exwin(ftext) tag ranges range]] \
	[FtocMakePairs [$exwin(ftext) tag ranges drange]] \
	[FtocMakePairs [$exwin(ftext) tag ranges mrange]]] {
	set mark1 [lindex $range 0]
	set line [lindex [split $mark1 .] 0]
	lappend lineset $line
    }
    FtocPickRange $lineset $addcurrent
    unset ftoc(pickend)
}
proc Ftoc_PickMsgs { ids } {
    # For adding to the selection by message number
    global ftoc
    Exmh_Status "Marking [llength $ids] hits"
    set lines {}
    for {set L 0} {$L <= $ftoc(numMsgs)} {incr L} {
	if {[lsearch $ids [Ftoc_MsgNumber $L]] >= 0} {
	    lappend lines $L
	}
    }
    Ftoc_LinesHighlight $lines
    FtocPickRange $lines
}
proc FtocRangeExtendXY { x y } {
    global ftoc exwin widgetText

    if ![info exists ftoc(extend)] {
	return
    }
    set active $ftoc(extend)

    set h [winfo height $exwin(ftext)]
    if {$y > $h} {
	set ftoc(extend) [expr $y-$h]
    } else {
	if {$y < 0} {
	    set ftoc(extend) $y
	} else {
	    set ftoc(extend) 0
	}
    }
    
    if {$ftoc(extend) == 0} {
	FtocRangeExtend [lindex [split [$exwin(ftext) index @$x,$y] .] 0]
    } else {
	if {! $active} {
	    set ftoc(lastmark) [lindex [ split [$exwin(ftext) index @$x,$y] .] 0]
	    after $widgetText(selectDelay) [list FtocSelExtend]
	}
    }
}
proc FtocSelExtend {} {
    global ftoc exwin widgetText
    set w $exwin(ftext)
    if {$ftoc(extend) != 0} {
	catch {
	    set delta [expr {$ftoc(extend) / 16}]
	    if {$delta == 0} {
		set delta [expr { ($ftoc(extend) < 0) ? -1 : 1 }]
	    }
	    set newmark [expr {$ftoc(lastmark) + $delta}]
	    FtocRangeExtend $newmark
	    set ftoc(lastmark) $newmark
	    $w yview -pickplace $newmark.0
	    after $widgetText(selectDelay) [list FtocSelExtend]
	}
    }    
}
proc FtocRangeExtend { line } {
    global ftoc
    if ![info exists ftoc(pickend)] {
	return
    }
    if {$line <= 0} {
	set line 1
    }
    if {$line > $ftoc(numMsgs)} {
	set line $ftoc(numMsgs)
    }
    if {$line == $ftoc(pickend)} {
	# Invariant, previously defined selection is fine.
	return
    }
    if {$line == 0} {
	# no messages in folder
	return
    }
    if {$ftoc(pickstate) != "invert"} {
	if {$ftoc(pickstart) < $ftoc(pickend)} {
	    # Growing downward
	    if {$line > $ftoc(pickend)} {
		Ftoc_RangeHighlight [expr $ftoc(pickend)+1] $line
	    } else {
		if {$line < $ftoc(pickstart)} {
		    if {$ftoc(pickstart) != $ftoc(pickend)} {
			# Change direction
			FtocRangeClear [expr $ftoc(pickstart)+1] \
					$ftoc(pickend)
		    }
		    Ftoc_RangeHighlight [expr $ftoc(pickstart)-1] $line
		} else {
		    # Shrink selection
		    FtocRangeClear [expr $line+1] $ftoc(pickend)
		}
	    }
	} else {
	    # Growing upward
	    if {$line < $ftoc(pickend)} {
		Ftoc_RangeHighlight [expr $ftoc(pickend)-1] $line
	    } else {
		if {$line > $ftoc(pickstart)} {
		    if {$ftoc(pickstart) != $ftoc(pickend)} {
			# Change direction
			FtocRangeClear [expr $ftoc(pickstart)-1] \
					$ftoc(pickend)
		    }
		    Ftoc_RangeHighlight [expr $ftoc(pickstart)+1] $line
		} else {
		    # Shrink selection
		    FtocRangeClear [expr $line-1] $ftoc(pickend)
		}
	    }
	}
    } else {
	if {$ftoc(pickstart) < $ftoc(pickend)} {
	    # Growing downward
	    if {$line > $ftoc(pickend)} {
		FtocRangeInvert [expr $ftoc(pickend)+1] $line
	    } else {
		if {$line < $ftoc(pickstart)} {
		    if {$ftoc(pickstart) != $ftoc(pickend)} {
			# Change direction
			FtocRangeInvert [expr $ftoc(pickstart)+1] \
					$ftoc(pickend)
		    }
		    FtocRangeInvert [expr $ftoc(pickstart)-1] $line
		} else {
		    # Shrink selection
		    FtocRangeInvert [expr $line+1] $ftoc(pickend)
		}
	    }
	} else {
	    # Growing upward
	    if {$line < $ftoc(pickend)} {
		FtocRangeInvert [expr $ftoc(pickend)-1] $line
	    } else {
		if {$line > $ftoc(pickstart)} {
		    if {$ftoc(pickstart) != $ftoc(pickend)} {
			# Change direction
			FtocRangeInvert [expr $ftoc(pickstart)-1] \
					$ftoc(pickend)
		    }
		    FtocRangeInvert [expr $ftoc(pickstart)+1] $line
		} else {
		    # Shrink selection
		    FtocRangeInvert [expr $line-1] $ftoc(pickend)
		}
	    }
	}
    }
    set ftoc(pickend) $line
}
proc FtocRangeInvert { start end } {
    global exwin
    set win $exwin(ftext)
    if {$start > $end} {
	set tmp $start ; set start $end ; set end $tmp
    }
    for {set line $start} {$line <= $end} {incr line} {
	catch {
	    set newtag range
	    set oldtag {}
	    foreach tag [$win tag names $line.0] {
		case $tag {
		    deleted { set newtag drange ; set oldtag $tag ; break; }
		    moved { set newtag mrange ; set oldtag $tag ; break; }
		    range { set newtag {} ; set oldtag $tag ; break; }
		    drange { set newtag deleted ; set oldtag $tag ; break; }
		    mrange { set newtag moved ; set oldtag $tag ; break; }
		}
	    }
	    if {$oldtag != {}} {
		$win tag remove $oldtag $line.0 $line.end
	    }
	    if {$newtag != {}} {
		$win tag add $newtag $line.0 $line.end
	    }
	}
    }
}
proc Ftoc_RangeHighlight { start end } {
    global exwin
    set win $exwin(ftext)
    if {$start > $end} {
	set tmp $start ; set start $end ; set end $tmp
    }
    for {set line $start} {$line <= $end} {incr line} {
	set newtag range
	foreach tag [$win tag names $line.0] {
	    case $tag {
		{drange deleted} { set newtag drange ;  break; }
		{mrange moved} { set newtag mrange ;  break; }
	    }
	}
	$win tag add $newtag $line.0 $line.end
    }
}
proc Ftoc_LinesHighlight { lines } {
    global exwin
    set win $exwin(ftext)
    if {$lines == {}} {
	return
    }
    WidgetTextYview $exwin(ftext) -pickplace [lindex $lines 0].0
    update idletasks
    foreach line $lines {
	set newtag range
	foreach tag [$win tag names $line.0] {
	    case $tag {
		{drange deleted} { set newtag drange ;  break; }
		{mrange moved} { set newtag mrange ;  break; }
	    }
	}
	$win tag add $newtag $line.0 $line.end
    }
}
proc FtocRangeClear { start end } {
    global exwin
    set win $exwin(ftext)
    if {$start > $end} {
	set tmp $start ; set start $end ; set end $tmp
    }
    for {set line $start} {$line <= $end} {incr line} {
	catch {
	    set newtag {}
	    set oldtag range
	    foreach tag [$win tag names $line.0] {
		case $tag {
		    drange { set newtag deleted ; set oldtag drange; break; }
		    mrange { set newtag moved ; set oldtag mrange; break; }
		    range { break }
		}
	    }
	    $win tag remove $oldtag $line.0 $line.end
	    if {$newtag != {}} {
		$win tag add $newtag $line.0 $line.end
	    }
	}
    }
}
proc Ftoc_RangeUnHighlight { } {
    global exwin
    set win $exwin(ftext)
    foreach tag {range drange mrange} {
	foreach range [FtocMakePairs [$win tag ranges $tag]] {
	    eval $win tag remove $tag $range
	    if {$tag == "drange"} {
		eval $win tag add deleted $range
	    }
	    if {$tag == "mrange"} {
		eval $win tag add moved $range
	    }
	}
    }
}

# For user programming
proc Ftoc_BindDouble { cmd } {
    global exwin
    bind $exwin(ftext) <Double-1> $cmd
}
proc Ftoc_BindRight { cmd } {
    global exwin
    bind $exwin(ftext) <3> $cmd
}

proc Ftoc_FindMsg { msgid {line {}} } {
    global ftoc
    if {$line != {}} {
	switch -glob -- $line {
	    first  {return 1}
	    last   {return $ftoc(numMsgs)}
	    [0-9]* {
		if {$line > $ftoc(numMsgs)} {
		    return $ftoc(numMsgs)
		} else {
		    return $line
		}
	    }
	    default {return {}}
	}
    }
    if {$msgid == {}} {
	return {}
    }
    set start $ftoc(numMsgs)
    set lastMsg [Ftoc_MsgNumber $start]
    if {$lastMsg <= $msgid} {
	# Search forward
	for {set L $start} {$L <= $ftoc(numMsgs)} {incr L} {
	    if {[Ftoc_MsgNumber $L] == $msgid} {
		return $L
	    }
	}
	for {set L 1} {$L < $start} {incr L} {
	    if {[Ftoc_MsgNumber $L] == $msgid} {
		return $L
	    }
	}
    } else {
	# Search backward
	for {set L $start} {$L > 0} {incr L -1} {
	    if {[Ftoc_MsgNumber $L] == $msgid} {
		return $L
	    }
	}
	for {set L $ftoc(numMsgs)} {$L >= $start} {incr L -1} {
	    if {[Ftoc_MsgNumber $L] == $msgid} {
		return $L
	    }
	}
    }
    return {}
}
proc Ftoc_MsgNumber { L } {
    global exwin
    if [catch {$exwin(ftext) get $L.0 $L.end} line] {
	return ""
    }
    return [Ftoc_MsgNumberRaw $line]
}
proc Ftoc_MsgNumberRaw { line } {
    if [regexp {( *)([0-9]+)} $line foo foo2 number] {
	return $number
    } else {
	return ""
    }
}
proc FtocPickRange { lineset {addcurrent false}} {
    # Select a range of messages, or add to the current range
    global exwin ftoc
    if {$lineset == {}} {
	return			;# spurious <ButtonRelease-1> events
    }
    set ftoc(lineset) $lineset
    set ftoc(pickone) 0
    if {($addcurrent != "false") && ($ftoc(curLine) != {})} {
	if {[lsearch $lineset $ftoc(curLine)] < 0} {
	    lappend ftoc(lineset) $ftoc(curLine)
	    Ftoc_RangeHighlight $ftoc(curLine) $ftoc(curLine)
	}
    }
    if {[llength $ftoc(lineset)] == 1} {
	# This calls Msg_Change,
	# which calls Ftoc_ClearCurrent, which sets pickone to 1,
	# and calls Ftoc_Change, which sets curline
	Msg_Pick [lindex $ftoc(lineset) 0] show
    } else {
	Buttons_Range	;# Enable actions on ranges
	foreach line $ftoc(lineset) {
	    if {$ftoc(curLine) == $line} {
		# Unhighlight current if it is within range.
		# This is a bit lame, but fixes display problems that
		# stem from combining the current and range looks.
		$exwin(ftext) tag remove current $ftoc(curLine).0 $ftoc(curLine).end
	    }
	}
    }
}
proc Ftoc_PickSize {} {
    global ftoc
    set len [llength $ftoc(lineset)]
    if {$len == 0} {
	return [llength $ftoc(curLine)]
    } else {
	return $len
    }
}

# Ftoc_ClearCurrent and Ftoc_Change are two parts of
# dinking the ftoc display when advancing a message.

proc Ftoc_ClearCurrent {} {
    # Clear display of current message
    global ftoc exwin
    set ftoc(pickone) 1
    set ftoc(lineset) {}

    if {$ftoc(curLine) != {}} {
	$exwin(ftext) tag remove current $ftoc(curLine).0 $ftoc(curLine).end
	Ftoc_RescanLine $ftoc(curLine)
    }
    return $ftoc(curLine)
}
proc Ftoc_Change { msgid line {show show} } {
    global ftoc exwin
    set ftoc(curLine) [Ftoc_FindMsg $msgid $line]
    if {$ftoc(curLine) == {}} {
	set ok 0
    } else {
	if {$show == "show"} {
	    $exwin(ftext) tag remove unseen $ftoc(curLine).0 $ftoc(curLine).end
	}
	Ftoc_RescanLine $ftoc(curLine) +
	$exwin(ftext) tag add current $ftoc(curLine).0 $ftoc(curLine).end
	FtocHackCurrentHighlight
	$exwin(ftext) yview -pickplace [expr $ftoc(curLine)-1]
	set ok 1
    }
    return $ok
}
proc FtocHackCurrentHighlight {} {
    global exwin ftoc
    if {$ftoc(mono)} {
	# Monochrome hack
	if [regexp {(deleted|moved|range)} \
		    [$exwin(ftext) tag names $ftoc(curLine).0]] {
	    $exwin(ftext) tag remove current \
		    $ftoc(curLine).0 $ftoc(curLine).end
	}
    }
}
proc Ftoc_ShowUnseen { folder } {
    global exwin flist
    set unseen [Flist_UnseenMsgs $folder]
    if {[llength $unseen] > 0} {
	set end [$exwin(ftext) index end]
	set line [lindex [split $end .] 0]
	for {} {$line > 0} {incr line -1} {
	    set msgNum [Ftoc_MsgNumber $line]
	    set i [lsearch $unseen $msgNum]
	    if {$i >= 0} {
		$exwin(ftext) tag add unseen $line.0 $line.end
		set unseen [lreplace $unseen $i $i]
		if {[llength $unseen] == 0} {
		    return 1
		}
	    }
	}
    } else {
	return 0
    }
}
proc Ftoc_MarkSeen { ids } {
    global exwin
    foreach msgid $ids {
	set L [Ftoc_FindMsg $msgid]
	if {$L != {}} {
	    $exwin(ftext) tag remove unseen $L.0 $L.end
	}
    }
}
proc Ftoc_RescanLine { ix {plus none} } {
    global exmh exwin
    if [catch {
	set text [$exwin(ftext) get ${ix}.0 ${ix}.end]
	set ok 0
	case $plus {
	    "none" {
		# Replace + (current marker) with blank
		set ok [regsub {( *[0-9]+)(\+)} $text {\1 } newtext]
	    }
	    "+" {
		# Stick a + after the number, if needed
		if ![regexp {( *)([0-9]+)(\+)} $text] {
		    set ok [regsub {( *[0-9]+)( )} $text {\1+} newtext]
		}
	    }
	    "dash" {
		# Stick a - after the number, if needed
		if ![regexp {( *)([0-9]+).-} $text] {
		    set ok [regsub {( *[0-9]+.)(.)} $text {\1-} newtext]
		}
	    }
	}
	if {$ok} {
	    set tags [$exwin(ftext) tag names ${ix}.0]
	    $exwin(ftext) configure -state normal
	    $exwin(ftext) delete ${ix}.0 ${ix}.end
	    $exwin(ftext) insert ${ix}.0 $newtext
	    $exwin(ftext) configure -state disabled
	    foreach tag $tags {
		$exwin(ftext) tag add $tag ${ix}.0 ${ix}.end
	    }
	}
    } msg] {
	Exmh_Error "FtocRescanLine $ix : $msg"
    }
}
proc Ftoc_Next { show {implied no} } {
    # Go to the next message in the scan display
    global exmh flist ftoc

    if {$implied != "no" && $ftoc(implied)} {
	if {$ftoc(direction) == "prev"} {
	    Ftoc_Prev $show
	    return
	}
    } else {
	set ftoc(direction) "next"
    }
    Exmh_Debug Ftoc_Next curline=$ftoc(curLine)
    if {$ftoc(curLine) == {}} {
	# No current; go for the next unseen.
	if [Msg_ShowUnseen] {
	    return
	}
    }
    set next [FtocSkipMarked $ftoc(curLine) 1]
    if {($ftoc(curLine) == $next) || \
	($ftoc(curLine) >= $ftoc(numMsgs)) || \
	($ftoc(curLine) <= 0)} {
	# End of folder
	# Try to chain to the next folder with unread messages.
	if {$implied != "no"} {
	    # Implied - chained with some other operation - be lenient
	    if {$ftoc(changed) > 0} {
		# Special case if last message is marked.
		# Nuke current highlighting because it conflicts with
		# current highlight on mono screen.  Better fix is to
		# have separate look for deleted-current.
		if {$ftoc(curLine) != {}} {
		    FtocHackCurrentHighlight
if {0} {
		    global exwin
		    if [regexp {(deleted|moved|range)} \
				[$exwin(ftext) tag names $ftoc(curLine).0]] {
			$exwin(ftext) tag remove current \
				$ftoc(curLine).0 $ftoc(curLine).end
		    }
}
		}
		Exmh_Status ""
		Exmh_Status "Changes pending; End of folder" blue
		return
	    }
	}
	foreach f [Flist_UnseenFolders] {
	    if {$f != $exmh(folder)} {
		if {$ftoc(softChange)} {
		    Folder_Change $f
		    return
		} else {
		    set ftoc(softChange) 1
		    Msg_ClearCurrent
		    Exmh_Status ""
		    Exmh_Status "End of folder; <Next> => $f" blue
		    return
		}
	    }
	}
	Exmh_Status ""
	Exmh_Status "End of folder" blue
    } else {
	# Simple case - go to the next message.
	Msg_Pick $next $show
    }
}
proc Ftoc_Prev { {show show} } {
    global ftoc

    Exmh_Debug Ftoc_Prev
    if {$ftoc(curLine) == {}} {
	if {$ftoc(numMsgs) > 0} {
	    Msg_Pick $ftoc(numMsgs) $show
	}
	return
    }
    if {$ftoc(curLine) > 1} then {
	set ftoc(direction) "prev"
	Msg_Pick [FtocSkipMarked $ftoc(curLine) -1] $show
    }
}
proc Ftoc_PrevMarked { {show show} } {
    global ftoc
    set skip $ftoc(skipMarked)
    set ftoc(skipMarked) 0
    Ftoc_Prev $show
    set ftoc(skipMarked) $skip
}
proc FtocSkipMarked {start inc} {
    global exwin ftoc

    if {$start == {}} {
	return {}
    }
    for {set i [expr $start+$inc]} {$i > 0 && $i <= $ftoc(numMsgs)} {incr i $inc} {
	if {$ftoc(skipMarked) == 0} {
	    return $i
	}
	set marked 0
	foreach tag [$exwin(ftext) tag names $i.0] {
	    if [regexp {(deleted|moved|drange|mrange)} $tag] {
		set marked 1 ; break;
	    }
	}
	if {! $marked} {
	    return $i
	}
    }
    return $start
}

proc Ftoc_Changes {type {allowAuto 1} } {
    global ftoc

    if {$ftoc(changed) != 0} then {
	if {("$allowAuto" == "1") && $ftoc(autoCommit)} {
	    Folder_Commit
	    return 0
	}
	if {$type != {}} {
	    Exmh_Status "$ftoc(changed) Changes pending: $type cancelled" red
	    Sound_Error
	} else {
	    Exmh_Status "Oops, $ftoc(changed) left over changes" red
	    set ftoc(changed) 0
	    return 1
	}
    }
    return $ftoc(changed)
}

proc Ftoc_Iterate { lineVar body } {
    global ftoc
    upvar $lineVar line
    catch {
	if {$ftoc(pickone)} {
	    if {$ftoc(curLine) != {}} {
		set line $ftoc(curLine)
		uplevel 1 $body
	    }
	} else {
	    foreach line $ftoc(lineset) {
		uplevel 1 $body
	    }
	}
    }
}
proc Ftoc_Unmark {} {
    global ftoc

    set hits 0
    Ftoc_Iterate line {
	if [FtocUnmarkInner $line] { incr hits -1}
    }
    Exmh_Status "Unmarked $hits msgs"
    incr ftoc(changed) $hits
}
proc FtocUnmarkInner { line } {
    global exwin
    set res 0
    foreach tag [$exwin(ftext) tag names $line.0] {
	if [regexp {(deleted|moved|drange|mrange)} $tag] {
	    $exwin(ftext) tag remove $tag $line.0 $line.end
	    if [regexp {(drange|mrange)} $tag] {
		eval $exwin(ftext) tag add range $line.0 $line.end
	    }
	    set res 1
	}
    }
    if {! $res} {
	Exmh_Debug FtocUnmarkInner $line miss
    }
    return $res
}
proc Ftoc_Delete { line msgid } {
    global exwin ftoc
    $exwin(ftext) configure -state normal
    $exwin(ftext) delete $line.0 "$line.end + 1 chars"
    $exwin(ftext) configure -state disabled
    set ftoc(displayDirty) 1
}
proc Ftoc_RemoveMark { line msgid } {
    # Flag the current message(s) for deletion
    global ftoc exwin
    if ![FtocUnmarkInner $line] {
	incr ftoc(changed)
    }

    if {$ftoc(pickone)} {
	$exwin(ftext) tag add deleted $line.0 $line.end
    } else {
	$exwin(ftext) tag remove range $line.0 $line.end
	$exwin(ftext) tag add drange $line.0 $line.end
    }
}
proc Ftoc_MoveMark { line msgid } {
    global ftoc exwin exmh
    if ![FtocUnmarkInner $line] {
	incr ftoc(changed)
    }
    # This tag records the target folder
    $exwin(ftext) tag add [list moved $exmh(target)] $line.0 $line.end

    if {$ftoc(pickone)} {
	$exwin(ftext) tag add moved $line.0 $line.end
    } else {
	$exwin(ftext) tag remove range $line.0 $line.end
	$exwin(ftext) tag add mrange $line.0 $line.end
    }
}
proc Ftoc_Commit { rmmCommit moveCommit } {
    global ftoc exwin
    Exmh_Status "Committing $ftoc(changed) changes..." red
    $exwin(ftext) configure -state normal
    FtocCommit deleted $rmmCommit
    FtocCommit moved $moveCommit
    $exwin(ftext) configure -state disabled
    set l $ftoc(curLine)
    if {$l == {}} {
	set l $ftoc(numMsgs)
    }
    if {$l > 0} {
	WidgetTextYview $exwin(ftext) $l
    }
    if {! [Ftoc_Changes {} noautocommit]} {
	Exmh_Status "ok" blue
    }
}
proc FtocCommit {tagname commitProc} {
    global ftoc exmh exwin

    set delmsgs {}
    set pairs [FtocMakeReversePairs [$exwin(ftext) tag ranges $tagname]]
    foreach range $pairs {
	set c0 [lindex $range 0]
	set ce [lindex $range 1]
	scan $c0 "%d" L
	set F {}
	foreach tag [$exwin(ftext) tag names $c0] {
	    if {([llength $tag] == 2) && ([lindex $tag 0] == "moved")} {
		set F [lindex $tag 1]
		if ![info exists movemsgs($F)] {
		    set movemsgs($F) {}
		}
	    }
	}
	set msgid [Ftoc_MsgNumber $L]
	if {$tagname == "deleted"} {
	    # Batch up deletes
	    lappend delmsgs $msgid
	} else {
	    # Build up a list of moved messages
	    # Note that the original order of the messages is maintained,
	    # (We are going from bottom to top thru the display.)
	    # The scan lines are reversed, which is handled by Scan_Move.
#	    set movemsgs($F) [concat $movemsgs($F) " " $msgid]
	    set movemsgs($F) [concat $msgid " " $movemsgs($F)]
	    lappend movescan($F) [$exwin(ftext) get $c0 "$ce + 1 chars"]
	}
	Msg_UnSeen $msgid	;# avoid MH mark bug
	Flist_MsgSeen $msgid	;# in case deleted or moved w/out viewing
	$exwin(ftext) delete $c0 "$ce + 1 chars"
	set ftoc(displayDirty) 1
	if {$L == $ftoc(curLine)} {
	    set ftoc(curLine) {}
	    Msg_ClearCurrent
	} else {
	    if {$L < $ftoc(curLine)} {
		incr ftoc(curLine) -1
		if {$ftoc(curLine) == 0} {
		    set ftoc(curLine) {}
		}
	    }
	}
	incr ftoc(numMsgs) -1
	incr ftoc(changed) -1
    }
    if {$delmsgs != {}} {
	Exmh_Status "$commitProc $delmsgs" red
	if [catch {
	    Background_Rmm $commitProc $exmh(folder) $delmsgs
	} err] {
	    Exmh_Status $err purple
	}
    }
    if {[catch {array names movemsgs} flist] == 0} {
	foreach f $flist {
	    Exmh_Status "Refiling to $f, $movemsgs($f)" red
	    set ok [Scan_CacheValid $f]
	    set new [file tail [Mh_Path $f new]]
	    if [catch {
		Background_Refile $commitProc $exmh(folder) $movemsgs($f) $f $ok $movescan($f) $new
	    } err] {
		Exmh_Status $err purple
	    }
	}
    }
}
proc FtocMakePairs { list } {
    set result {}
    for {set i 0} {$i < [expr [llength $list]-1]} {incr i +2} {
	set first [lindex $list $i]
	set second [lindex $list [expr $i+1]]
	lappend result [list $first $second]
    }
    if {$result == {}} {
	return $list
    } else {
	return $result
    }
}
proc FtocMakeReversePairs { list } {
    set result {}
    for {set i [expr [llength $list]-1]} {$i >= 0} {incr i -2} {
	set second [lindex $list $i]
	set first [lindex $list [expr $i-1]]
	lappend result [list $first $second]
    }
    if {$result == {}} {
	return $list
    } else {
	return $result
    }
}

proc Ftoc_MoveFeedback { msgid line } {
    global exwin ftoc
    set msg [Exmh_OldStatus]
    if {[string compare $line "last"] == 0} {
	set line $ftoc(numMsgs)
    } elseif {[string compare $line first] == 0} {
	set line 1
    }
    foreach tag [$exwin(ftext) tag names $line.0] {
	if [regexp {moved (.+)} $tag match folder] {
	    Exmh_Status "$msgid => +$folder"
	    return
	}
    }
    Exmh_Status $msg
}
proc Ftoc_FindNext {} {
    Find_It forw
}
proc Ftoc_FindPrev {} {
    Find_It back
}
proc Ftoc_FindMatch {L string} {
    global exwin ftoc
    if {$L == $ftoc(lasthit)} {
	return 0
    }
    if [catch {$exwin(ftext) get $L.0 $L.end} text] {
	return -1	;# off the end or beginning
    }
    if [regexp -nocase $string $text] {
	set ftoc(lasthit) $L
	Msg_Pick $L show
	return 1
    }
    return 0
}

