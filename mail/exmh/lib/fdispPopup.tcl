#
# fdispPopup.tcl
#
# Nested folder popup (or popdown) display.
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc FdispDisplayPopdown {folder pop {bx -1} {by -1}} {
    global fdisp

    set can canvas	;# popdowns only on main display

    set folderSet [Flist_FolderSet $folder]
    set canvas $fdisp($can)
    set width [winfo width $canvas]

    if ![info exists fdisp(maxy,canvas)] {
	return	;# display not initialized yet
    }
    case $fdisp(popdownStyle) in {
	{r*}	{set sq 1}
	default	{set sq 0}
    }
    set tag $folder$sq

    if {($pop == "down") && ($fdisp(poptop) != -1)} {
	if {$fdisp(popdown,0) == $tag} {
	    # Clicking on a folder with popup already displayed.
	    set pop "remove"
	}
    }

    if {$pop == "stack"} {
	if {$fdisp(popdown,$fdisp(poptop)) == $tag} {
	    #Exmh_Debug "Reuse top of stack $tag"
	    FdispPopupView $canvas $tag
	    return
	}
	for {set i 0} {$i <= $fdisp(poptop)} { incr i} {
	    if {$fdisp(popdown,$i) == $tag} {
		#Exmh_Debug popdown already visible $tag
		FdispPopupView $canvas $tag
		return
	    }
	}
	incr fdisp(poptop)
    } else {
	if {$pop == "Xremove" && $fdisp(poptop) >= 0} {
	    # alternate popdown remove code - experimental
	    #Exmh_Debug remove top popdown $fdisp(popdown,$fdisp(poptop))
	    $canvas move $fdisp(popdown,$fdisp(poptop)) -1000 0
	    incr fdisp(poptop) -1
	    # may need to make FdispPopupView scroll view backward
	    return
	}
	for {set i 0} {$i <= $fdisp(poptop)} { incr i} {
	    #Exmh_Debug remove popdown $fdisp(popdown,$i)
	    $canvas move $fdisp(popdown,$i) -1000 0
	}
	if {$pop == "remove"} {
	    FdispPopupResetView $can
	    set fdisp(poptop) -1
	    return
	}
	set fdisp(poptop) 0
    }

    if {[$canvas gettag $tag] != ""} {
	set fdisp(popdown,$fdisp(poptop)) $tag
	#Exmh_Debug reuse popdown $tag
	$canvas move $tag 1000 0
	FdispPopupView $canvas $tag
	return
    }

    Exmh_Status "Building popdown display for $folder"

    set bid [FdispGetBmap $can $folder]
    if {$bid == ""} {
	set id [$canvas find closest $bx $by]
	Exmh_Debug CLOSEST: x=$bx y=$by is id=$id [$canvas type $id]

	if {[$canvas type $id] == "text"} {
	    set bid [$canvas find below $id]
	    Exmh_Debug bid=$bid [$canvas type $bid] @ [$canvas coo $bid]
	} else {
	    set bid $id
	}
    }

    set bbox [$canvas coords $bid]

    set bx1 [lindex $bbox 0]
    set bx2 [lindex $bbox 2]
    set by1 [lindex $bbox 1]
    set by2 [lindex $bbox 3]

    FdispLayoutInner $can -1000 0 [expr ($width*3/4)-1000] $folderSet $folder \
		FdispBindPopupLabel skipSelf $tag

    set bbox [$canvas bbox $tag]
    set wid [expr {$fdisp(xgap)*1/4 + [lindex $bbox 2] - [lindex $bbox 0]}]
    set hei [expr {[lindex $bbox 3] - [lindex $bbox 1]}]

    set wid [expr {$wid - $bx2 + $bx1}]

    set lx [expr $bx1-$wid/2]
    set rx [expr $bx2+4+$wid/2]
    if {$lx < 3} {
	set rx [expr {$rx - $lx + 3}]
	set lx 3
    }
    if {$rx > $width - 4} {
	set lx [expr {$width + $lx - $rx - 4}]
	set rx [expr $width-4]
    }

    if {$sq} {
	set gap 4

	set rx1 $lx
	set ry1 [expr $by2+$gap]
	set rx2 $rx
	set ry2 [expr {$ry1 + $fdisp(ygap)/4+$hei}]

	set loweredge [expr {$ry2 + 4}]

	set box [$canvas create rect $rx1 $ry1 $rx2 $ry2 -fill $fdisp(c_popup)]

	$canvas move $tag [expr {1000 + $rx1 + $fdisp(xgap)*3/4}] \
			[expr {$ry1 + $fdisp(ygap)/2}]
	$canvas raise $tag $box

	$canvas addtag $tag withtag $box
    } else {
	set gap 14

	set px1 $bx1
	# +2 makes us overlap the lower black border
	set py1 [expr $by2+2]
	set px2 [expr $bx2+4]
	set py2 $py1
	set px3 $rx
	set py3 [expr $by2+$gap]
	set px4 $px3
	set py4 [expr {$py3 + $fdisp(ygap)/4+$hei}]
	set px5 $lx
	set py5 $py4
	set px6 $px5
	set py6 $py3

	set loweredge [expr {$py4 + 4}]

	set border [$canvas create poly $px1 $py1 $px2 $py2 \
			$px3 $py3 $px4 $py4 \
			$px5 $py5 $px6 $py6 -fill $fdisp(c_fg)]

	set box [$canvas create poly [expr $px1+1] [expr $py1+1] \
			[expr $px2-1] [expr $py2+1] \
			[expr $px3-1] [expr $py3+1] \
			[expr $px4-1] [expr $py4-1] \
			[expr $px5+1] [expr $py5-1] \
			[expr $px6+1] [expr $py6+1] \
			-fill $fdisp(c_popup)]

	$canvas move $tag [expr {1000 + $px6 + $fdisp(xgap)*3/4}] \
			[expr {$py6 + $fdisp(ygap)/2}]
	$canvas raise $tag $box

	$canvas addtag $tag withtag $border
	$canvas addtag $tag withtag $box
    }
    # Cannot bind to <Leave> because that triggers when you enter a label.
    $canvas bind $box <Double-$fdisp(navbutton)> \
	[list FdispDisplayPopdown {} remove]

    set sr [lindex [$canvas configure -scrollregion] 4]
    if  {$loweredge > [lindex $sr 3]} {
	$canvas configure -scrollregion [concat [lrange $sr 0 2] $loweredge]
    }

    Exmh_Status ""
    set fdisp(popdown,$fdisp(poptop)) $tag
    FdispPopupView $canvas $tag
    # Highlight newly created labels
    Fdisp_HighlightCanvas canvas
}
proc FdispBindPopupLabel { can id ftype f } {
    global fdisp
    set canvas $fdisp($can)
    if {[string compare $ftype hasNested] == 0} {
	# This label has nested folders
	case $fdisp(popdownAction) {
	    redisplay {
		$canvas bind $id <$fdisp(navbutton)> \
		    [list FdispMain $f]
	    }
	    enter {
		$canvas bind $id <Any-Enter> \
		    [list FdispDisplayPopdown $f stack %x %y]
	    }
	    navbutton {
		$canvas bind $id <$fdisp(navbutton)> \
		    [list FdispDisplayPopdown $f stack %x %y]
	    }
	}
    }
    $canvas bind $id <$fdisp(curbutton)> [list Folder_Change $f]
    $canvas bind $id <$fdisp(tarbutton)> [list Folder_TargetMove $f]
}
proc FdispPopupResetView { can } {
    global fdisp
    set canvas $fdisp($can)
    if {$fdisp(popdownRemove) == "navbutton" || \
        [$canvas canvasy 0] > $fdisp(maxy,$can)} {
	    $canvas yview 0
    }
}
proc FdispPopupView { canvas tag } {
    $canvas raise $tag
    set h [lindex [$canvas configure -height] 4]
    set inc [lindex [$canvas configure -scrollincrement] 4]
    set ybot [$canvas canvasy $h]
    set bbox [$canvas bbox $tag]
    set popbot [lindex $bbox 3]
    if {$popbot <= $ybot} {return}

    # Bottom edge clipped
    set moveup [expr $popbot-$ybot]

    set ytop [$canvas canvasy 0]
    set poptop [lindex $bbox 1]
    set room [expr $poptop-$ytop]
    set moveup [expr {($moveup > $room || $poptop == $inc+1) ? $room-2*$inc : $moveup}]
    $canvas yview [expr 1+(($ytop+$moveup)/$inc)]
}
