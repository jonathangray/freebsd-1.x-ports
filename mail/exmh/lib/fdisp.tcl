#
# fdisp.tcl
#
# Folder display, handling nesting and highlights to reflect folder state.
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Fdisp_Init {} {
    global fdisp mhProfile

    Preferences_Add "Folder Display" \
"These items affect the display of the labels in the folder display window." {
	{fdisp(maxLines) fdispLines 4 {Max fdisp rows}
"The maximum number of rows of folder labels in
the folder display.  If there are more folders than
will fit in this space, the display becomes scrollable." }
	{fdisp(toplevel) fl_toplevel OFF {Detached fdisp display}
"The folder display area can be displayed in a separate
toplevel window.  You can use the *Fltop.position Xresource
to control its initial placement on the screen, and the
*Fltop.Canvas.width and *Fltop.Canvas.height to control
its size."}
	{fdisp(popdownStyle) fdispPopdownStyle {CHOICE polygon rectangle} {Subfolder popdown}
"The style of the sub folder popdown menu
used to display subfolders." }
	{fdisp(popdownAction) fdispPopdownAction {CHOICE navbutton enter redisplay} {Popdown action}
"This determines how the popdown display is triggered:
navbutton - press navigation (middle) button to get the popdown.
enter - move the mouse over the button to get the popdown.
redisplay - do not use popdowns at all.  Instead, navbutton
(middle) causes the whole folder display to change.
The navigation button is settable via an Xresource." }
	{fdisp(popdownRemove) fdispPopdownRemove {CHOICE leave navbutton} {Remove popdown on...}
"This determines what causes a popdown display to be removed:
navbutton - press navigation (middle) button on another label.
leave - leave the area of the popdown.  This actually has to
be implemented by triggering on <Enter> to other labels." }
    }
    # The remaining parameters can be overridden by hand in the user resources

    Preferences_Resource fdisp(font)		fl_font fixed
    Preferences_Resource fdisp(xgap)		fl_xgap 8
    Preferences_Resource fdisp(ygap)		fl_ygap 8
    Preferences_Resource fdisp(curbutton)	fl_curbutton 1
    Preferences_Resource fdisp(navbutton)	fl_navbutton 2
    Preferences_Resource fdisp(tarbutton)	fl_tarbutton 3

    Preferences_Resource fdisp(c_current)	c_current red
    Preferences_Resource fdisp(c_unseen)	c_unseen  blue
    Preferences_Resource fdisp(c_unseenf)	c_unseenf "light blue"
    Preferences_Resource fdisp(c_moved)		c_moved   yellow
    Preferences_Resource fdisp(c_popup)		c_popup   wheat
    Preferences_Resource fdisp(c_fg)		c_foreground black
    Preferences_Resource fdisp(c_bg)		c_background white

    trace variable fdisp(font) w FdispFixupFont
    set fdisp(lastFont) $fdisp(font)
    trace variable fdisp(maxLines) w FdispFixupMaxLines
    set fdisp(lastMaxLines) $fdisp(maxLines)
    trace variable fdisp(toplevel) w FdispFixupToplevel
    set fdisp(lastToplevel) $fdisp(toplevel)

    set fdisp(poptop) -1

}
proc FdispFixupMaxLines { args } {
    global exwin fdisp
    if {$fdisp(maxLines) != $fdisp(lastMaxLines)} {
	set fdisp(width,canvas) 0
	set fdisp(maxLines,$fdisp(canvas)) $fdisp(maxLines)
	set fdisp(lastMaxLines) $fdisp(maxLines)
	set h [expr {$fdisp(maxLines)*($fdisp(itemHeight) + $fdisp(ygap)) + $fdisp(ygap)}]
	$fdisp(canvas) configure -height $h
    }
}
proc FdispFixupToplevel { args } {
    global exwin fdisp tk_version
    if {$fdisp(toplevel) != $fdisp(lastToplevel)} {
	if {$fdisp(toplevel)} {
	    destroy $fdisp(frame)
	    FdispMakeToplevel
	    unset fdisp(frame)
	} else {
	    destroy $fdisp(topWidget)
	    FdispMakeFrame
	    unset fdisp(topWidget)
	}
    }
    set fdisp(lastToplevel) $fdisp(toplevel)
}
proc FdispMakeToplevel { } {
    global fdisp
    Exwin_Toplevel .fl "Folder list" Fltop nomenu
    set fdisp(topWidget) .fl
    wm minsize $fdisp(topWidget) 100 30
    wm protocol .fl WM_DELETE_WINDOW FdispDeleted
    FdispMakeCanvas $fdisp(topWidget)
    set icon [option get $fdisp(topWidget) iconposition IconPosition]
    catch {
	Exwin_IconPosition $fdisp(topWidget) $icon
    }
    set iconic [option get $fdisp(topWidget) iconic Iconic]
    if {$iconic == {}} {
	set iconic $exmh(iconic)
    }
    if {$iconic} {
	wm iconify $fdisp(topWidget)
    }
}
proc Fdisp_Checkpoint { varName } {
    # Add Xresources lines to $varName that save window size
    upvar $varName newstuff
    global fdisp
    catch {
	set width [winfo width $fdisp(topWidget).can]
	set height [winfo height $fdisp(topWidget).can]
	lappend newstuff "*Fltop.Canvas.height:\t$height"
	lappend newstuff "*Fltop.Canvas.width:\t$width"
    }
}
proc FdispDeleted {} {
    after 200 FdispMakeToplevel	;# ha ha
}
proc FdispMakeFrame { } {
    global fdisp
    set fdisp(frame) [Widget_Frame $fdisp(parent) f1 Frame]
    FdispMakeCanvas $fdisp(frame)
}
proc FdispMakeCanvas { frame } {
    global fdisp tk_version exwin
    set fdisp(canvas) [canvas $frame.can -bd 2 -relief raised ]
    set s [scrollbar $frame.sv -command [list $fdisp(canvas) yview]]
    $fdisp(canvas) configure -yscrollcommand [list $s set]
    bind $fdisp(canvas) <2> {%W scan mark %x %y}
    bind $fdisp(canvas) <B2-Motion> {%W scan dragto %x %y}
    if {! $fdisp(toplevel)} {
	if [info exists fdisp(itemHeight)] {
	    set h [expr {$fdisp(maxLines)*($fdisp(itemHeight) + $fdisp(ygap)) + $fdisp(ygap)}]
	    $fdisp(canvas) configure -height $h
	}
    }
    bind $fdisp(canvas) <Configure> FdispCanvasConfigure
    if {$tk_version >= 3.3} {
	pack $s -side $exwin(scrollbarSide) -fill y
	pack $fdisp(canvas) -side $exwin(scrollbarSide) -fill both -expand 1
    } else {
	pack append $frame $s [list $exwin(scrollbarSide) fill] \
		    $fdisp(canvas) [list $exwin(scrollbarSide) expand fill]
    }
}
proc FdispFixupFont { args } {
    global exwin fdisp
    if {$fdisp(lastFont) != $fdisp(font)} {

	# Find out how big labels are
	set id [$fdisp(canvas) create text 0 0 \
		-anchor nw -justify center -text foo -font $fdisp(font)]
	set size [$fdisp(canvas) bbox $id]
	set fdisp(itemHeight) [expr {[lindex $size 3] - [lindex $size 1]}]
	$fdisp(canvas) delete $id
	set fdisp(lastFont) $fdisp(font)

	# Changing canvas size triggers redisplay
	set h [expr {$fdisp(maxLines)*($fdisp(itemHeight) + $fdisp(ygap)) + $fdisp(ygap)}]
	$fdisp(canvas) configure -height $h
	if [info exists fdisp(cache)] {
	    set h [expr {($fdisp(itemHeight) + $fdisp(ygap)) + $fdisp(ygap)}]
	    $fdisp(cache) configure -height $h
	}
    }
}

proc Fdisp_Window { parent } {
    global fdisp exwin

    set fdisp(parent) $parent

    # a bogus child is needed inside fdisp(parent) so it properly
    # shrinks down when the cache is removed or when the main display
    # is moved to a separate top-level
    Widget_Frame $parent bogus Frame

    # The following creates fdisp(canvas), either in a toplevel or a frame
    if {$fdisp(toplevel)} {
	FdispMakeToplevel
     } else {
	FdispMakeFrame
     }

    # Find out how big labels are
    set id [$fdisp(canvas) create text 0 0 \
	    -anchor nw -justify center -text 0123456789 -font $fdisp(font)]
    set size [$fdisp(canvas) bbox $id]
    set fdisp(itemHeight) [expr {[lindex $size 3] - [lindex $size 1]}]
    set fdisp(charWidth) [expr {([lindex $size 2] - [lindex $size 0])/10}]
    $fdisp(canvas) delete $id
    $fdisp(canvas) configure -scrollincrement \
	    [expr {$fdisp(itemHeight)+$fdisp(ygap)+1}]
    if {! $fdisp(toplevel)} {
	set h [expr {$fdisp(maxLines)*($fdisp(itemHeight) + $fdisp(ygap)) + $fdisp(ygap)}]
	$fdisp(canvas) configure -height $h
    }

    global fcache
    if $fcache(enabled) {
	Fcache_CreateWindow
    }

    set fdisp(folder) .
    foreach can {canvas cache} {
	set fdisp(entered,$can) 0		;# Display routine entered
	set fdisp(pending,$can) 0		;# Display routine blocked
	set fdisp(width,$can) 0			;# last display width
	set fdisp(fset,$can) {}			;# last folder set
	set fdisp(cur,$can) {}			;# current folder name
	set fdisp(tar,$can) {}			;# target folder name
	set fdisp(curid,$can) {}		;# canvas item ids
	set fdisp(boxid,$can) {}
	set fdisp(tarid,$can) {}
	set fdisp(tboxid,$can) {}
    }

}
proc Fdisp_Redisplay {} {
    global fdisp
    FdispMain $fdisp(folder) 1
    Fcache_Display 1
}

proc FdispCanvasConfigure {} {
    global fdisp
    FdispMain $fdisp(folder) 1
}

proc FdispMain { {folder {.}} {force 0} } {
    # Layout the current level of folder buttons on the canvas
    global fdisp exmh
    Label_Main [expr {[string compare $folder "."]==0 ? {} : $folder}]
    set fdisp(folder) $folder
    Flist_FindAllFolders
    set folderSet [Flist_FolderSet $folder]
    set len [llength $folderSet]
    set msec [lindex [time [list Fdisp_Layout canvas $folderSet $folder $force]] 0]
    if {$len > 0} {
	Exmh_Debug Fdisp_Layout $msec msec, per-label [expr $msec/$len]
    }
    Exmh_Debug Fdisp_HighlightCanvas [time [list Fdisp_HighlightCanvas canvas]]
}

proc Fdisp_Layout { can folderSet {folder {}} {force 0} } {
    # Main layout routine.  Because this is triggered by
    # <Configure> events, and because it dinks with the
    # size of the canvas, it needs to be reentrant.
    #
    global fdisp

    set canvas $fdisp($can)

    if {$fdisp(entered,$can)} {
	set fdisp(pending,$can) 1
	Exmh_Debug Fdisp_Layout Blocked $can
	return
    }
    set width [winfo width $canvas]

    if {! $force &&
	($width == $fdisp(width,$can)) &&
	($folderSet == $fdisp(fset,$can))} {
	Exmh_Debug Fdisp_Layout Duplicate $can
	if {$fdisp(pending,$can)} {
	    set fdisp(pending,$can) 0
	    after 1 [list Fdisp_Layout $can $folderSet $folder]
	}
	return
    }
    incr fdisp(entered,$can)

    set fdisp(width,$can) $width
    set fdisp(fset,$can) $folderSet

    catch { $canvas delete all }
    if {$can != "cache"} {
	Exmh_Status "Building folder display... $folder"
    }
    set fdisp(maxy,$can) [FdispLayoutInner $can $fdisp(xgap) $fdisp(ygap) \
		    $width $folderSet $folder FdispBindLabel]
    FdispSetCanvasSize $canvas $fdisp(maxy,$can)
    if {$can != "cache"} {
	Exmh_Status ""
    }
    incr fdisp(entered,$can) -1
    if {$fdisp(pending,$can)} {
	set fdisp(pending,$can) 0
	after 1 [list Fdisp_Layout $can $folderSet $folder]
    }
}
proc FdispLayoutInner { can x1 y1 width folderSet folder bindProc {skipSelf 0} {tag _notag_} } {
    global fdisp
    set canvas $fdisp($can)
    set maxy $fdisp(itemHeight)		;# Per row max item height
    set x $x1
    set y $y1
    foreach f $folderSet {
	# Determine label text for the folder
	if {$f == $folder} {
	    if {[string compare $skipSelf "0"] != 0} {
		continue
	    } else {
		set text ".."
	    }
	} else {
	    if {$can == "cache"} {
		set text $f
	    } else {
		set text [file tail $f]
	    }
	}
	# Create the text (or bitmap) at location 0 0
	set id [Fdisp_Label $canvas $f $text]
	set bbox [$canvas bbox $id]
	set twidth [expr [lindex $bbox 2]-[lindex $bbox 0]]
	set theight [expr [lindex $bbox 3]-[lindex $bbox 1]]
	if {$twidth + $fdisp(xgap)/2 + $x > $width} {
	    incr y [expr {$fdisp(ygap) + $maxy}]
	    set x $x1
	    set maxy $fdisp(itemHeight)		;# Per row max item height
	}
	if {$theight > $maxy} {
	    set maxy $theight
	}
	# Move it into position after we see how big it is.
	$canvas move $id $x $y
	incr x [expr {$fdisp(xgap) + $twidth}]

	# Determine style of the box, depending on nesting
	if {$f == $folder} {
	    set ftype goParent
	} else {
	    if [Flist_SubFolders $f] {
		if {[string compare $can "cache"] == 0} {
		    # This supresses the drop-shadow in the cache display,
		    # but also turns off the redisplay mode behavior...
		    set ftype leaf
		} else {
		    set ftype hasNested
		}
	    } else {
		set ftype leaf
	    }
	}
	set box [Fdisp_Box $fdisp($can) $id $ftype $tag]
	FdispUpdateMap $can $f $id
	FdispUpdateBmap $can $f $box
	$bindProc $can $id $ftype $f
    }
    return [expr {$y + $maxy}]
}
proc Fdisp_Label { canvas f text } {
    global fdisp folderInfo
    if [info exists folderInfo(bitmap,$f)] {
	set special 0
	if [info exists folderInfo(fg,$f)] {
	    set fg $folderInfo(fg,$f)
	    set special 1
	} else {
	    set fg black
	}
	if [info exists folderInfo(bg,$f)] {
	    set bg $folderInfo(bg,$f)
	    set special 1
	} else {
	    set bg white
	}
	set id [$canvas create bitmap 0 0 -anchor nw \
		    -bitmap $folderInfo(bitmap,$f) \
		    -foreground $fg -background $bg]
	if {! $special} {
	    $canvas addtag bitmap withtag $id
	} else {
	    lappend folderInfo(special,$canvas) $id
	    set folderInfo(special,$canvas,$id) [list $fg $bg]
	}
    } else {
	set id [$canvas create text 0 0 -anchor nw \
		-justify center -text $text -font $fdisp(font) -tag text]
    }
    return $id
}
proc Fdisp_FixupSpecials { canvas } {
    global folderInfo
    if ![info exists folderInfo(special,$canvas)] {
	return
    }
    foreach id $folderInfo(special,$canvas) {
	if [info exists folderInfo(special,$canvas,$id)] {
	    set fg [lindex $folderInfo(special,$canvas,$id) 0]
	    set bg [lindex $folderInfo(special,$canvas,$id) 1]
	    $canvas itemconfigure $id -background $bg -foreground $fg
	}
    }
}
proc Fdisp_Box { canvas tid ftype {tag {}} } {
    # outline box.  I note that for variable width fonts,
    # the bbox is too long.  Oh well.
    global fdisp

    if {$tag != {}} {
	$canvas addtag $tag withtag $tid
    }

    set bbox [$canvas bbox $tid]
    set x1 [expr {[lindex $bbox 0] - 1}]
    set x2 [expr {[lindex $bbox 2] + 1}]
    set y1 [expr {[lindex $bbox 1] - 1}]
    set y2 [expr {[lindex $bbox 3] + 1}]

    set box [$canvas create rect $x1 $y1 $x2 $y2 -fill $fdisp(c_bg) \
	-tags [list box $tag]]

    # Need one box for a dropshadow, and then one extra box to ensure
    # a stippled foreground obscures the dropshadow box
    if {[string compare $ftype goParent] == 0} {
	$canvas lower [$canvas create rect $x1 $y1 $x2 $y2 -fill $fdisp(c_bg) -tags $tag]
	$canvas lower [$canvas create rect \
	    [expr $x1+3] [expr $y1+3] [expr $x2+3] [expr $y2+3] -fill $fdisp(c_bg) -tags $tag]
    } else {
	if {[string compare $ftype hasNested] == 0} {
	    $canvas lower [$canvas create rect $x1 $y1 $x2 $y2 -fill $fdisp(c_bg) -tags $tag]
	    $canvas lower [$canvas create rect \
		[expr $x1+3] [expr $y1+3] [expr $x2+3] [expr $y2+3] -fill $fdisp(c_fg) -tags $tag]
	}
    }
    $canvas raise $tid	;# display text over top the box
    return $box
}
proc FdispBindLabel { can id ftype f } { 
    global fdisp
    set canvas $fdisp($can)

    $canvas bind $id <$fdisp(curbutton)> [list Folder_Change $f]
    $canvas bind $id <$fdisp(tarbutton)> [list Folder_TargetMove $f]

    if {[string compare $ftype goParent] == 0} {
	$canvas bind $id <$fdisp(navbutton)> \
	    [list FdispMain [file dirname $f]]
    } else {
	if {[string compare $ftype hasNested] == 0} {
	    if {$can != "cache"} {
		case $fdisp(popdownAction) {
		    redisplay {
			$canvas bind $id <$fdisp(navbutton)> \
			    [list FdispMain $f]
		    }
		    enter {
			$canvas bind $id <Any-Enter> \
			    [list FdispDisplayPopdown $f down %x %y]
		    }
		    navbutton {
			$canvas bind $id <$fdisp(navbutton)> \
			    [list FdispDisplayPopdown $f down %x %y]
		    }
		}
	    } else {
		if {$fdisp(popdownAction) == "redisplay"} {
		    $canvas bind $id <$fdisp(navbutton)> \
			    [list FdispMain $f]
		}
	    }
	} else {
	    # Leaf
	    if {$fdisp(popdownAction) == "redisplay"} {
		$canvas bind $id <$fdisp(navbutton)> {}
	    } else {
		if {$fdisp(popdownRemove) == "navbutton"} {
		    $canvas bind $id <$fdisp(navbutton)> \
			[list FdispDisplayPopdown {} remove]
		} else {
		    # Use enter on another leaf label to simulate Leave
		    # of the popdown.  Cannot bind to <Leave> on the popdown
		    # background because that triggers when you enter one
		    # of its own labels.
		    $canvas bind $id <Enter> \
			[list FdispDisplayPopdown {} remove]
		}
	    }
	}
    }
}
proc FdispSetCanvasSize { canvas maxy } {
    global fdisp

    set w [winfo width $canvas]
    set h [expr {$maxy + $fdisp(ygap) + $fdisp(ygap)}]
    $canvas configure -scrollregion [list 0 0 $w $h]
    return $h
}

proc FdispUpdateMap { can folder id } {
    global fdisp
    $fdisp($can) addtag Ftext=$folder withtag $id
}
proc FdispUpdateBmap { can folder box } {
    global fdisp
    $fdisp($can) addtag Fbox=$folder withtag $box
}
proc FdispGetMap { can folder } {
    global fdisp
    return [$fdisp($can) find withtag Ftext=$folder]
}
proc FdispGetBmap { can folder } {
    global fdisp
    return [$fdisp($can) find withtag Fbox=$folder]
}
# Routines to Highlight the folder display

proc Fdisp_ResetHighlights {} {
    global fdisp
    Fdisp_ClearHighlights
    Fdisp_HighlightCanvas canvas
    if [info exists fdisp(cache)] {
	Fdisp_HighlightCanvas cache
    }
}
proc Fdisp_ClearHighlights {} {
    global fdisp
    FdispClearHighlights $fdisp(canvas)
    if [info exists fdisp(cache)] {
	FdispClearHighlights $fdisp(cache)
    }
}

proc Fdisp_HighlightCanvas { can } {
    global fdisp flist
    if ![info exist fdisp($can)] {
	return
    }
    if {$fdisp(cur,$can) != {}} {
	FdispHighlightCur $can $fdisp(cur,$can)
    }
    if {$fdisp(tar,$can) != {}} {
	FdispHighlightTarget $can $fdisp(tar,$can)
    }
    foreach f [Flist_UnseenFolders] {
	FdispHighlightUnseen $can $f
    }
    Fdisp_LabelConfigure $fdisp($can)
}

proc FdispWhichLabel { can f } {
    # Figure out what label to highlight, handling nesting
    global fdisp mhProfile

    set map [FdispGetMap $can $f]
    if {$map != {}} {
	return [expr {($f == "..") ? {} : $f}]
    }
    while {($f != ".") && ($f != "/")} {
	set nf [file dirname $f]
	if {$nf == $f} {
	    break
	}
	set f $nf
	set map [FdispGetMap $can $f]
	if {$map != {}} {
	    return [expr {($f == "..") ? {} : $f}]
	}
    }
    return {}
}
proc FdispAllLabels { can f } {
    # Figure out what labels to highlight, returning
    # multiple labels if they are present because of popdowns.
    global fdisp mhProfile

    set res {}
    set map [FdispGetMap $can $f]
    if {($map != {}) && ($f != "..")} {
	lappend res $f
    }
    while {($f != ".") && ($f != "/")} {
	set nf [file dirname $f]
	if {$nf == $f} {
	    break
	}
	set f $nf
	set map [FdispGetMap $can $f]
	if {($map != {}) && ($f != "..")} {
	    lappend res $f
	}
    }
    return $res
}
proc Fdisp_HighlightCur { f } {
    global fdisp

    Fcache_Folder $f
    foreach can {canvas cache} {
	if [info exists fdisp($can)] {
	    FdispHighlightCur $can $f
	    Fdisp_LabelConfigure $fdisp($can)
	}
    }
}
proc FdispHighlightCur { can f } {
    global fdisp
    set l [FdispWhichLabel $can $f]
    set canvas $fdisp($can)
    if {$fdisp(curid,$can) != {}} {
	$canvas dtag $fdisp(curid,$can) cur[$canvas type $fdisp(curid,$can)]
	$canvas dtag $fdisp(boxid,$can) curbox
    }

    set fdisp(cur,$can) $f
    Exmh_Debug FdispHighlightCur $can $f $l
    if {$l != {}} {
	set id [FdispGetMap $can $l]
	set box [FdispGetBmap $can $l]
	$canvas addtag cur[$canvas type $id] withtag $id
	$canvas addtag curbox withtag $box
	set fdisp(curid,$can) $id
	set fdisp(boxid,$can) $box
    }
}
proc Fdisp_HighlightTarget { f } {
    global fdisp
    Fcache_Folder $f
    foreach can {canvas cache} {
	if [info exists fdisp($can)] {
	    FdispHighlightTarget $can $f
	    Fdisp_LabelConfigure $fdisp($can)
	}
    }
}
proc FdispHighlightTarget { can f } {
    global fdisp
    set l [FdispWhichLabel $can $f]
    set canvas $fdisp($can)
    if {$fdisp(tarid,$can) != {}} {
	$canvas dtag $fdisp(tarid,$can) tar[$canvas type $fdisp(tarid,$can)]
	$canvas dtag $fdisp(tboxid,$can) tarbox
    }

    set fdisp(tar,$can) $f
    Exmh_Debug FdispHighlightTarget $can $f $l
    if {$l != {}} {
	set id [FdispGetMap $can $l]
	set box [FdispGetBmap $can $l]
	$canvas addtag tar[$canvas type $id] withtag $id
	$canvas addtag tarbox withtag $box
	set fdisp(tarid,$can) $id
	set fdisp(tboxid,$can) $box
    }
}

proc Fdisp_HighlightUnseen { f } {
    global fdisp
    foreach can {canvas cache} {
	if [info exists fdisp($can)] {
	    FdispHighlightUnseen $can $f
	    Fdisp_LabelConfigure $fdisp($can)
	}
    }
}
proc FdispHighlightUnseen { can f } {
    global exmh fdisp
    set ll [FdispAllLabels $can $f]
    set canvas $fdisp($can)
    Exmh_Debug FdispHighlightUnseen $can $f $ll
    foreach l $ll {
	set id [FdispGetMap $can $l]
	set box [FdispGetBmap $can $l]
	$canvas addtag leaf=$f withtag $id
	$canvas addtag unsn[$canvas type $id] withtag $id
	$canvas addtag unsnbox withtag $box
    }
}
proc Fdisp_UnHighlightUnseen { f } {
    global fdisp
    foreach can {canvas cache} {
	if [info exists fdisp($can)] {
	    FdispUnHighlightUnseen $fdisp($can) $can $f
	    Fdisp_LabelConfigure $fdisp($can)
	}
    }
}
proc FdispUnHighlightUnseen { canvas can f } {
    global exmh fdisp
    set ll [FdispAllLabels $can $f]
    set canvas $fdisp($can)
    Exmh_Debug FdispUnHighlightUnseen $can $f $ll
    foreach l $ll {
	set id [FdispGetMap $can $l]
	set box [FdispGetBmap $can $l]
	set stillLight 0
	foreach tag [$canvas gettags $id] {
	    if [string match leaf=* $tag] {
		set leaf [lindex [split $tag =] 1]
		if {$leaf == $f} {
		    $canvas dtag $id $tag
		} else {
		    set stillLight 1
		}
	    }
	}
	if {! $stillLight} {
	    $canvas dtag $id unsn[$canvas type $id]
	    $canvas dtag $box unsnbox
	}
    }
}
