# widgetText.tcl
#
# Layer over the TK text widget that provides contrained scrolling
# and 1-to-1 draging.
#
# Based on code contributed by John Robert Loverso
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Widget_TextInit {} {
    Preferences_Add "Text Widget" \
"These items have to do with Text scrolling.
You'll also want do open the Bind dialog in order
to customize the keystroke bindings." {
    {widgetText(constrained) textConstrainScroll OFF "Constrained Scrolling"
"Constrained scrolling clamps the last line
of text to the bottom of the text widget."} 
    {widgetText(constrainFtoc) textConstrainFtoc ON "Constrain FTOC"
"In spite of the general constrained text setting,
constrain the scrolling of the folder table-of-contents."}
    {widgetText(scrollContext) textContextLines 2 "Scroll Context"
"Scroll context is the number of lines that pages
overlap when paging up and down through text."}
    }
}
proc Widget_TextInitText { t } {
    global widgetText		;# Constrained scrolling module
    set widgetText($t,geo) {}
    set widgetText($t,extend) 0
    if ![info exists widgetText(selectDelay)] {
	set widgetText(selectDelay) 4
    }
    global TextType		;# Text bindings module
    set TextType($t) text
    selection handle $t [list Text_HandleSelRequest $t]

}
proc Widget_Text { frame height args } {
    # Create the text widget used to display messages
    global exwin
    if ![info exists exwin(scrollbarSide)] {
	set side right
    } else {
	set side $exwin(scrollbarSide)
    }
    set cmd [list text $frame.t -relief raised -bd 2 \
		-yscroll [list WidgetScrollSet $frame.sv $frame.t] \
		-wrap word]
    if [catch [concat $cmd $args] t] {
	puts stderr "Widget_Text (warning) $t"
	set t [eval $cmd $args {-font fixed}]
    }
    if {[option get $frame.t width Width] == {}} {
	$frame.t configure -width 80
    }
    if {[option get $frame.t height Height] == {}} {
	$frame.t configure -height $height
    }
    scrollbar $frame.sv -command [list WidgetTextYview $t]
    pack append $frame $frame.sv [list $side filly] $t {expand fill}
    $t mark set insert 0.0

    Widget_TextInitText $t	;# Init state variables

    if [regexp {setgrid} $args] {
	wm minsize [winfo toplevel $frame] 10 1
    }

    return $t
}

# For Keyboard scrolling

proc Widget_TextPageOrNext { t } {
    global widgetText
    set bottom [lindex $widgetText($t,view) 3]
    set end [expr [$t index end]-1]
    if {$bottom >= $end} {
	Ftoc_Next show implied
    } else {
	Widget_TextPageDown $t
    }
}

proc Widget_TextPageDown { t } {
    global widgetText
    Exmh_Debug WidgetTextPageDown $t
    if [info exists widgetText($t,view)] {
	set bottom [lindex $widgetText($t,view) 3]
	WidgetTextYview $t [expr $bottom-$widgetText(scrollContext)]
    }
}
proc Widget_TextPageUp { t } {
    global widgetText
    Exmh_Debug WidgetTextPageUp $t
    if [info exists widgetText($t,view)] {
	set top [lindex $widgetText($t,view) 2]
	set hei [lindex $widgetText($t,view) 1]
	WidgetTextYview $t [expr $top-$hei+$widgetText(scrollContext)]
    }
}

proc Widget_TextLineDown { t } {
    global widgetText
    Exmh_Debug WidgetTextLineDown $t
    if [info exists widgetText($t,view)] {
        set top [lindex $widgetText($t,view) 2]
        WidgetTextYview $t [expr $top+1]
    }
}

proc Widget_TextLineUp { t } {
    global widgetText
    Exmh_Debug WidgetTextLineUp $t
    if [info exists widgetText($t,view)] {
        set top [lindex $widgetText($t,view) 2]
        WidgetTextYview $t [expr $top-1]
    }
}

# Constrained scrolling

proc WidgetTextYview { t args } {
    global widgetText
    if {!$widgetText(constrained) &&
	!($widgetText(constrainFtoc) && [string match *.ftoc.* $t])} {
	eval {$t yview} $args
	return
    }
    set mark $args
    if {([llength $args] == 1) && ([scan $args %d line] == 1)} {
	if {[string compare $line $args] == 0} {
	    # Being invoked as a scrollcommand, in which lines are
	    # counted from 0.  incr to get back to mark coordinates.
	    incr line
	    set mark $line.0
	}
    }
    if {[lindex $args 0] == "-pickplace"} {
	set pick -pickplace
	set mark [lrange $args 1 end]
    } else {
	set pick {}
    }
    if [$t compare $mark > end] {
	set mark end
    }
    eval {$t yview} $pick {$mark}
    set height [lindex [split [winfo geometry $t] +x] 1]
    set bot [$t index @0,$height]
    set end [$t index end]
    if {$bot != $end} {
	return
    }
    set max [lindex [$t config -height] 4]
    set i 0
    while {$bot == $end} {
	set mark [$t index [list $mark -1 lines]]
	$t yview $mark
	set bot [$t index @0,$height]
	incr i
	if {$i > $max} {
	    return	;# message smaller than window
	}
    }
    set mark [$t index [list $mark +1 lines]]
    $t yview $mark
}
proc WidgetScrollSet { s t args } {
    global widgetText
    set widgetText($t,view) $args
    if {$s != {}} {
	eval {$s set} $args
    }
}
proc WidgetTextMark { t y } {
    global widgetText
    set widgetText($t,mark) $y			;# Remember mark point
    scan [$t index @1,1] %d widgetText($t,top)	;# and starting top line
}
proc WidgetTextDragto { t y speed } {
    global widgetText
    if ![info exists widgetText($t,mark)] {
	return
    }
    if {$y == $widgetText($t,mark)} {
	return
    }
    set gridy [WidgetTextGridY $t]
    set dy [expr {($widgetText($t,mark)-$y)*$speed}]
    set dlines [expr $dy/$gridy]
    set rem [expr $dy%$gridy]
    if {$dy < 0} {
	# observe that -3/12 = -1 under the new rounding rules...
	global tk_version
	if {$tk_version >= 3.3} {
	    incr dlines
	    set rem [expr $rem-$gridy]
	}
    }
    if {$dlines >= 1.0 || $dlines <= -1.0} {
	set widgetText($t,mark) [expr $y+$rem]
	set newtopline [expr $widgetText($t,top)+$dlines]
	WidgetTextYview $t $newtopline.0
	set widgetText($t,top) $newtopline
    }
}
proc WidgetTextGridY { t } {
    global widgetText
    set geo [split [winfo geometry $t] +x]
    if { [string compare $geo $widgetText($t,geo)] != 0 } {
	# Reverse engineer grid size - broken for windows that get resized.
	set widgetText($t,geo) $geo
	set h [lindex $geo 1]
	set nlines [lindex [$t config -height] 4]
	set widgetText($t,gridY) [expr $h/$nlines]
	Exmh_Debug widgetText($t,gridY) $widgetText($t,gridY)
    }
    return $widgetText($t,gridY)
}

# Text selection, with power-drag-scroll when mouse leaves the window

proc WidgetTextSelMotion { w x y } {
    global widgetText

    set active $widgetText($w,extend)

    set h [winfo height $w]
    if {$y > $h} {
	set widgetText($w,extend) [expr $y-$h]
    } else {
	if {$y < 0} {
	    set widgetText($w,extend) $y
	} else {
	    set widgetText($w,extend) 0
	}
    }
    
    if {$widgetText($w,extend) == 0} {
#	tk_textSelectTo $w @$x,$y
	Text_SelectTo $w @$x,$y
    } else {
	if {! $active} {
	    set widgetText($w,lastmark) [$w index @$x,$y]
	    after $widgetText(selectDelay) [list WidgetTextSelExtend $w]
	}
    }
}

proc WidgetTextSelExtend { w } {
    global widgetText

    if {$widgetText($w,extend) != 0} {
	set delta [expr {$widgetText($w,extend) / 16}]
	if {$delta == 0} {
	    set delta [expr { ($widgetText($w,extend) < 0) ? -1 : 1 }]
	}
	set sign [expr {($delta > 0) ? "+" : ""}]
	catch {
	    if [$w compare $widgetText($w,lastmark) <= sel.first] {
		set mark "sel.first $sign $delta lines"
	    } else {
		set mark "sel.last $sign $delta lines"
	    }
	    set widgetText($w,lastmark) [$w index $mark]
#	    tk_textSelectTo $w $mark
	    Text_SelectTo $w $mark
	    $w yview -pickplace $mark
	    after $widgetText(selectDelay) [list WidgetTextSelExtend $w]
	}
    }
}

proc WidgetTextSelDone { w } {
    global widgetText
    set widgetText($w,extend) 0
    Text_SelectionEnd $w 1
}

