# exwin.tcl
#
# Main window layout for the application
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Exwin_Init {} {
    global exwin
    Preferences_Resource exwin(mtextLines) mtextLines 25
    Preferences_Add "Window Stuff" \
"Unfortunatly you cannot directly change the size of interior windows, so you have to pick the size of the folder display via this interface.  To change the size of the message display, just stretch the main window and exmh will remember that size.  The scrollbar side can be \"left\" or \"right\"." {
	{exwin(scrollbarSide) scrollbarSide right {Scrollbar side}
"Which side the scrollbars appear on.  This
only takes effect after you restart exmh.
Set to \"left\" or \"right\"."}
	{exwin(scrollSpeed) scrollSpeed 1 {Drag-Scroll speed}
"How fast things scroll when you drag a text widget
around with the (by default) middle button."}
	{exwin(scrollAccel) scrollAccel 4 {Drag-Scroll acceleration}
"How fast things scroll when you drag a text widget
around with the (by default) middle button with Shift depressed."}
	{exwin(ftextLines)	ftextLines 15	{Folder lines}
"Lines in the folder display window."}
	{exwin(placeToplevel)	placeToplevel ON	{Remember window placement}
"With this enabled, exmh will remember the placement of the various
popup windows between sessions.  This means you can position them
once manually and they will always appear there.  However, if you
use a virtual root window manager and run exmh in different \"rooms\"
then a remembered placement might be in the wrong room.  You can nuke
all the placement memory from the end of the .exmh-defaults file and
always run exmh from the same room, or just disable this feature.."}
    }

    trace variable exwin(ftextLines) w ExwinFixupFtextLines
#    trace variable exwin(mtextLines) w ExwinFixupMtextLines
    if ![info exists exwin(toplevels)] {
	set exwin(toplevels) [option get . exwinPaths {}]
    }
}
proc ExwinFixupFtextLines { args } {
    global exwin
    Exmh_Debug ExwinFixupFtextLines $exwin(ftextLines)
    $exwin(ftext) configure -height $exwin(ftextLines)
}
#proc ExwinFixupMtextLines { args } {
#    global exwin
#    Exmh_Debug ExwinFixupMtextLines $exwin(mtextLines)
#    $exwin(mtext) configure -height $exwin(mtextLines)
#}

proc Exwin_Layout {} {
    global exwin exmh

    set fixed {top fill}
    set expand {top fill expand}

    wm minsize . 100 100
    Flag_Init

    # Top row of buttons for global ops and version string
    set exwin(mainButtons) [Widget_Frame . main Main $fixed]
    Buttons_Main $exwin(mainButtons)
    Label_MainSetup $exwin(mainButtons)

    # Folders with unread messages
    Fdisp_Window [Widget_Frame . flist Fdisp $fixed]

    # Second row of buttons for folder ops and current folder label
    set exwin(fopButtons) [Widget_Frame . fops Fops $fixed]
    Buttons_Folder $exwin(fopButtons)
    Label_FolderSetup $exwin(fopButtons)

    # Folder display (Ftoc)
    set exwin(ftext) [Widget_Text [Widget_Frame . ftoc Ftoc $fixed] \
				$exwin(ftextLines) -cursor hand2 -wrap none]
    Ftoc_Bindings $exwin(ftext)
    Ftoc_ColorConfigure $exwin(ftext)

    # Frame for faces, status, message buttons
    set mid     [Widget_Frame . mid Mid $fixed]
    Widget_SplitFrameR $mid Face Right
    set face $mid.left
    canvas $face.c -background white -width 48 -height 48
    set exwin(faceCanvas) $face.c
    # default image
    $face.c create bitmap 0 0 -anchor nw -bitmap @$exmh(library)/exmh.bitmap
    pack append $face $face.c {top}

    # Status line + MsgID
    set right $mid.right
    Widget_SplitFrameV $right Status Mops
    set exwin(status) [Widget_Entry $right.top msg {right expand fill}]
    Label_MessageSetup $right.top
    # Status line does double-duty for folder/msg selection typein
    Select_EntryBind $exwin(status)

    # Buttons for message ops, plus display of current message id
    set exwin(mopButtons) $right.bot
    Buttons_Message $exwin(mopButtons)

    # Message display
    set exwin(mtext) [Widget_Text [Widget_Frame . msg Msg $expand] \
				$exwin(mtextLines)]
    Msg_Setup $exwin(mtext)
    Bindings_Main $exwin(mtext)
    focus $exwin(mtext)
}
proc Exwin_FullFtoc {} {
    global exwin
    global tk_version
    if ![info exists exwin(fullFtoc)] {
	set exwin(fullFtoc) notFullScreen
    }
    if {$exwin(fullFtoc) == "notFullScreen"} {
	set exwin(fullFtoc) fullScreen
	if {$tk_version >= 3.3} {
	    set exwin(ftocPack) [pack newinfo .msg]
	    pack forget .msg
	    $exwin(ftext) configure -height \
		[expr $exwin(ftextLines)+$exwin(mtextLines)]
	} else {
	    set exwin(ftocPack) {top fill expand}
	    pack unpack .msg
	}
    } else {
	set exwin(fullFtoc) notFullScreen
	$exwin(ftext) configure -height $exwin(ftextLines)
	if {$tk_version >= 3.3} {
	    eval pack .msg $exwin(ftocPack)
	} else {
	    pack append . .msg $exwin(ftocPack)
	}

    }
}

proc Exwin_IconPosition { w icon } {
    # icon looks like +x+y, or -x-y, etc.
    set x 0 ; set y 0
    if {[llength $icon] == 1} {
	if [regexp {([\+-])([0-9]+)([\+-])([0-9]+)} $icon match s1 x s2 y] {
	    if {$s1 == "-"} {
		set x -$x
	    }
	    if {$s2 == "-"} {
		set y -$y
	    }
	}
    } else {
	set x [lindex $icon 0]
	set y [lindex $icon 1]
    }
    if {($x < 0) || ([string compare $x "-0"] == 0)} {
	# 48 depends on icon width
	set x [expr [winfo screenwidth $w]+$x-48]
    }
    if {($y < 0) || ([string compare $y "-0"] == 0)} {
	# 64 depends on icon height
	set y [expr [winfo screenheight $w]+$y-64]
    }
    if [catch {wm iconposition $w $x $y} err] {
	puts stderr "wm iconposition $w $x $y: $err"
    }
}

proc Exwin_Toplevel { path name {class Dialog} {dismiss yes}} {
    global exwin
    if [catch {wm state $path} state] {
	set t [Widget_Toplevel $path $name $class]
	if ![info exists exwin(toplevels)] {
	    set exwin(toplevels) [option get . exwinPaths {}]
	}
	set ix [lsearch $exwin(toplevels) $t]
	if {$ix < 0} {
	    lappend exwin(toplevels) $t
	}
	if {$dismiss == "yes"} {
	    set f [Widget_Frame $t but Menubar {top fill}]
	    Widget_AddBut $f quit "Dismiss" [list Exwin_Dismiss $path]
	}
	return 1
    } else {
	if {$state != "normal"} {
	    catch {
		wm geometry $path $exwin(geometry,$path)
		Exmh_Debug Exwin_Toplevel $path $exwin(geometry,$path)
	    }
	    wm deiconify $path
	} else {
	    catch {raise $path}
	}
	return 0
    }
}
proc Exwin_Dismiss { path {geo ok} } {
    global exwin
    case $geo {
	"ok" {
	    set exwin(geometry,$path) [wm geometry $path]
	}
	"nosize" {
	    set exwin(geometry,$path) [string trimleft [wm geometry $path] 0123456789x]
	}
	default {
	    catch {unset exwin(geometry,$path)}
	}
    }
    wm withdraw $path
    Exmh_Focus    
}
proc Exwin_CheckPoint { } {
    global exwin
    if {! $exwin(placeToplevel)} {
	Preferences_RewriteSection "Saved Window Positions" "End Positions" {}
	return
    }
    set oldstuff [Preferences_ReadSection "Saved Window Positions" "End Positions"]
    set newstuff [list [format "*exwinPaths:\t%s" $exwin(toplevels)]]
    foreach path $exwin(toplevels) {
	set npath [string trimleft $path .]
	if [catch {wm state $path} state] {
	    # No widget - retrieve from old values, if possible
	    set geo {}
	    foreach item $oldstuff {
		if [regexp ^\\*$npath\\.position: $item] {
		    set geo [lindex $item 1]
		    break
		}
	    }
	} else {
	    case $state {
		"normal" {set geo [wm geometry $path]}
		default {
		    if [info exists exwin(geometry,$path)] {
			set geo $exwin(geometry,$path)
		    } else {
			set geo [option get $path position Position]
			if {$geo == {}} {
			    set geo [wm geometry $path]
			}
		    }
		}
	    }
	}
	lappend newstuff [format "*%s.position:\t%s" $npath \
			[string trimleft $geo -x0123456789]]
    }
    lappend newstuff [format "%s.geometry:\t%s" [winfo name .] [wm geometry .]]
    Fdisp_Checkpoint newstuff
    Preferences_RewriteSection "Saved Window Positions" "End Positions" $newstuff
}
proc Exwin_ClearCheckPoint {} {
    Preferences_RewriteSection "Saved Window Positions" "End Positions" {}
}
