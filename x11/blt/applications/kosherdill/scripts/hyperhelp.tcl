#
# Hyperhelp
# ----------------------------------------------------------------------
# Utility calls to implement a hypertext help function
#
#   AUTHOR:  Michael J. McLennan       Phone: (215)770-2842
#            AT&T Bell Laboratories   E-mail: michael.mclennan@att.com
#
#      RCS:  hyperhelp.tcl,v 1.2 1994/01/04 16:09:39 mmc Exp
# ----------------------------------------------------------------------
#                 Copyright (c) 1993  AT&T Bell Laboratories
# ======================================================================
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted,
# provided that the above copyright notice appear in all copies and that
# both that the copyright notice and warranty disclaimer appear in
# supporting documentation, and that the names of AT&T Bell Laboratories
# any of their entities not be used in advertising or publicity
# pertaining to distribution of the software without specific, written
# prior permission.
#
# AT&T disclaims all warranties with regard to this software, including
# all implied warranties of merchantability and fitness.  In no event
# shall AT&T be liable for any special, indirect or consequential
# damages or any damages whatsoever resulting from loss of use, data or
# profits, whether in an action of contract, negligence or other
# tortuous action, arising out of or in connection with the use or
# performance of this software.
# ======================================================================
#
# USAGE:
#   Each application must register its intent to use HyperHelp by
#   invoking "hyperhelp_init" near the start of the application:
#
#     hyperhelp_init <application-name>
#
#   Thereafter, help files are displayed using one of the following
#   calls:
#
#     hyperhelp_file <file> [ <line> ]  <== call to display info in file
#     hyperhelp_mesg <mesg> [ <line> ]  <== call to display string info
#
# HYPERLINKS:
#   To handle hypertext jumping between help files, edit the help
#   files and embed commands of the form:
#
#      %% hyperhelp_link <label> <jumpFile> [<jumpMarker>] %%
#
#   For example:
#
#      Pop-up menus can be implemented using the %%
#        hyperhelp_link "menu" Doc/Menu/Overview
#      %% command.
#      Given a few basic arguments, this command will
#      create a pop-up menu that can be attached to a
#      button using the %%
#        hyperhelp_link "menubutton" Doc/Menu/Menubuttons MenuIntro
#      %% command.
#
#   The optional parameter <jumpMarker> is a string refering to a
#   marker declared within the <jumpFile>.  For example, the file
#   "Doc/Menu/Menubuttons" might look like this:
#
#      blah blah blah...
#      .
#      .
#      %% hyperhelp_mark MenuIntro %%
#      INTRODUCTION TO MENUS
#        Menus are really quite simple when you get the
#        hang of them.  Consider the example below...
#
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
#  X-DEFAULTS
# ----------------------------------------------------------------------
set class [winfo class .]
option add $class*hyperhelp.view.file.height \
	4i startupFile
option add $class*hyperhelp.view.file*Font \
	-*-lucidatypewriter-bold-r-normal-*-*-120-*-*-*-*-*-* startupFile
option add $class*hyperhelp.view.file.cursor \
	dot startupFile
option add $class*hyperhelp.view.file*Button*Pad \
	0 startupFile

option add $class*hyperhelp*topics*Button.relief \
	flat startupFile

if {[winfo screenvisual .] == "staticgray"} {
	option add $class*hyperhelp.view.file*Button*foreground \
		white startupFile
	option add $class*hyperhelp.view.file*Button*background \
		black startupFile
	option add $class*hyperhelp.view.file*Button*activeForeground \
		black startupFile
	option add $class*hyperhelp.view.file*Button*activeBackground \
		white startupFile
	option add $class*hyperhelp.view.file.background \
		white startupFile
} else {
	option add $class*hyperhelp.view.file*Button*foreground \
		white startupFile
	option add $class*hyperhelp.view.file*Button*background \
		maroon startupFile
	option add $class*hyperhelp.view.file*Button*activeForeground \
		white startupFile
	option add $class*hyperhelp.view.file*Button*activeBackground \
		red startupFile
	option add $class*hyperhelp.view.file.background \
		white startupFile
}

bind Blt_htext <ButtonPress-2> {%W scan mark 0 %y}
bind Blt_htext <B2-Motion> {%W scan dragto 0 %y}

# ----------------------------------------------------------------------
#  BITMAPS
# ----------------------------------------------------------------------
blt_bitmap define hyperhelp-logo { {31 31} {
   0x00, 0x00, 0x00, 0x00, 0x00, 0xe0, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0xff, 0x7f, 0x00, 0x00, 0x00, 0x00, 0x00, 0xe0, 0xe7, 0xf3, 0x03,
   0x00, 0xf8, 0x0f, 0x00, 0xf0, 0xfd, 0xdf, 0x07, 0x00, 0xfe, 0x3f, 0x00,
   0x78, 0xff, 0x7f, 0x0f, 0x00, 0x3f, 0x7e, 0x00, 0x7c, 0xdf, 0x7d, 0x1f,
   0x00, 0x1f, 0x7c, 0x00, 0x7e, 0xdf, 0x7d, 0x3f, 0x00, 0x00, 0x7e, 0x00,
   0xfe, 0x3f, 0xbf, 0x3f, 0x00, 0x80, 0x1f, 0x00, 0xfe, 0xdf, 0xef, 0x3f,
   0x00, 0xe0, 0x07, 0x00, 0xfc, 0xef, 0xf7, 0x1f, 0x00, 0xe0, 0x07, 0x00,
   0xf8, 0xef, 0xf7, 0x0f, 0x00, 0x00, 0x00, 0x00, 0xf0, 0xef, 0xf7, 0x07,
   0x00, 0xe0, 0x07, 0x00, 0xe0, 0xef, 0xf7, 0x03, 0x00, 0x00, 0x00, 0x00,
   0x00, 0xff, 0x7f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xe0, 0x03, 0x00,
   0x00, 0x00, 0x00, 0x00
} }

blt_bitmap define hyperhelp-fullpage { {15 15} {
   0x00, 0x00, 0xff, 0x0f, 0x01, 0x08, 0x7d, 0x09, 0x01, 0x08, 0xed, 0x59,
   0x01, 0x28, 0xbd, 0x5b, 0x01, 0x28, 0x7d, 0x5b, 0x01, 0x08, 0xbd, 0x09,
   0x01, 0x08, 0xff, 0x0f, 0x00, 0x00
} }

blt_bitmap define hyperhelp-emptypage { {15 15} {
   0x00, 0x00, 0xff, 0x0f, 0x01, 0x08, 0x01, 0x08, 0x01, 0x08, 0x01, 0x58,
   0x01, 0x28, 0x01, 0x58, 0x01, 0x28, 0x01, 0x58, 0x01, 0x08, 0x01, 0x08,
   0x01, 0x08, 0xff, 0x0f, 0x00, 0x00
} }

# ----------------------------------------------------------------------
#  USAGE: hyperhelp_init <application-name> <help-dir>
#
#  Creates the HyperHelp display window and initializes HyperHelp
#  variables.  Should be invoked at the start of each application
#  to set up for HyperHelp support.
# ----------------------------------------------------------------------
proc hyperhelp_init {appName helpdir} {
	if {[file readable $helpdir] && [file isdirectory $helpdir]} {
		hyperhelp_path $helpdir
	} else {
		error "invalid help directory: $helpdir"
	}

	toplevel .hyperhelp
	frame .hyperhelp.panel
	label .hyperhelp.panel.icon -bitmap hyperhelp-logo -padx 3 -pady 3
	pack .hyperhelp.panel.icon -side left -padx 4 -pady 4

	label .hyperhelp.panel.label -text "HyperHelp Topics: "
	pack .hyperhelp.panel.label -side left
	frame .hyperhelp.panel.topics
	pack .hyperhelp.panel.topics -side left -expand yes -fill x

	frame .hyperhelp.cntl -borderwidth 1 -relief raised
	button .hyperhelp.cntl.back -text " Go Back " \
		-command "hyperhelp_discard"
	frame .hyperhelp.cntl.d -borderwidth 2 -relief sunken
	button .hyperhelp.cntl.d.done -text " Dismiss " \
		-command "hyperhelp_close"
	pack .hyperhelp.cntl.d.done -side top -padx 4 -pady 4

	blt_table .hyperhelp.cntl \
		.hyperhelp.cntl.back 0,0 -anchor c -padx 8 -pady 8 \
		.hyperhelp.cntl.d 0,1 -anchor c -padx 8 -pady 8

	bind .hyperhelp <Key-Return> {
		.hyperhelp.cntl.d.done flash
		.hyperhelp.cntl.d.done invoke
	}

	frame .hyperhelp.view -borderwidth 2 -relief sunken
	scrollbar .hyperhelp.view.sbar -command ".hyperhelp.view.file yview" \
		-orient vertical
	blt_htext .hyperhelp.view.file -yscrollcommand ".hyperhelp.view.sbar set"
	pack .hyperhelp.view.sbar -side right -fill y
	pack .hyperhelp.view.file -side top -expand yes -fill both

	pack .hyperhelp.panel -side top -fill x
	pack .hyperhelp.view -side top -expand yes -fill both -padx 4 -pady 4
	pack .hyperhelp.cntl -side bottom -fill x

	wm title .hyperhelp "$appName: HyperHelp"
	wm withdraw .hyperhelp
}

# ----------------------------------------------------------------------
#  USAGE: hyperhelp_file <file> [<jumpMarker>]
#
#  This command loads a file into the list of help information
#  displays.  An optional <jumpMarker> can be specified, causing
#  the help display to jump to that position in the file whenever
#  this bit of help information is displayed.
# ----------------------------------------------------------------------
proc hyperhelp_file {file args} {
	global hhPlace

	if {[llength $args] >= 1} {
		set jumpMarker [lindex $args 0]
	} else {
		set jumpMarker null
	}
	hyperhelp_open
	hyperhelp_insert end -file [hyperhelp_find $file] $jumpMarker
	hyperhelp_display [expr $hhPlace+1]
}

# ----------------------------------------------------------------------
#  USAGE: hyperhelp_mesg <mesg> [<jumpMarker>]
#
#  This command loads a string message into the list of help
#  information displays.  An optional <jumpMarker> can be specified,
#  causing the help display to jump to that position in the message
#  whenever this bit of help information is displayed.
# ----------------------------------------------------------------------
proc hyperhelp_mesg {mesg args} {
	global hhPlace

	if {[llength $args] >= 1} {
		set jumpMarker [lindex $args 0]
	} else {
		set jumpMarker null
	}

	hyperhelp_open
	hyperhelp_insert end -text "$mesg" $jumpMarker
	hyperhelp_display [expr $hhPlace+1]
}

# ----------------------------------------------------------------------
#  USAGE: hyperhelp_link <label> <jumpFile> [<jumpMarker>]
#
#  Implements a hypertext link to load and display a new file.
#  This command is normally embedded in a help information file,
#  set off by %% delimiters:
#
#     EXAMPLE HELP FILE:
#
#       Pop-up menus can be implemented using the %%
#         hyperhelp_link "menu" Doc/Menu/Overview
#       %% command.
#       Given a few basic arguments, this command will ...
#
#  NOTE:  If the <label> begins with "@" then it is interpreted
#         as the name of a bitmap image.
#
# ----------------------------------------------------------------------
proc hyperhelp_link {label filename args} {
	global blt_htext hhCount

	if {$args != ""} {
		set jumpMarker $args
	} else {
		set jumpMarker null
	}
	set jumpFile [hyperhelp_find $filename]

	if {$jumpFile == ""} then {
		puts stderr "
  / \\   HyperHelp:  HELP DOCUMENTATION ERROR
 / ! \\  //        TROUBLE:  hyperhelp_link file not found
 -----  //    FILE NEEDED:  $filename
        //  REFERENCED BY:  $blt_htext(file) line $blt_htext(line)
"
	}

	if {[string index $label 0] == "@"} {
		button .hyperhelp.view.file.$hhCount -bitmap $label \
			-command "hyperhelp_jump -file $jumpFile $jumpMarker"
	} else {
		button .hyperhelp.view.file.$hhCount -text "$label" \
			-command "hyperhelp_jump -file $jumpFile $jumpMarker"
	}
	.hyperhelp.view.file append .hyperhelp.view.file.$hhCount

	incr hhCount
}

# ----------------------------------------------------------------------
#  USAGE: hyperhelp_mark <marker>
#
#  This command associates the <marker> string with the current
#  position in a help file.  It is used to define the <jumpMarker>
#  positions referred to by the "hyperhelp_link" command.
# ----------------------------------------------------------------------
proc hyperhelp_mark {marker} {
	global blt_htext hhLinks

	set hhLinks($marker) $blt_htext(line)
}

# ----------------------------------------------------------------------
#  USAGE: hyperhelp_path <dir-path>
#
#  Sets the search path for help files to the given file path.
#  Help files specified in any "hyperhelp_file" or "hyperhelp_link"
#  command can then be specified relative to any directory in
#  this path, and the proper file name will be automatically
#  expanded.
# ----------------------------------------------------------------------
proc hyperhelp_path {path} {
	global hhPath

	if {[info exists hhPath]} {
		set hhPath $hhPath:$path
	} else {
		set hhPath $path
	}
}

# ----------------------------------------------------------------------
#  USAGE: hyperhelp_find <file-name>
#
#  Searches for the given file name on the search path specified
#  via "hyperhelp_path".  Returns the proper (expanded) file name,
#  or the original name if the file is not found.
# ----------------------------------------------------------------------
proc hyperhelp_find {fname} {
	global hhPath

	foreach dir [split $hhPath :] {
		if {[file readable $dir/$fname]} {
			return "$dir/$fname"
		}
	}
	return $fname
}

# ======================================================================
# Internal Commands
# ======================================================================

# ----------------------------------------------------------------------
#  USAGE: hyperhelp_open
#
#  Called whenever something is about to be displayed, to make sure
#  that the help window is mapped.
# ----------------------------------------------------------------------
proc hyperhelp_open {} {
	if {![winfo exists .hyperhelp]} {
		error "HyperHelp not properly initialized\n(must invoke hyperhelp_init before using help facilities)"
	}
	if {![winfo ismapped .hyperhelp]} {
		global hhPlace hhCmds hhSources hhMarkers
		set hhPlace -1
		set hhCmds {}
		set hhSources {}
		set hhMarkers {}
	}
	hyperhelp_map .hyperhelp
}

# ----------------------------------------------------------------------
#  USAGE: hyperhelp_close
#
#  Causes the help window to disappear.
# ----------------------------------------------------------------------
proc hyperhelp_close {} {
	.hyperhelp.view.file config -text ""
	hyperhelp_unmap .hyperhelp
}

# ----------------------------------------------------------------------
#  USAGE: hyperhelp_discard
#
#  Discards the current help topic being displayed.
# ----------------------------------------------------------------------
proc hyperhelp_discard {} {
	global hhPlace hhSources

	hyperhelp_delete $hhPlace

	if {[llength $hhSources] > 0} then {
		set newPlace $hhPlace
		set hhPlace -1
		hyperhelp_display $newPlace
	} else {
		hyperhelp_close
	}
}

# ----------------------------------------------------------------------
#  USAGE: hyperhelp_jump <jumpCmd> <jumpSource> <jumpMarker>
#
#  Loads the given help information and causes it to be the
#  current topic for display.  <jumpCmd> contains the configure
#  key passed to the htext widget (usually "-file" or "-text").
#  <jumpSource> contains the name of the help file or the
#  message to be displayed.  <jumpMarker> indicates the starting
#  position in the message for display.
# ----------------------------------------------------------------------
proc hyperhelp_jump {jumpCmd jumpSource jumpMarker} {
	global hhPlace

	hyperhelp_insert $hhPlace $jumpCmd $jumpSource $jumpMarker
	hyperhelp_display [expr $hhPlace+1]
}

# ----------------------------------------------------------------------
#  USAGE: hyperhelp_insert <pos> <jumpCmd> <jumpSource> <jumpMarker>
#
#  Inserts the given <jumpCmd>, <jumpSource> and <jumpMarker>
#  information into the global lists keeping track of this help
#  display.  The new elements are inserted *after* the given
#  position <pos> in the list; if <pos> is "end", then new elements
#  are appended at the end of the list.
# ----------------------------------------------------------------------
proc hyperhelp_insert {pos jumpCmd jumpSource jumpMarker} {
	global hhCmds hhSources hhMarkers

	if {$pos == "end"} {
		set pos [llength $hhSources]
	}
	set after [expr $pos+1]

	set hhCmds    [linsert $hhCmds    $after $jumpCmd]
	set hhSources [linsert $hhSources $after $jumpSource]
	set hhMarkers [linsert $hhMarkers $after $jumpMarker]
}

# ----------------------------------------------------------------------
#  USAGE: hyperhelp_delete <pos>
#
#  Deletes the help topic at the given position in the global lists
#  that maintain the state of this help display.
# ----------------------------------------------------------------------
proc hyperhelp_delete {pos} {
	global hhPlace hhCmds hhSources hhMarkers

	set hhCmds    [lreplace $hhCmds    $pos $pos]
	set hhSources [lreplace $hhSources $pos $pos]
	set hhMarkers [lreplace $hhMarkers $pos $pos]

	if {$hhPlace >= [llength $hhCmds]} then {
		set hhPlace [expr [llength $hhCmds]-1]
	}
}

# ----------------------------------------------------------------------
#  USAGE: hyperhelp_display <entry>
#
#  Updates the help display, making the given entry the current
#  topic.
# ----------------------------------------------------------------------
proc hyperhelp_display {entry} {
	global hhPlace hhCmds hhSources hhMarkers hhLinks hhCount

	set ypos [.hyperhelp.view.file gotoline]
	if {$ypos >= 0 && $hhPlace != -1} {
		set hhMarkers [lreplace $hhMarkers $hhPlace $hhPlace "@$ypos"]
	}

	set hhPlace $entry

	set jumpCmd    [lindex $hhCmds $entry]
	set jumpSource [lindex $hhSources $entry]
	set jumpMarker [lindex $hhMarkers $entry]

	set hhCount 0
	set hhLinks(null) 1
	.hyperhelp.view.file config $jumpCmd $jumpSource

	if {[string match @* $jumpMarker]} {
		scan $jumpMarker "@%d" ypos
		.hyperhelp.view.file gotoline $ypos
	} else {
		if {$jumpMarker != ""} {
			set jumpLine $hhLinks($jumpMarker)

			if {$jumpMarker == ""} {
				puts stderr "
  / \\   HyperHelp:  INTERNAL ERROR
 / ! \\  |
 -----  |        TROUBLE:  hyperhelp_link marker not recognized
  ///   |         MARKER:  $jumpMarker
  ///   | NOT DEFINED IN:  $jumpSource
"
				set jumpLine 1
			}
		} else {
			set jumpLine 1
		}
		.hyperhelp.view.file gotoline $jumpLine
	}
	hyperhelp_pages
}

# ----------------------------------------------------------------------
#  USAGE: hyperhelp_pages
#
#  Updates a picture of the list of available help topics.  This
#  picture is a row of icons representing the help topics.  One
#  icon picture will be drawn differently to represent the current
#  topic in the list.
# ----------------------------------------------------------------------
proc hyperhelp_pages {} {
	global hhPlace hhCmds hhSources hhMarkers

	foreach i [winfo children .hyperhelp.panel.topics] {
		destroy $i
	}
	set pages [llength $hhCmds]
	for {set i 0} {$i < $pages} {set i [expr $i+1]} {
		button .hyperhelp.panel.topics.page$i \
			-bitmap hyperhelp-emptypage -padx 0 -pady 0 \
			-command "hyperhelp_display $i"

		if {$i == $hhPlace} then {
			.hyperhelp.panel.topics.page$i config \
				-bitmap hyperhelp-fullpage
		}
		pack .hyperhelp.panel.topics.page$i -side left
	}
}

# ----------------------------------------------------------------------
#  USAGE:  hyperhelp_map <win>
#
#  Used instead of "wm deiconify" to map a window.  If the window has
#  already been mapped and has placement information, this is set just
#  before the window is mapped to put the window back in the proper
#  place.  Needed for proper interaction with virtual window managers
#  when windows are in outlying quadrants.
# ----------------------------------------------------------------------
proc hyperhelp_map {win} {
	global hhGeom

	if {[info exists hhGeom($win)]} {
		wm geometry $win $hhGeom($win)
	}
	wm deiconify $win
	raise $win
	focus $win
}

# ----------------------------------------------------------------------
#  USAGE:  hyperhelp_unmap <win>
#
#  Used instead of "wm withdraw" to unmap a window.  Saves current
#  placement information for the window, for the next call to
#  hyperhelp_map.  Needed for proper interaction with virtual window
#  managers when windows are in outlying quadrants.
# ----------------------------------------------------------------------
proc hyperhelp_unmap {win} {
	global hhGeom

	set hhGeom($win) "+[winfo rootx $win]+[winfo rooty $win]"
	wm withdraw $win
}
