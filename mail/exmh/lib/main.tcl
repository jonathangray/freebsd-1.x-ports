# main.tcl
#
# Main body of the application.  Note that system-dependent global
# variable settings have been defined in the exmh script.
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Exmh {} {
    global exmh

    global tk_version

    Mh_Init		;# Defines mhProfile

    Preferences_Init ~/.exmh-defaults $exmh(library)/app-defaults

    # Add this preference to initialize exmh(userLibrary) and exmh(logEnabled)
    Preferences_Add "Hacking Support" \
"These items support the extension of Exmh by User code." {
	{exmh(userLibrary)	userLibrary ~/.tk/exmh	{User library directory}
"You can override modules of the exmh implementation
by putting your versions into a private library directory.
Remember to update the tclIndex file with auto_mkindex
after you add things to that directory."}
	{exmh(logEnabled)	logEnabled OFF	{Debug log enabled}
"Debug information is recorded in a log that you can view
from within exmh.  Turning off the log may save some
memory usage.  You can enable the log temporarily."}
    }

    ExmhArgv		;# snarf up command-line arguments
    ExmhResources	;# and some resources we need soon

    # Support per-user customization
    if [info exists exmh(userLibrary)] {
	auto_path_update $exmh(userLibrary)
    }

    Sedit_BindInit	;# Text, Entry class bindings
    Widget_TextInit	;# Text scrolling
    ExmhLogInit		;# Enables debug loging

    if [catch {User_Init} err] {
	puts stderr "User_Init: $err"
    }

    # The order of the following mainly determines the way
    # their associated items appear in the Preferences dialog
    Sedit_Init		;# built in editor 
    Edit_Init		;# interface to external editors
    Print_Init
    Buttons_Init
    Ftoc_Init
    Msg_Init		;# Depends on Ftoc_Init, Buttons_Init
    Mime_Init
    Folder_Init		;# Sets exmh(folder)
    Inc_Init
    Exwin_Init
    Flist_Init
    Fcache_Init
    Fdisp_Init		;# After Flist and Fcache
    Sound_Init
    Faces_Init
    Background_Init

    wm protocol . WM_DELETE_WINDOW Exmh_Done
    Exwin_Layout
    if [catch {User_Layout} err] {
	puts stderr "User_Layout: $err"
    }
    Exmh_Status $exmh(version)
    if {! $exmh(iconic)} {
	wm deiconify .
    } else {
	wm iconify .
    }
    update
    bind . <Unmap> ExmhUnmapped
    bind . <Map> ExmhMapped

    Folder_Change $exmh(folder)
    busy ExmhJunk
}
proc ExmhJunk {} {
    Inc_Startup
    Exmh_Focus
    Background_Startup
}

proc ExmhArgv {} {
    global argc argv exmh
    set extra {}
    set geo [option get . geometry Geometry]
    set icon [option get . iconposition IconPosition]
    set iconic [option get . iconic Iconic]
    set bg_action {}
    for {set i 0} {$i < $argc} {incr i} {
	set arg [lindex $argv $i]
	case $arg {
	    "-geo*" {
		incr i
		set geo [lindex $argv $i]
	    }
	    "-iconposition" {
		incr i
		set icon [lindex $argv $i]
	    }
	    "-iconic" {
		set iconic 1
		option add *Fltop.iconic 1
	    }
	    "-mono" {
		tk colormodel . monochrome
	    }
	    "-bgAction" {
		incr i
		set exmh(background) [lindex $argv $i]
	    }
	    "-bgPeriod" {
		incr i
		set exmh(bgPeriod) [lindex $argv $i]
	    }
	    default {
		lappend extra $arg
	    }
	}
    }
    # wish snarfs up -geometry and puts it into "geometry"
    global geometry
    if [info exists geometry] {
	set geo $geometry
    }
    if {$geo != {}} {
	if [catch {wm geometry . $geo} err] {
	    puts stderr "-geometry $arg: $err"
	}
    }
    if {$iconic != {}} {
	set exmh(iconic) $iconic
    } else {
	set exmh(iconic) 0
    }
    if {$icon != {}} {
	Exwin_IconPosition . $icon
    }

    set argv $extra
    set argc [llength $extra]
}
proc Exmh_Focus {} {
    global exwin
    focus $exwin(mtext)
}
proc ExmhResources {} {
    global exmh
    if {[tk colormodel .] == "color"} {
	set exmh(c_st_normal) [option get . c_st_normal {}]
	if {$exmh(c_st_normal) == {}} {set exmh(c_st_normal) blue}
	set exmh(c_st_error) [option get . c_st_error {}]
	if {$exmh(c_st_error) == {}} {set exmh(c_st_error) purple}
	set exmh(c_st_warn) [option get . c_st_warn {}]
	if {$exmh(c_st_warn) == {}} {set exmh(c_st_warn) red}
    } else {
	set exmh(c_st_normal) [option get . c_st_normal {}]
	if {$exmh(c_st_normal) == {}} {set exmh(c_st_normal) black}
	if {$exmh(c_st_normal) != "white" && $exmh(c_st_normal) != "black"} {
	    set exmh(c_st_normal) black
	}
	set exmh(c_st_error) $exmh(c_st_normal)
	set exmh(c_st_warn) $exmh(c_st_normal)
    }
}

proc Exmh_Status {string { level normal } } {
    global exmh exwin
    if { $string == 0 } { set string $exmh(version) }
    if [info exists exwin(status)] {
	case $level {
	    red		{set level warn}
	    blue	{set level normal}
	    purple	{set level error}
	    "medium sea green" {set level background}
	    default	{set level normal}
	}
	if ![info exists $exmh(c_st_$level)] {
	    set exmh(c_st_$level) black
	}
	$exwin(status) configure -state normal
	catch {$exwin(status) configure -fg $exmh(st_$level)}
	$exwin(status) delete 0 end
	$exwin(status) insert 0 $string
	$exwin(status) configure -state disabled
	ExmhLog $string
	update idletasks
    } else {
	catch {puts stderr $string}
    }
}
proc Exmh_OldStatus {} {
    global exwin
    if [info exists exwin(status)] {
	return [$exwin(status) get]
    } else {
	return ""
    }
}

proc Exmh_CheckPoint {} {
    Scan_CacheUpdate
    Msg_CheckPoint
}

proc Exmh_Done {} {
    global exmh

    if { [Ftoc_Changes "exit"] == 0} then {
	Background_Wait
	Exmh_Status "Checkpointing state" red
	Exmh_CheckPoint
	Fcache_CheckPoint
	Background_Cleanup
	Mime_Cleanup
	Exwin_CheckPoint
	Sedit_CheckPoint
	destroy .
    }
}
proc Exmh_Abort {} {
    Background_Cleanup
    destroy .
}

proc ExmhUnmapped {} {
    # This triggers auto-commit
    Ftoc_Changes iconified
}
proc ExmhMapped {} {
    Inc_Mapped
}

#### Exmh_Debugging

proc Exmh_Debug { args } {
    global exmhDebug
    if ![info exists exmhDebug] {
	set exmhDebug 0
    }
    if {$exmhDebug} {
	puts stderr $args
    }
    ExmhLog $args
}

proc ExmhLogInit {} {
    global exmh
    set exmh(logInit) 1
    set exmh(logButton) 0
    set exmh(logWindow) 0
}
proc ExmhLog { stuff } {
    global exmh
    if {![info exists exmh(logInit)]} {
	return
    }
    if {! $exmh(logEnabled)} {
	return
    }
    if {! $exmh(logButton)} {
	global exwin
	if [info exists exwin(mainButtons)] {
	    Widget_AddBut $exwin(mainButtons) log "Log" { ExmhLogShow }
	    set exmh(logButton) 1
	}
    }
    if {! $exmh(logWindow)} {
	set exmh(logWindow) 1
	Exwin_Toplevel .log "Exmh Log" Log
	set exmh(logTop) .log
	wm withdraw $exmh(logTop)
	Widget_AddBut $exmh(logTop).but trunc "Truncate" ExmhLogTrunc
	Widget_AddBut $exmh(logTop).but save "Save To File" ExmhLogSave
	set exmh(logYview) 1
	Widget_CheckBut $exmh(logTop).but yview "View Tail" exmh(logYview)
	set exmh(log) [Widget_Text $exmh(logTop) 20 -setgrid true]
	#
	$exmh(log) configure -yscroll {.log.sv set}
    }
    if [info exists exmh(log)] {
	catch {
	    $exmh(log) insert end $stuff
	    $exmh(log) insert end \n
	    if {$exmh(logYview)} {
		$exmh(log) yview -pickplace "end - 1 lines"
	    }
	}
    }
}
proc ExmhLogShow {} {
    global exmh
    if [Exwin_Toplevel .log "Exmh Log" Log] {
	puts stderr "ExmhLogShow - created toplevel?"
    } else {
	# Exwin_Toplevel raises the window with saved geometry
    }
}
proc ExmhLogTrunc {} {
    global exmh
    $exmh(log) delete 1.0 end
}
proc ExmhLogSave {} {
    global exmh
    for {set id 0} {$id < 100} {incr id} {
	set name /tmp/exmhlog.$id
	if ![file exists $name] {
	    if ![catch {open $name w} logfile] {
		break
	    }
	}
    }
    if [catch {
	puts $logfile [$exmh(log) get 1.0 end]
	close $logfile
	Exmh_Status "Saved log in /tmp/exmhlog.$id" blue
    } msg] {
	Exmh_Status "Cannot save log: $msg" purple
    }
}
#### Misc

proc TraceInfo {} {
    if {[info commands tclanalyze] != {}} {
	catch {destroy .traceinfo}
	Widget_Toplevel .traceinfo
	text .traceinfo.msg -width 50 -height 10
	pack append .traceinfo .traceinfo.msg {top expand fill}
	.traceinfo.msg insert end [tclanalyze info]
	bind .traceinfo.msg <Button-1> {destroy .traceinfo}
    }
}

proc DoNothing { args } {
    return ""
}



