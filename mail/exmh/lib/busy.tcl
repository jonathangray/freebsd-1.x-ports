# 
# busy.tcl
#
# Busy feedback.
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Busy_Init {} {
    global busy exmh

    Preferences_Add "Busy Indicator" \
"These items affect how exmh indicates it is busy." \
    [list \
        {busy(style) busyStyle {CHOICE cursor icon cursorAll none} {How to indicate busy}
"icon - show a bitmap in the faces display.
cursor - change the cursor to a busy indicator.
cursorAll - Like cursor, but hits all widgets and takes longer.
none - do nothing."} \
	[list busy(cursor) busyCursor watch {Cursor for busy indicator} \
"This is a TK specification for a cursor.  You can use a standard
X cursor, like \"watch\", or provide your own bitmaps and mask
with \"@filename\".  You can optionally provide foreground and
background colors. (See Tk_GetCursor for complete details).
A final twist is that relative pathnames are munged to be
absolute pathnames under $exmh(library)
Examples include:
    watch		- standard watch
    watch blue		- a clear and blue watch
    watch blue white	- a white and blue watch
    @timer.bitmap black	 - 16x16 timer/watch (need fg color!)
    @hourglass1.bitmap black	- Standard wish hourglass
    @hourglass2.bitmap blue	- Large 32x32 hourglass, in blue
    @hourglass2.bitmap hourglass2.mask red black - (need two colors!)
    @/usr/foo/exmh/bar.mask /usr/foo/exmh/bar.bitmap black yellow
"] \
	[list busy(bitmap) busyBitmap @hourglass2.bitmap {Bitmap for busy indicator} \
"This is a TK specification for a bitmap.  There are only a few
boring built-in bitmaps, so mostly you specify these with the
@pathname syntax.  A relative pathname is munged to be an
absolute pathname under $exmh(library)
Examples include:
    @hourglass1.bitmap	- Standard wish hourglass
    @/usr/foo/exmh/bar.bitmap
"] \
    ]
    trace variable busy(cursor) w BusyFixupCursor
    BusyFixupCursor

    trace variable busy(bitmap) w BusyFixupBitmap
    BusyFixupBitmap

}
proc BusyFixupCursor { args } {
    global busy exmh
    # busy(cursor) could be
    # @foo.cursor bar.cursor color color
    # Here we insert the exmh library
    switch -regexp $busy(cursor) {
	{^@[^/]} {
	    regsub @(.*) $busy(cursor) $exmh(library)/\\1 newfile
	    if ![file exists [lindex $newfile 0]] {
		Exmh_Status "Invalid file [lindex $newfile 0]"
		return
	    }
	    if {[llength $newfile] > 2} {
		regsub {([^ ]*) (.*)} $newfile "\\1 $exmh(library)/\\2" newfile
	    }
	    set busy(Xcursor) @$newfile
	}
	 .* {
	    set busy(Xcursor) $busy(cursor)
	}
    }
    Exmh_Status "Cursor $busy(Xcursor)"
}
proc BusyFixupBitmap { args } {
    global busy exmh
    # busy(bitmap) could be
    # @foo.bitmap bar.bitmap color color
    # Here we insert the exmh library
    switch -regexp $busy(bitmap) {
	{^@[^/]} {
	    regsub @(.*) $busy(bitmap) $exmh(library)/\\1 newfile
	    if [file exists $newfile] {
		set busy(Xbitmap) @$newfile
	    } else {
		Exmh_Status "Invalid file $newfile"
		return
	    }
	}
	 .* {
	    set busy(Xbitmap) $busy(bitmap)
	}
    }
    Exmh_Status "Bitmap $busy(Xbitmap)"
}
proc busy { args } {
    global busy
    if ![info exists busy(style)] Busy_Init
    switch $busy(style) {
	icon		{busyIcon $args}
	cursorAll	{busyCursor $args}
	cursor		{busyCursorHack $args}
	default		{}
    }
}
proc busyIcon { cmd } {
    global exwin exmh errorInfo busy

    if [info exists exwin(faceCanvas)] {
	set h [$exwin(faceCanvas) create bitmap 24 24 -anchor c \
		    -bitmap $busy(Xbitmap)]
	update idletasks
    }

    set error [catch {uplevel #0 $cmd} result]
    set ei $errorInfo

    if [info exists h] {
	$exwin(faceCanvas) delete $h
    }

    if $error {
	error $result $ei
    } else {
	return $result
    }

}
proc busyCursorInner { cmd widgets } {
    global errorInfo busy
    foreach w $widgets {
	catch {[lindex $w 0] config -cursor $busy(Xcursor)}
    }
    update idletasks

    set error [catch {uplevel #0 $cmd} result]
    set ei $errorInfo

    foreach w $widgets {
	catch {[lindex $w 0] config -cursor [lindex $w 1]}
    }
    if $error {
	error $result $ei
    } else {
	return $result
    }
}
proc busyCursorHack {cmd} {
    set widgets {}
    catch {
	#Fdisp_Busy
	global fdisp
	foreach can {canvas cache} {
	    if [info exists fdisp($can)] {
		set w $fdisp($can)
		set cursor [lindex [$w config -cursor] 4]
		lappend widgets [list $w $cursor]
	    }
	}
	#Exwin_Busy
	global exwin
	foreach w [list $exwin(mtext) $exwin(ftext)] {
	    set cursor [lindex [$w config -cursor] 4]
	    lappend widgets [list $w $cursor]
	}
	#Sedit_Busy
	global sedit
	foreach w $sedit(allids) {
	    set cursor [lindex [$w config -cursor] 4]
	    lappend widgets [list $w $cursor]
	}
	#Label_Busy
	global label
	foreach w [list $label(main) $label(folder) $label(message) $exwin(status)] {
	    set cursor [lindex [$w config -cursor] 4]
	    lappend widgets [list $w $cursor]
	}
    }
    return [busyCursorInner $cmd $widgets]
}
proc busyCursor {cmd} {
    set widgets {.app .root}
    set list [winfo children .]
    while {$list != ""} {
	set next {}
	foreach w $list {
	    set cursor [lindex [$w config -cursor] 4]
	    lappend widgets [list $w $cursor]
	    set next [concat $next [winfo children $w]]
	}
	set list $next
    }
    return [busyCursorInner $cmd $widgets]
}

