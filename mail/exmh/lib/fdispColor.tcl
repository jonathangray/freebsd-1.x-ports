#
# fdispLabel.tcl
#
# This sets the color (or monochrome) feedback for the folder display area.
# There are four states for a label, normal, unseen, target, current.
# Each label has two parts, a box and text.
# (Some labels can be bitmaps, in which case they just have a bitmap.)
# You can read the TK man page on the canvas widget to see what
# sort of looks are possible for text and rectangles.  The defaults use
# -fill color		;# for text color
# -fill color		;# for box background
# -outline color	;# for box outline color
# -width number		;# for box outline width
# -stipple bitmap	;# for box stipple pattern
#
# In turn, for color displays, there are 5 basic colors that can be set
# by ~/.exmh_defaults
# Resource	Variable		Comment
# c_foreground	$fdisp(c_fg)		Foreground
# c_background	$fdisp(c_bg)		Background
# c_current	$fdisp(c_current)	Current folder
# c_unseen	$fdisp(c_unseen)	Unseen messages in the folder
# c_moved	$fdisp(c_moved)		Target folder / moved message
# c_deleted	$fdisp(c_deleted)	Deleted message
#
# For consistency these colors are shared with the folder table of contents
# display.  If you find this limiting, wack away on the itemconfigures...
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Fdisp_LabelConfigure { canvas } {
    global fdisp
    #
    # Note that a particular label may have multiple tags, so more than
    # one of these itemconfigures will hit.  Therefore their ordering
    # is important, and you can play some tricks with combinations.
    # (more so on color displays)
    # 
    Fdisp_FixupSpecials $canvas
    if {[tk colormodel .] == "monochrome"} {
	$canvas itemconfigure text -fill black
	$canvas itemconfigure box -fill white -width 1 -stipple {}
	$canvas itemconfigure bitmap -foreground black -background white

	$canvas itemconfigure unsntext -fill black
	$canvas itemconfigure unsnbox -width 3
	$canvas itemconfigure unsnbitmap -foreground white -background black

	$canvas itemconfigure tartext -fill black
	$canvas itemconfigure tarbox -fill black -stipple gray25
	$canvas itemconfigure tarbitmap -foreground white -background black

	$canvas itemconfigure curtext -fill white
	$canvas itemconfigure curbox -fill black -stipple {}
	$canvas itemconfigure curbitmap -foreground white -background black
    } else {
	$canvas itemconfigure text -fill $fdisp(c_fg)
	$canvas itemconfigure box -fill $fdisp(c_bg) \
				-outline $fdisp(c_fg) -width 1 -stipple {}
	$canvas itemconfigure bitmap -foreground $fdisp(c_fg) \
				-background $fdisp(c_bg)

	# Let current text be overridden by unseen & target feedback
	$canvas itemconfigure curtext -fill $fdisp(c_current)

	$canvas itemconfigure unsntext -fill $fdisp(c_bg)
	$canvas itemconfigure unsnbox -fill $fdisp(c_unseen)
	$canvas itemconfigure unsnbox -outline $fdisp(c_unseen) -width 2
	$canvas itemconfigure unsnbitmap -foreground $fdisp(c_unseen) \
					-background $fdisp(c_bg)

	$canvas itemconfigure tartext -fill $fdisp(c_fg)
	$canvas itemconfigure tarbox -fill $fdisp(c_moved)
	$canvas itemconfigure tarbitmap -foreground $fdisp(c_moved) \
					-background $fdisp(c_bg)

	$canvas itemconfigure curbox -outline $fdisp(c_current) -width 2
	$canvas itemconfigure curbitmap -foreground $fdisp(c_current) \
					-background $fdisp(c_bg)
    }
}

proc FdispClearHighlights { canvas } {
    foreach tag {unsnbitmap unsnbox unsntext} {
	$canvas dtag $tag
    }
}

