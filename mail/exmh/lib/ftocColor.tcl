# 
# ftocColor.tcl
#
# Color and Monochrome feedback for the ftoc display..
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Ftoc_ColorConfigure { text } {
    global exmh ftoc
    if {[tk colormodel .] == "monochrome"} {
	$text tag configure deleted \
	    -bgstipple @$exmh(library)/linethru.bitmap \
	    -foreground black -background black
	$text tag configure moved \
	    -bgstipple gray25 -foreground black -background black
	$text tag configure range -background white \
	    -relief raised -borderwidth 2
	$text tag configure mrange \
	    -bgstipple gray25 -background black -background black \
	    -relief raised -borderwidth 2
	$text tag configure drange \
	    -bgstipple @$exmh(library)/linethru.bitmap \
	    -foreground black -background black \
	    -relief raised -borderwidth 2
	$text tag configure unseen -underline true
	$text tag configure current \
	    -bgstipple {} -foreground white -background black
    } else {
	Preferences_Resource ftoc(c_current) 	c_current red
	Preferences_Resource ftoc(c_unseen) 	c_unseen  blue
	Preferences_Resource ftoc(c_moved) 	c_moved   yellow
	Preferences_Resource ftoc(c_deleted) 	c_deleted grey75
	set bg [lindex [$text configure -background] 4]
	$text tag configure deleted -background $ftoc(c_deleted)
	$text tag configure moved -background $ftoc(c_moved)
	$text tag configure range -background $bg -relief raised -borderwidth 2
	$text tag configure mrange -background $ftoc(c_moved) \
		-relief raised -borderwidth 2
	$text tag configure drange -background $ftoc(c_deleted) \
		-relief raised -borderwidth 2
	$text tag configure unseen -foreground $ftoc(c_unseen)
	$text tag configure current -foreground $ftoc(c_current)
    }
}

