# help.tcl
#
# Help and color key.
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Help_KeyDisplay {} {
    global fdisp

    if [Exwin_Toplevel .key "Exmh Key" Key] {
	set key .key
	set b .key.but
    
	set l [Widget_Label $b]
	$l configure -text "[tk colormodel .]"
    
	set font $fdisp(font)

	set key [Widget_Frame .key rim Rim]
	$key configure -borderwidth 10
    
	set t [Widget_SimpleText $key t {top fillx} -width 32 -height 8 -font $font -wrap none]
	Ftoc_ColorConfigure $t
	$t configure -state normal
	$t delete 0.0 end
	$t insert insert "current message\nmoved messages\ndeleted messages\nunseen messages\n"
	$t tag add current 1.0 1.end
	$t tag add moved   2.0 2.end
	$t tag add deleted 3.0 3.end
	$t tag add unseen  4.0 4.end
	$t insert insert "\nleft   => change folder\nmiddle => view subfolders\nright  => set move target\n"
	$t configure -state disabled
    
	set c [canvas $key.can -width 20 -height 30]
	pack append $key $c {top expand fill}
	set y [expr [lindex [$c configure -height] 4]/2]
	set x 4
	set x [HelpKeyLabel $c $x $y current $font curtext curbox]
	set x [HelpKeyLabel $c $x $y unseen $font unsntext unsnbox]
	set x [HelpKeyLabel $c $x $y moveTarget $font tartext tarbox]
	Fdisp_LabelConfigure $c
    }
}
proc HelpKeyLabel { c x y text font texttag boxtag} {
    global fdisp
    set id [$c create text $x $y -text $text -anchor nw -font $font -tags $texttag]
    set box [Fdisp_Box $c $id leaf]
    $c addtag $boxtag withtag $box
    set bbox [$c bbox $box]
    incr x [expr [lindex $bbox 2]-[lindex $bbox 0]+$fdisp(xgap)]
    return $x
}

proc Help {} {
    global exmh
    if [Exwin_Toplevel .help "Exmh Help" Help] {
	Widget_Label .help.but label {left fill} -text "  Help file for exmh"
    
	set t [Widget_Text .help 30 -setgrid true]
	Ftoc_ColorConfigure $t
	$t insert insert "Help for EXMH, a TK front-end for the MH mail system.\n"
	$t insert insert "Version: $exmh(version)\n"
	foreach dir [list $exmh(library) .] {
	    if [catch {open $dir/exmh.help} in] {
		continue
	    }
	    $t insert insert [read $in]
	    # This is data-dependent...
	    set L 26
	    $t tag add current $L.0 $L.end ; incr L
	    $t tag add deleted $L.0 $L.end ; incr L
	    $t tag add moved $L.0 $L.end ; incr L
	    $t tag add unseen $L.0 $L.end ; incr L
	    return
	}
	$t insert insert "Cannot find help file to display"
    }
}

