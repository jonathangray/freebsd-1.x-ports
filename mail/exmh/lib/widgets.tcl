# widgets.tcl
#
# Widget utilities
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Widget_Toplevel { path name {class Dialog} {x {}} {y {}} } {
    set self [toplevel $path -class $class]
    set usergeo [option get $path position Position]
    if {$usergeo != {}} {
	if [catch {wm geometry $self $usergeo} err] {
	    Exmh_Debug Widget_Toplevel $self $usergeo => $err
	}
    } else {
	if {($x != {}) && ($y != {})} {
	    Exmh_Debug Event position $self +$x+$y
	    wm geometry $self +$x+$y
	}
    }
    wm title $self $name
    wm group $self .
    return $self
}
proc Widget_Vgeo { geo win } {
    set vx [winfo vrootx $win]
    set vy [winfo vrooty $win]
    if {$vx == 0 && $vy == 0} {
	Exmh_Debug Widget_Vgeo vx=$vx vy=$vy
	return $geo
    }
    set wd [winfo width $win]
    set ht [winfo height $win]
    if [regexp {([\+-])([0-9]+)([\+-])([0-9]+)} $geo match s1 x s2 y] {
	if {$s1 == "-"} {
	    set x -$x
	}
	if {$s2 == "-"} {
	    set y -$y
	}
	if {($x < 0) || ([string compare $x "-0"] == 0)} {
	    set x [expr [winfo screenwidth $win]+$x-$wd]
	}
	if {($y < 0) || ([string compare $y "-0"] == 0)} {
	    # 64 depends on icon height
	    set y [expr [winfo screenheight $win]+$y-$ht]
	}
	set x [expr $x-$vx]
	set y [expr $y-$vy]
	Exmh_Debug Widget_Vgeo: $geo, vx=$vx vy=$vy, => +$x+$y
	return +$x+$y
    } else {
	Exmh_Debug Widget_Vgeo failed on $geo
	return $geo
    }
}
proc Widget_Frame {par child {class Exmh} {where {top expand fill}} args } {

    if {$par == "."} {
	set self .$child
    } else {
	set self $par.$child
    }
    eval {frame $self -borderwidth 0 -class $class} $args
    pack append $par $self $where
    return $self
}

proc Widget_SplitFrame {f c1 c2} {
    # Create a left and right frame within a frame
    frame $f.left -relief raised -class $c1
    frame $f.right -relief raised -class $c2
    pack append $f $f.left {left fill expand} $f.right {left fill expand}
    return [list $f.left $f.right]
}

proc Widget_SplitFrameR {f c1 c2} {
    # Create a left and right frame within a frame - left frame doesn't expand
    frame $f.left -relief raised -class $c1
    frame $f.right -relief raised -class $c2
    pack append $f $f.left {left fill} $f.right {left fill expand}
    return [list $f.left $f.right]
}
proc Widget_SplitFrameV {f c1 c2} {
    # Create a top and bottom frame within a frame
    frame $f.top -relief raised -class $c1
    frame $f.bot -relief raised -class $c2
    pack append $f $f.top {top fill expand} $f.bot {bottom fill expand}
    return [list $f.top $f.bot]
}

proc Widget_AddButDef {par but {where {right padx 1}} } {
    # Create a Packed button.  Return the button pathname
    set cmd2 [list button $par.$but -relief raised]
    if [catch $cmd2 t] {
	puts stderr "Widget_AddBut (warning) $t"
	eval $cmd2 {-font fixed}
    }
    pack append $par $par.$but $where
    return $par.$but
}

proc Widget_AddBut {par but txt cmd {where {right padx 1}} } {
    # Create a Packed button.  Return the button pathname
    set cmd2 [list button $par.$but -text $txt -command $cmd -relief raised]
    if [catch $cmd2 t] {
	puts stderr "Widget_AddBut (warning) $t"
	eval $cmd2 {-font fixed}
    }
    pack append $par $par.$but $where
    return $par.$but
}

proc Widget_CheckBut {par but txt var {where {right padx 1}} } {
    # Create a check button.  Return the button pathname
    set cmd [list checkbutton $par.$but -text $txt -variable $var -relief raised]
    if [catch $cmd t] {
	puts stderr "Widget_CheckBut (warning) $t"
	eval $cmd {-font fixed}
    }
    pack append $par $par.$but $where
    return $par.$but
}

proc Widget_RadioBut {par but txt var {where {right padx 1}} } {
    # Create a radio button.  Return the button pathname
    set cmd [list radiobutton $par.$but -text $txt -variable $var -relief raised -value $txt]
    if [catch $cmd t] {
	puts stderr "Widget_RadioBut (warning) $t"
	eval $cmd {-font fixed}
    }
    pack append $par $par.$but $where
    return $par.$but
}

proc Widget_AddMenuBDef {par b {where {left fill}} } {
    # Create a button and a menu to go with it.  Return the button pathname
    set cmd [list menubutton $par.$b -menu $par.$b.m -relief raised]
    if [catch $cmd t] {
	puts stderr "Widget_AddMenuB (warning) $t"
	eval $cmd {-font fixed}
    }
    if [catch {menu $par.$b.m}] {
	menu $par.$b.m -font fixed
    }
    pack append $par $par.$b $where
    return $par.$b
}
proc Widget_AddMenuB {par b {label {}} {where {left fill}} } {
    # Create a button and a menu to go with it.  Return the button pathname
    set cmd [list menubutton $par.$b -menu $par.$b.m -relief raised -text $label]
    if [catch $cmd t] {
	puts stderr "Widget_AddMenuB (warning) $t"
	eval $cmd {-font fixed}
    }
    if [catch {menu $par.$b.m}] {
	menu $par.$b.m -font fixed
    }
    pack append $par $par.$b $where
    return $par.$b
}

proc Widget_AddMenuItem {mb l cmd {accel NONE}} {
    # Create a menu command entry with optional accelerator string.
    # Note that the mb parameter is really the menu button pathname.
    set cmd2 [list $mb.m add command -label $l  -command $cmd]
    if [catch $cmd2 t] {
	puts stderr "Widget_AddMenuItem (warning) $t"
	eval $cmd2 {-font fixed}
    }
    if {$accel != "NONE"} {
	$mb.m entryconfigure $l -accelarator $accel
    }
}

proc Widget_AddMenuSeparator {mb} {
    $mb.m add separator
}

proc Widget_FunnyMenuItem {mb l cmd {accel NONE}} {
    # Create a menu command entry with optional accelerator string.
    # The command is munged to make the entry promote itself to a button
    set cmd2 [list $mb.m add command -label $l -command [list WidgetMenuToButton $mb $cmd]]
    if [catch $cmd2 t] {
	puts stderr "Widget_FunnyMenuItem (warning) $t"
	eval $cmd2 {-font fixed}
    }
    if {$accel != "NONE"} {
	$mb.m entryconfigure $l -accelerator $accel
    }
}

proc Widget_RadioMenuItem {m l {cmd { }} {var {}}} {
    # Create a radio menu entry.  By default all radio entries
    # for a menu share a variable.
    # Note that the m parameter is really the menu button pathname
    if {$var == {}} {
	set var v$m
    }
    set cmd2 [list $m.m add radio -label $l  -variable $var -command $cmd]
    if [catch $cmd2 t] {
	puts stderr "Widget_RadioMenuItem (warning) $t"
	eval $cmd2 {-font fixed}
    }
}

proc Widget_CheckMenuItem {m l {c { }} {var {}}} {
    # Create a Check button menu entry.  By default all check entries
    # have their own variable.
    # Note that the m parameter is really the menu button pathname
    if {$var == {}} {
	set var v$m.$l
    }
    set cmd [list $m.m add check -label $l -variable $var -command $c]
    if [catch $cmd t] {
	puts stderr "Widget_CheckMenuItem (warning) $t"
	eval $cmd {-font fixed}
    }
    return $var
}

proc WidgetMenuToButton { mb cmd } {
    # Warp a menu entry into a button

    # First, do the action
    eval $cmd
    # Search out a menu entry and make it into a button
    set frame [string range $mb 0 [expr [string last . $mb]-1]]
    set numItems [$mb.m index last]
    set hit 0
    for {set i 0} {$i < $numItems} {incr i} {
	foreach conf [$mb.m entryconfigure $i] {
	    # Depend on -command appearing before -label
	    if {[lindex $conf 0] == "-command" &&
		[string match *$cmd* [lindex $conf 4]]} {
		set hit 1
	    }
	    if {$hit && [lindex $conf 0] == "-label"} {
		if [catch {
		    global _PromoteSibling
		    set cmd2 [list button $frame.b$i -text [lindex $conf 4] -command $cmd]
		    if [catch $cmd2 t] {
			puts stderr "WidgetMenuToButton (warning) $t"
			eval $cmd2 {-font fixed}
		    }
		    if ![info exists _PromoteSibling] {
			set _PromoteSibling $mb
		    }
		    pack before $_PromoteSibling $frame.b$i {left fill}
		    set _PromoteSibling $frame.b$i
		} msg] {
		    if {! [string match {*already exists*} $msg]} {
			puts stderr "Promote: $msg"
		    }
		}
		return
	    }
	}
    }
}

proc Widget_SimpleText { frame name {where {expand fill}} args } {
    # Create a one-line text widget
    global exwin
    set cmd [list text $frame.$name]
    if [catch [concat $cmd $args] t] {
	puts stderr "Widget_TextLine (warning) $t"
	set t [eval $cmd $args {-font fixed}]
    }
    pack append $frame $t $where
    $t mark set insert 0.0

    if [regexp {setgrid} $args] {
	wm minsize [winfo toplevel $frame] 10 1
    }
    Widget_TextInitText $t
    return $t
}
proc Widget_Message { frame {name msg} args} {
    set cmd [list message $frame.$name]
    if [catch [concat $cmd $args] t] {
	puts stderr "Widget_Message (warning) $t"
	eval $cmd $args {-font fixed}
    }
    pack append $frame $frame.$name {top fill expand}
    return $frame.$name
}
proc Widget_Label { frame {name label} {where {left fill}} args} {
    set cmd [list label $frame.$name ]
    if [catch [concat $cmd $args] t] {
	puts stderr "Widget_Label (warning) $t"
	eval $cmd $args {-font fixed}
    }
    pack append $frame $frame.$name $where
    return $frame.$name
}
proc Widget_Entry { frame {name entry} {where {left fill}} args} {
    set cmd [list entry $frame.$name ]
    if [catch [concat $cmd $args] t] {
	puts stderr "Widget_Entry (warning) $t"
	eval $cmd $args {-font fixed}
    }
    pack append $frame $frame.$name $where
    return $frame.$name
}


proc Widget_ReadOnlyText { w } {
    # Undo the modification keystrokes
    foreach b [bind $w] {
	if {! [string match *Button* $b] && ! [string match {*B[123]*} $b]} {
	    bind $w $b ""
	}
    }
}
