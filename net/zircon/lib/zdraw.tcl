proc zdraw {nk cmd} {
    set chan [string tolower [lindex $cmd 0]]
    global Draw
    if !$Draw(${chan}) {
	makeZDraw $chan
	regsub -all {[][$]} [lrange $cmd 1 end] {} dc
	set w .@zd${chan}.pic.canvas
	case [lindex $dc 0] {
	delete { eval $w $dc }
	default { eval $w $dc -tags $nk }
	}
    }
}

proc zdDestroy win {
    destroy $win
    foreach v {ZDStart ZDPoints ZDMode ZDLast ZDFill ZDOln} {
	global $v ; unset ${v}($win)
    }
}

proc zdModeChange {type win} {
    global ZDMode
    if [info exists ZDMode($win)] {
	$win.plt.$ZDMode(${win}) configure -relief raised
    }
    set ZDMode(${win}) $type
    foreach v {ZDStart ZDLast ZDPoints} {
	global $v
	set ${v}(${win}) {}
    }
    $win.pic.canvas delete @r
    $win.pic.canvas delete @p
    $win.plt.$type configure -relief sunken
}

if {[tk colormodel .] != "monochrome"} {
proc clButtons win {
    global zircon
    button $win.oln.none -bitmap @$zircon(lib)/bitmaps/none.xbm \
      -command "zdColChange ZDOln $win oln none" -relief raised \
      -height 24 -width 24
    button $win.fill.none -bitmap @$zircon(lib)/bitmaps/none.xbm \
      -command "zdColChange ZDFill $win fill none" -relief raised \
      -height 24 -width 24
    zpack $win.oln none {left expand fillx}
    zpack $win.fill none {left expand fillx}
    foreach cl {black white red orange yellow green blue violet} {
	button $win.oln.$cl -background $cl \
	  -command "zdColChange ZDOln $win oln $cl" \
	  -relief raised
	button $win.fill.$cl \
	  -background $cl -command "zdColChange ZDFill $win fill $cl" \
	  -relief raised 
	zpack $win.oln $cl {left expand fill}
	zpack $win.fill $cl {left expand fill}
    }
}
} {
proc clButtons win {
    foreach cl {none black white red orange yellow green blue violet} {
	set ccl [capitalise $cl]
	button $win.oln.$cl -text $ccl -width 6 \
	  -command "zdColChange ZDOln $win oln $cl" \
	  -relief raised
	button $win.fill.$cl -text $ccl -width 6 \
	  -command "zdColChange ZDFill $win fill $cl" \
	  -relief raised 
	zpack $win.oln $cl {left expand fill}
	zpack $win.fill $cl {left expand fill}
    }
}
}

proc zdColChange {var win frame cl} {
global $var
    if [info exists ${var}($win)] {
	$win.$frame.$cl configure -relief raised
    }
    set ${var}($win) $cl
    $win.$frame.$cl configure -relief sunken
}

proc makeZDraw chan {
    set win .@zd${chan}
    if [winfo exists $win] { wm deiconify $win ; raise $win ; return }
    toplevel $win -class Zircon
    wm title $win "${chan} Sketch Pad"
    wm minsize $win 100 100
    set f0 [frame $win.btn -relief raised]
    button $f0.save -text Save
    button $f0.print -text Print -command "zdPrint ${chan}"
    button $f0.clear -text Clear -command "zdClear $win.pic.canvas $chan"
    button $f0.quit -text Quit -command "zdDestroy $win"
    zpack $f0 {save print clear quit} {left expand fillx}
    set fp [frame $win.plt -relief raised]
    global zircon
    foreach t {line arc polygon rectangle oval text} {
	button $fp.$t -bitmap @$zircon(lib)/bitmaps/${t}.xbm \
	  -command "zdModeChange $t ${win}" -height 48 -width 48 \
	  -relief raised
	zpack $fp $t {left expand fillx}
    }
    frame $win.oln -relief raised
    label $win.oln.label -text Outline -width 10
    zpack $win.oln label left
    frame $win.fill -relief raised
    label $win.fill.label -text Fill -width 10
    zpack $win.fill label left
    clButtons $win
    set f1 [frame $win.pic -relief raised]
    set f2 [frame $win.hsFrm]
    scrollbar $f1.vscroller -command "$f1.canvas yview" 
    canvas $f1.canvas -yscrollcommand "$f1.vscroller set" \
      -xscrollcommand "$f2.hscroller set" -background white
    zpack $f1 canvas {left expand fill}
    zpack $f1 vscroller {right filly}
    scrollbar $f2.hscroller -command "$f1.canvas xview" -orient horizontal
    frame $f2.pf0
    zpack $f2 hscroller {left expand fillx}
    zpack $f2 pf0 {right padx 20}
    zpack $win {btn plt oln fill} fillx
    zpack $win pic {expand fill}
    zpack $win hsFrm fillx
    zdModeChange line $win
    zdColChange ZDOln $win oln black
    zdColChange ZDFill $win fill none
    bind $f1.canvas <1> " zdPress ${chan} 1 %x %y "
    bind $f1.canvas <Double-1> " zdDouble ${chan} 1 %x %y "
    bind $f1.canvas <B1-Motion> { zdMove %W 1 %x %y }
    bind $f1.canvas <ButtonRelease-1> " zdUp ${chan} 1 %x %y "
}

proc zdPrint chan {
    .@zd${chan}.pic.canvas postscript -file /tmp/${chan}.ps
}

proc zdSave chan {
    mkFileBox .@zs${chan} "Save Sketch ${chan}" {}\
      "Save ${chan} sketch pad to:" \
      "OK {.@zd${chan}.pic.canvas postscript -file }" \
      {Cancel {}}
}

proc zdDo {chan args} {
    global lcNickname
    eval .@zd${chan}.pic.canvas [join $args] -tags $lcNickname
    sendCtcp ZIRCON ${chan} "DRAW ${chan} [join $args]"
}

proc zdClear {win chan} {
    global lcNickname
    zdDo $chan delete $lcNickname
}

proc zdPress {chan btn x y} {
    global ZDMode
    global ZDStart
    global ZDLast
    set w .@zd${chan}
    if {$ZDStart($w) == {}} {
	set ZDLast($w) [set ZDStart($w) [list $x $y]]
	return
    }
    global ZDFill
    if {[set fill $ZDFill($w)] == {none}} {set fill {{}}}
    global ZDOln
    if {[set oln $ZDFill($w)] == {none}} {set oln {{}} }
    case $ZDMode($w) {
    {arc oval rectangle} {
	    zdDo $chan create $ZDMode($w) [join $ZDStart($w)] $x $y \
	    -fill $fill -outline $oln
	    set ZDLast($w)[ set ZDStart($w) {}]
	}
    line {
	    zdDo $chan create $ZDMode($w) [join $ZDStart($w)] $x $y \
	    -fill $oln
	    set ZDLast($w)[ set ZDStart($w) {}]
	}
    polygon {
	    eval ${w}.pic.canvas create line [join $ZDLast($w)] $x $y \
	      -tags @p -fill $oln
	    global ZDPoints
	    lappend ZDPoints($w) "$x $y"
	    set ZDLast($w) [list $x $y]
	}
    text {
	}
    }
}

proc zdMove {win btn x y} {
    global ZDStart
    set w [winfo toplevel $win]
    if {$ZDStart($w) == {} || $ZDStart($w) == [list $x $y]} { return }
    global ZDMode
    global ZDLast
    global ZDFill
    if {[set fill $ZDFill($w)] == {none}} {set fill {{}}}
    global ZDOln
    if {[set oln $ZDFill($w)] == {none}} {set oln {{}} }
    $win delete @r
    case $ZDMode($w) {
    {arc oval rectangle} {
	    eval ${win} create $ZDMode($w) [join $ZDStart($w)] $x $y \
	      -tags @r -fill $fill -outline $oln
	}
    line {
	    eval ${win} create line [join $ZDStart($w)] $x $y \
	      -tags @r -fill $oln
	}
    polygon {
	    eval ${win} create line [join $ZDLast($w)] $x $y -tags @r \
	      -fill $oln
	}
    text {
	}   
    }
}

proc zdUp {chan btn x y} {
    global ZDMode
    global ZDStart
    set w .@zd${chan}
    if {$ZDStart($w) != {} && $ZDStart($w) != [list $x $y]} {
	global ZDLast
	global ZDFill
	if {[set fill $ZDFill($w)] == {none}} {set fill {{}}}
	global ZDOln
	if {[set oln $ZDFill($w)] == {none}} {set oln {{}} }
	set win .@zd${chan}.pic.canvas
	$win delete @r
	case $ZDMode($w) {
	{arc oval rectangle} {
		zdDo ${chan} create $ZDMode($w) [join $ZDStart($w)] $x $y \
		  -fill $fill -outline $oln
		set ZDLast($w) [set ZDStart($w) {}]
	    }
	line {
		zdDo ${chan} create $ZDMode($w) [join $ZDStart($w)] $x $y \
		  -fill $oln
		set ZDLast($w) [set ZDStart($w) {}]
	    }
	polygon {
		eval ${win} create line [join $ZDLast($w)] $x $y -tags @r \
		  -fill $oln
		set ZDLast($w) [list $x $y]
	    }
	text {
	    }    
	}
    }
}

proc zdDouble {chan btn x y} {
    global ZDMode
    global ZDStart
    global ZDLast
    set w .@zd${chan}
    if {$ZDStart($w) == {}} {
	set ZDLast($w) [set ZDStart($w) [list $x $y]]
	return
    }
    global ZDFill
    if {[set fill $ZDFill($w)] == {none}} {set fill {{}}}
    global ZDOln
    if {[set oln $ZDFill($w)] == {none}} {set oln {{}} }
    case $ZDMode($w) {
    {arc oval rectangle} {
	    zdDo $chan create $ZDMode($w) [join $ZDStart($w)] $x $y \
	      -fill $fill -outline $oln
	    set ZDLast($w)[ set ZDStart($w) {}]
	}
    line {
	    zdDo $chan create $ZDMode($w) [join $ZDStart($w)] $x $y \
	      -fill $oln
	    set ZDLast($w)[ set ZDStart($w) {}]
	}
    polygon {
	    global ZDPoints
	    zdDo ${chan} create polygon [join $ZDStart($w)] \
	      [join $ZDPoints($w)] $x $y -fill $fill
	    ${w}.pic.canvas delete @p
	    if {$oln != {{}}} {
		zdDo ${chan} create line [join $ZDStart($w)] \
		  [join $ZDPoints($w)] $x $y [join $ZDStart($w)] \
		  -fill $oln
	    }
	    set ZDPoints($w) {}
	    set ZDLast($w)[ set ZDStart($w) {}]
	}
    text {
	}
    }
}



