proc zdraw {usr cmd} {
    set this [find [lindex $cmd 0]]
    if [$this draw] {
	$this makeZDraw
	set w .@zd${this}.pic.canvas
	switch [lindex $cmd 1] {
	delete { $w delete [string tolower [lindex $cmd 2]] }
	default {
		regsub -all {[][$]} [lrange $cmd 1 end] {} dc
		set tid [eval $w $dc]
		$w itemconfigure $tid -tags [$usr lname]
	    }
	}
    }
}

proc zdDestroy {win} {
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
    pack $win.oln.none -side left -expand 1 -fill x
    pack $win.fill.none -side left -expand 1 -fill x
    foreach cl {black white red orange yellow green blue violet} {
	button $win.oln.$cl -background $cl \
	  -command "zdColChange ZDOln $win oln $cl" \
	  -relief raised
	button $win.fill.$cl \
	  -background $cl -command "zdColChange ZDFill $win fill $cl" \
	  -relief raised 
	pack $win.oln.$cl -side left -expand 1 -fill both
	pack $win.fill.$cl -side left -expand 1 -fill both
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
	pack $win.oln.$cl -side left -expand 1 -fill both
	pack $win.fill.$cl -side left -expand 1 -fill both
    }
}
}

proc zdColChange {var win frame cl} {
    global $var
    if [info exists ${var}($win)] {
	$win.$frame.[set ${var}($win)] configure -relief raised
    }
    set ${var}($win) $cl
    $win.$frame.$cl configure -relief sunken
}

proc channel_makeZDraw {this} {
    set win .@zd$this
    if [winfo exists $win] { popup $win ; return }
    set chan [$this name]
    toplevel $win -class Zircon
    wm title $win "${chan} Sketch Pad"
    wm minsize $win 100 100
    set f0 [frame $win.btn -relief raised]
    button $f0.save -text Save
    button $f0.print -text Print -command "$this zdPrint"
    button $f0.clear -text Clear -command "$this zdClear"
    button $f0.quit -text Quit -command "zdDestroy $win"
    pack $f0.save $f0.print  $f0.clear  $f0.quit -side left -expand 1 -fill x
    set fp [frame $win.plt -relief raised]
    global zircon
    foreach t {line arc polygon rectangle oval text} {
	button $fp.$t -bitmap @$zircon(lib)/bitmaps/${t}.xbm \
	  -command "zdModeChange $t ${win}" -height 48 -width 48 \
	  -relief raised
	pack $fp.$t -side left -expand 1 -fill x
    }
    frame $win.oln -relief raised
    label $win.oln.label -text Outline -width 10
    frame $win.fill -relief raised
    label $win.fill.label -text Fill -width 10
    pack $win.oln.label $win.fill.label -side left
    clButtons $win
    set f1 [frame $win.pic -relief raised]
    set f2 [frame $win.hsFrm]
    scrollbar $f1.vscroller -command "$f1.canvas yview" 
    canvas $f1.canvas -yscrollcommand "$f1.vscroller set" \
      -xscrollcommand "$f2.hscroller set" -background white
    pack $f1.canvas -side left -expand 1 -fill both
    pack $f1.vscroller -side right -fill y
    scrollbar $f2.hscroller -command "$f1.canvas xview" -orient horizontal
    frame $f2.pf0
    pack $f2.hscroller -side left -expand 1 -fill x
    pack $f2.pf0 -side right -padx 20
    pack $win.btn $win.plt $win.oln $win.fill -fill x
    pack $win.pic -expand 1 -fill both
    pack $win.hsFrm -fill x
    zdModeChange line $win
    zdColChange ZDOln $win oln black
    zdColChange ZDFill $win fill black
    bind $f1.canvas <1> " $this zdPress 1 %x %y "
    bind $f1.canvas <Double-1> "$this zdDouble 1 %x %y "
    bind $f1.canvas <B1-Motion> { zdMove %W 1 %x %y }
    bind $f1.canvas <ButtonRelease-1> "$this zdUp 1 %x %y "
}

proc channel_zdPrint {this} {
    .@zd${this}.pic.canvas postscript -file /tmp/[$this name].ps
}

proc channel_zdSave {this chan} {
    mkFileBox .@zs$this "Save Sketch ${chan}" {}\
      "Save ${chan} sketch pad to:" \
      "OK {.@zd${this}.pic.canvas postscript -file }" \
      {Cancel {}}
}

proc channel_zdDo {this args} {
    global myid
    set w .@zd$this.pic.canvas
    switch [lindex $args 0] {
    delete {$w delete [string tolower [lindex $args 1]] }
    default {
	    set tid [eval $w [join $args]]
	    $w itemconfigure $tid -tags [$myid lname]
	}
    }
    set chan [$this name]
    sendCtcp ZIRCON ${chan} "DRAW ${chan} [join $args]"
}

proc channel_zdClear {this} {
    global myid
    $this zdDo delete [$myid name]
}

proc channel_zdPress {this btn x y} {
    global ZDMode ZDStart ZDLast
    set w .@zd$this
    if [string match {} $ZDStart($w)] {
	set ZDLast($w) [set ZDStart($w) [list $x $y]]
	return
    }
    global ZDFill ZDOln
    if [string match {none} [set fill $ZDFill($w)]] {set fill {{}}}
    if [string match {none} [set oln $ZDOln($w)]] {set oln {{}} }
    case $ZDMode($w) {
    {arc oval rectangle} {
	    $this zdDo create $ZDMode($w) [join $ZDStart($w)] $x $y \
	    -fill $fill -outline $oln
	    set ZDLast($w)[ set ZDStart($w) {}]
	}
    line {
	    $this zdDo create $ZDMode($w) [join $ZDStart($w)] $x $y \
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
	    mkEntryBox .@[newName text] Text {Enter your text:} \
	       {{Text {}}} "OK {zAddText $this $w $x $y}" {Cancel {}}
	}
    }
}
#
proc zAddText {this w x y txt} {
    global ZDOln ZDFill
#    if [string match {none} [set oln $ZDOln($w)]] {set oln {{}} }
    if ![string match {none} [set fill $ZDFill($w)]] {
	$this zdDo create text $x $y -text "{$txt}" -fill $fill
    }
}
#
proc zdMove {win btn x y} {
    global ZDStart
    set w [winfo toplevel $win]
    if {[string match {} $ZDStart($w)] || $ZDStart($w) == [list $x $y]} { return }
    global ZDMode ZDLast ZDFill ZDOln
    if [string match {none} [set fill $ZDFill($w)]] {set fill {{}}}
    if [string match {none} [set oln $ZDOln($w)]] {set oln {{}} }
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

proc channel_zdUp {this btn x y} {
    global ZDMode ZDStart
    set w .@zd$this
    if {![string match "" $ZDStart($w)] && $ZDStart($w) != [list $x $y]} {
	global ZDLast ZDFill ZDOln
	if [string match {none} [set fill $ZDFill($w)]] {set fill {{}}}
	if [string match {none} [set oln $ZDOln($w)]] {set oln {{}} }
	set win $w.pic.canvas
	$win delete @r
	case $ZDMode($w) {
	{arc oval rectangle} {
		$this zdDo create $ZDMode($w) [join $ZDStart($w)] $x $y \
		  -fill $fill -outline $oln
		set ZDLast($w) [set ZDStart($w) {}]
	    }
	line {
		$this zdDo create $ZDMode($w) [join $ZDStart($w)] $x $y \
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

proc channel_zdDouble {this btn x y} {
    global ZDMode ZDStart ZDLast
    set w .@zd$this
    if [string match "" $ZDStart($w)] {
	set ZDLast($w) [set ZDStart($w) [list $x $y]]
	return
    }
    global ZDFill ZDOln
    if [string match {none} [set fill $ZDFill($w)]] {set fill {{}}}
    if [string match {none} [set oln $ZDOln($w)]] {set oln {{}} }
    case $ZDMode($w) {
    {arc oval rectangle} {
	    $this zdDo create $ZDMode($w) [join $ZDStart($w)] $x $y \
	      -fill $fill -outline $oln
	    set ZDLast($w)[ set ZDStart($w) {}]
	}
    line {
	    $this zdDo create $ZDMode($w) [join $ZDStart($w)] $x $y \
	      -fill $oln
	    set ZDLast($w)[ set ZDStart($w) {}]
	}
    polygon {
	    global ZDPoints
	    $this zdDo create polygon [join $ZDStart($w)] \
	      [join $ZDPoints($w)] $x $y -fill $fill
	    ${w}.pic.canvas delete @p
	    if {$oln != {{}}} {
		$this zdDo create line [join $ZDStart($w)] \
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
