
set bindings(dummy) {}

proc bltResetBindings { graph type } {
    global bindings

    set all [array names bindings] 
    set cmds {}
    foreach i $all {
	if [string match "$type,$graph,*" $i] {
	    lappend cmds $bindings($i)
	}
    }
    bind $graph $type [join $cmds \n]
}

proc bltActivateLegend { graph name } {
    global lastActive

    set last $lastActive($graph)
    if { $name != $last } {
	if { $last != "" } {
	    $graph legend deactivate $last
	    $graph element deactivate $last
	}
	if { $name != "" } {
	    $graph legend activate $name
	    $graph element activate $name 
	}
	set lastActive($graph) $name
    }
}

proc SetActiveLegend { graph } {
    global lastActive bindings

    set lastActive($graph) {}
    set bindings(<Motion>,$graph,activeLegend) {
	set info [%W legend get @%x,%y]
	bltActivateLegend %W $info
    }    
    bltResetBindings $graph <Motion>
}


proc SetCrosshairs { graph } {
    global bindings
    
    $graph crosshairs set on 
    set bindings(<Motion>,$graph,crosshairs) {
	%W crosshairs configure -position @%x,%y
    }
    bltResetBindings $graph <Motion>
}


proc bltFindElement { graph x y } {
    set info [$graph element closest $x $y ]
    if { $info == "" } {
	blt_bell
	return
    }
    set name [lindex $info 0]
    set points [lrange $info 2 3]
    set index [lindex $info 1]
    global tagId
    catch { $graph tag delete $tagId($graph,$name,$index) }
    set tagId($graph,$name,$index) \
	[$graph tag create text $points -text " $name \[$index\] " -anchor s \
	 -yoffset -10 -fg black -bg {}]
    bltFlashPoint $graph $name $index 10
}

proc bltFlashPoint { graph name index count } {
    if { $count & 1 } {
        $graph element deactivate $name
    } else {
        $graph element activate $name $index
    }
    incr count -1
    if { $count > 0 } {
	after 200 bltFlashPoint $graph $name $index $count
	update
    } else {
	global tagId
	catch { $graph tag delete $tagId($graph,$name,$index) }
    }
}

proc SetClosestPoint { graph } {
    global bindings

    global tagId
    set tagId(dummy) {}
    set bindings(<ButtonPress-3>,$graph,closestPoint) {
	bltFindElement %W  %x %y
    }
    bltResetBindings $graph <ButtonPress-3>
}


proc bltGetCoords { graph winX winY var index } {
    scan [$graph invtransform $winX $winY] "%s %s" x y 
    scan [$graph xaxis limits] "%s %s" xmin xmax
    scan [$graph yaxis limits] "%s %s" ymin ymax

    if { $x > $xmax } { 
	set x $xmax 
    } elseif { $x < $xmin } { 
	set x $xmin 
    }

    if { $y > $ymax } { 
	set y $ymax 
    } elseif { $y < $ymin } { 
	set y $ymin 
    }
    upvar $var arr
    set arr($index,x) $x
    set arr($index,y) $y
}


proc bltGetAnchor { graph x y } {
    global pos bindings

    set pos(B,x) {}
    set pos(B,y) {}
    bltGetCoords $graph $x $y pos A
    set bindings(<B1-Motion>,$graph,zoom) { 
	bltScan %W %x %y 
    }
    set bindings(<ButtonRelease-1>,$graph,zoom) { 
	bltZoom %W %x %y 
    }
    bltResetBindings $graph <ButtonRelease-1>
    bltResetBindings $graph <B1-Motion>
}


proc bltBox { graph x1 y1 x2 y2 } {
    global tagId 

    set text [format "%.4g,%.4g" $x1 $y1] 
    if { $tagId($graph,text1) == "" } then {
	set tagId($graph,text1) \
	    [$graph tag create text {$x1 $y1} -text $text ] 
    } else {
	$graph tag configure $tagId($graph,text1) -text $text 
	$graph tag coords $tagId($graph,text1) "$x1 $y1"
    }
    set text [format "%.4g,%.4g" $x2 $y2] 
    if { $tagId($graph,text2) == "" } then {
	set tagId($graph,text2) \
	    [$graph tag create text {$x2 $y2} -text $text ] 
    } else {
	$graph tag configure $tagId($graph,text2) -text $text 
	$graph tag coords $tagId($graph,text2) "$x2 $y2"
    }
    set coords {
	$x1 $y1 $x1 $y2 $x1 $y1 $x2 $y1 $x2 $y1 $x2 $y2 $x1 $y2 $x2 $y2 
    }
    if { $tagId($graph,outline) == "" } then {
	set tagId($graph,outline) [$graph tag create line $coords]
    } else {
	$graph tag coords $tagId($graph,outline) $coords
    }
}

set pos(last,x) 0
set pos(last,y) 0

proc bltScan { graph x y } {
    global pos

    set deltaX [expr abs($pos(last,x)-$x)]
    set deltaY [expr abs($pos(last,y)-$y)]
    if { ($deltaX < 5) && ($deltaY < 5) } {
	return
    }	
    set pos(last,x) $x
    set pos(last,y) $y

    bltGetCoords $graph $x $y pos B
    if { $pos(A,x) > $pos(B,x) } { 
	bltBox $graph $pos(B,x) $pos(B,y) $pos(A,x) $pos(A,y)
    } else {
	bltBox $graph $pos(A,x) $pos(A,y) $pos(B,x) $pos(B,y)
    }
}

proc bltZoom { graph x y } {
    global bindings pos tagId

    # Go back to original bindings
    set bindings(<ButtonPress-1>,$graph,zoom) { 
	bltGetAnchor %W %x %y 
    }
    set bindings(<B1-Motion>,$graph,zoom) {}

    catch {$graph tag delete $tagId($graph,text1) $tagId($graph,text2)}
    set tagId($graph,text1) {}
    set tagId($graph,text2) {}

    bltResetBindings $graph <B1-Motion>
    bltResetBindings $graph <ButtonPress-1>

    if { $pos(B,x) == "" } then {
	catch {$graph tag delete $tagId($graph,outline)}
	set tagId($graph,outline) {} 
	$graph xaxis configure -min {} -max {} 
	$graph yaxis configure -min {} -max {}
	return
    }
    if { $pos(A,x) > $pos(B,x) } { 
	$graph xaxis configure -min $pos(B,x) -max $pos(A,x) 
    } else { 
	if { $pos(A,x) < $pos(B,x) } {
	    $graph xaxis configure -min $pos(A,x) -max $pos(B,x) 
	}
    }
    if { $pos(A,y) > $pos(B,y) } { 
	$graph yaxis configure -min $pos(B,y) -max $pos(A,y)
    } else {
	if { $pos(A,y) < $pos(B,y) } {
	    $graph yaxis configure -min $pos(A,y) -max $pos(B,y)
	}
    }
#    $graph configure -cursor crosshair 
    catch {$graph tag delete $tagId($graph,outline)}
    set tagId($graph,outline) {}
    blt_busy hold $graph
    update
    blt_busy release $graph
}


proc SetZoom { graph } {
    global bindings tagId

    set tagId($graph,text1) {}
    set tagId($graph,text2) {}
    set tagId($graph,outline) {}
    set bindings(<ButtonRelease-2>,$graph,zoom) {
	catch {%W tag delete $tagId(outline) }
	set tagId(outline) {} 
	%W yaxis configure -min {} -max {} 
	%W xaxis configure -min {} -max {}
	blt_busy hold %W
	update
	blt_busy release %W
    }
    set bindings(<ButtonPress-1>,$graph,zoom) { 
#       %W configure -cursor {crosshair red black}
	bltGetAnchor %W %x %y 
	bltScan %W %x %y 
    }
    bltResetBindings $graph <ButtonPress-1>
    bltResetBindings $graph <ButtonRelease-2>
}

proc SetPrint { graph } {
    global bindings
    set bindings(<Shift-ButtonRelease-3>,$graph,print) {
	puts stdout "creating file \"out.ps\... " nonewline
	flush stdout
	%W postscript "out.ps" -pagewidth 6.5i -pageheight 8.5i -landscape true
	puts stdout "done."
	flush stdout
    }
    bltResetBindings $graph <Shift-ButtonRelease-3>
}
