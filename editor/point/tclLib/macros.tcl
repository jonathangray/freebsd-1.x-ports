
# Some macros to extend Point commands
#
proc Filter {{cmd fmt}} {
	set s [selection get]
	set ret [catch {exec $cmd << $s} ns]
	# if the command failed then do not delete the selection
	if {$ret==0} DeleteToScrap
	# insert the result or the error message
	InsertString $ns
}

proc CenterLines {} {
	# get the right margin and the current selection
	set rightMargin [Option get rightMargin]
	set sel [Sel get]
	# remember where the selection is
	set start [lindex $sel 0]
	set stop [lindex $sel 1]
	# loop once for each line in the selection
	set lastLeft -1
	for {} 1 {} {
		set left [MoveSel line left0]
		# check to be sure we are not at the end of the file
		if {$left<=$lastLeft} \
			break
		set lastLeft $left
		# see if we are past the selection
		if {$left>=$stop} \
			break;
		# remove white space at the beginning of the line
		for {} 1 {} {
			set selchar [string index [Sel return] 0]
			if {$selchar!="\t"&&$selchar!=" "} \
				break;
			DeleteToScrap
			incr stop -1
		}
		# find the rightmost character
		MoveSel line right
		set right [lindex [Sel get] 0]
		# move back to the beginning of the line
		MoveSel line left0
		# figure out where to center it
		set len [expr $right-$left]
		set extra [expr ($rightMargin-$len)/2]
		while {$extra>0} {
			InsertString " "
			incr stop 1
			incr extra -1
		}
		# move to the next line
		MoveSel char down
	}
	Sel set $start $stop
	set w [WindowName get sel]
	Redraw
}

proc DeleteLine {{all 0}} {
	if $all {
		MoveSel line left0 noupdate
	}
	set beginSel [lindex [Sel get] 0]
	MoveSel line right noupdate
	set endSel [lindex [Sel get] 1]
	if !$all {
		set endSel [expr $endSel-1]
	}
	Sel set $beginSel $endSel
	DeleteToScrap
	Redraw
}


proc LatexSelection {} {
	set sel [Sel get]
	set start [lindex $sel 0]
	set stop [lindex $sel 1]
	set sel1 [selection get]
	pr "Sel is <$sel1>"
	set fid [open "xx99.tex" w]
	puts $fid "\\nonstopmode{}\\input{texheader}{}{}"
	puts $fid $sel1
	puts $fid "\\input{textrailer}{}"
	close $fid
	set ret [catch {exec latex xx99} out]
	pr "ret=$ret and out=<$out>"
	exec xdvi xx99.dvi
}


proc IndentSelection {{outdent 0}} {
	set sel [Sel get]
	set start [lindex $sel 0]
	set stop [lindex $sel 1]
	set lastHere -1
	for {} 1 {} {
		set here [MoveSel line left0]
		# check to be sure we are not at the end of the file
		if {$here<=$lastHere} \
			break
		set lastHere $here
		Sel set $here $here
		if {$here>=$stop} \
			break
		if $outdent {
			# get the one character in the selection
			set selchar [string index [Sel return] 0]
			if {$selchar=="\t"||$selchar==" "} {
				DeleteToScrap
				incr stop -1
			}
		} else {
			InsertString \t
			incr stop 1
		}
		MoveSel char down
	}
	Sel set $start $stop
	set w [WindowName get sel]
	Redraw
}

proc DefineMacro {{id 0}} {
	set name Macro$id
	global $name
	set $name [selection get]
}

proc ExecMacro {{id 0}} {
	set name Macro$id
	global $name
	eval [set $name]
}

proc ExecSel {{id 0}} {
	eval [selection get]
}

proc MoveWindow {geometry} {
	set aw [WindowName get active]
	wm geometry $aw [FixGeometry $geometry]
	RaiseWindow
}

proc ExtendSelToLines {} {
	set endSel [lindex [Sel get] 1]
	MoveSel line left0 noupdate
	set beginSel [lindex [Sel get] 0]
	Sel set $endSel $endSel
	MoveSel line right noupdate
	set endSel [lindex [Sel get] 1]
	Sel set $beginSel $endSel
	set w [WindowName get sel]
	Redraw
}


