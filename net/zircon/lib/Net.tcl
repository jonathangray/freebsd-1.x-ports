#
proc net0 {op args} {
    switch $op {
    display {
	    global mainInfo verboseCTCP
	    set tag [lindex $args 0]
	    if {$tag != {@CTCP} || $verboseCTCP} {
		$mainInfo addText $tag [lindex $args 1]
	    }
	}
    info { global mainInfo ; return $mainInfo }
    default { eval net_$op net0 $args }
    }
}
#
proc net_configure {this args} {
}
