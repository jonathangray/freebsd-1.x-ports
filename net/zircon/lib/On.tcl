#
# ON condition support code
#
proc on {action pattern code} {
    set pat {}
    foreach p $pattern {
	lappend pat [string tolower $p]
    }
    global OnCode
    lappend OnCode($action) [list $pat $code]
}
#
proc processOns {} {
    global OnCode ons
    if ![string match {} $ons] {
	foreach act $ons {
	    lappend OnCode([lindex $act 0]) [lrange $act 1 end]
	}
	mkDialog {} .@ons {On Warning} \
	  {Your configuration file needs updating ("on" format).} {} \
	  {Update {saverc}} {Cancel {}}
    }
}
#
proc handleOn {action pattern} {
    global OnCode zircon
    if { $zircon(o) && [info exists OnCode($action)]} {
	foreach act $OnCode($action) {
	    set re [lindex $act 0]
	    set i 0
	    set match 1
	    foreach pat $pattern {
		set up [lindex $re $i]
		if {![string match {} $up] && ![regexp -nocase $up $pat]} {
		    set match 0
		    break
		}
		incr i
	    }
	    if $match {
		set i 0
		foreach pat $pattern {
		    global onPar${i}
		    set onPar${i} $pat
		    incr i
		}
		uplevel #0 [lindex $act 1]
		while {$i >= 0} { catch "unset onPar${i}" ; incr i -1}
		return
	    }
	}
    }
}
#
proc operator {chan} {
    return [[Channel :: find ${chan}] operator]
}
