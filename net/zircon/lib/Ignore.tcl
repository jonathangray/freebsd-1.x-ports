#
proc ignore {pattern args} {
    global ignores
    set lst [list $pattern $args]
    if {[set x [listmatch $ignores $pattern]] >= 0} {
	listupdate ignores $x $lst
    } {
	lappend ignores $lst
    }
}
#
# Look and see if there are any ignores for this nick/name. List has format:
#	{{pattern {list of what} ......}
#
proc z_ignore {usr nm} {
    global ignores
    set nm [string tolower $nm]
    set nk [$usr lname]
    foreach ig $ignores {
	if [string match [lindex $ig 0] ${nk}!$nm] {
	    return [lindex $ig 1]
	}
    }
    return {}
}
#
proc ignoreSet {lst what} {
    return [expr {[lsearch $lst $what] >= 0}]
}
#
proc ignoreFlag {lst what} {
    set nk [lindex $lst 0]
    set v [lindex $lst 1]
    if {[set x [lsearch $v $what]] < 0} {
	return [list ${nk} [lappend v $what]]
    } {
	return [list ${nk} [listdel v $x]]
    }
}
#
proc flipIgnore {usr what} {
    set nk [$usr lname]
    global ignores
    if {[set x [listmatch $ignores ${nk}!*@*]] < 0} {
	lappend ignores [ignoreFlag [list ${nk}!*@* {}] $what]
    } {
	listupdate ignores $x [ignoreFlag [lindex $ignores $x] $what]
    }
    global confChange
    set confChange 1
}
#
proc ignoreAll {usr} {
    global zircon
    global IFlag
    set lst [z_ignore $usr *@*]
    foreach x $zircon(ignore) {
	set lx [string tolower $x]
	if ![ignoreSet $lst $lx] {
	    flipIgnore $usr $lx
	    set IFlag(${usr},${lx}) 1
	}
    }
}
#
proc ignoreClear {usr} {
    global zircon
    global IFlag
    set lst [z_ignore $usr *@*]
    foreach x $zircon(ignore) {
	set lx [string tolower $x]
	if [ignoreSet $lst $lx] {
	    flipIgnore $usr $lx
	    set IFlag(${usr},${lx}) 0
	}
    }
}
#
proc addIgnoreMenu {win usr} {
    ${win} add cascade -label Ignore -menu ${win}.ignore
    menu ${win}.ignore
    global zircon
    global IFlag
    ${win}.ignore add command -label all -command "ignoreAll ${usr}"
    ${win}.ignore add command -label clear -command "ignoreClear ${usr}"
    set lst [z_ignore $usr *@*]
    foreach x $zircon(ignore) {
	set lx [string tolower $x]
	${win}.ignore add checkbutton -label $x -variable IFlag($usr,${lx}) \
	  -command "flipIgnore $usr $lx"
	set IFlag($usr,${lx}) [ignoreSet $lst $lx]
    }
}
