#
proc Server {name args} {
    if [string match {::} $name] {
	return [eval Server_[lindex $args 0] [lrange $args 1 end]]
    }
    global STO Shost Sport Soper Soperpw Sscript Sname
    set this [newName Server]
    set STO([string tolower $name]) $this
    set Sname($this) $name
    set Shost($this) $name
    set Sport($this) 6667
    set Soper($this) {}
    set Soperpw($this) {}
    set Sscript($this) {}
    proc $this {args} " eval server_call $this \$args "
    if ![string match {} $args] { eval $this configure $args }
    return $this
}
#
proc server_configure {this args} {
    while {![string match {} $args]} {
	set val [lindex $args 1]
	switch [set var [string range [lindex $args 0] 1 end]] {
	host {
		global Shost STO
		set Shost($this) $val
	    }
	default {
		set name "S$var"
		global $name
		set ${name}($this) $val
	    }
	}
	set args [lrange $args 2 end]
    }
}
#
proc server_call {this op args} {
   switch $op {
   name { global Sname ; return $Sname($this) }
   host { global Shost ; return $Shost($this) }
   port { global Sport ; return $Sport($this) }
   script { global Sscript ; return $Sscript($this) }
   oper { global Soper Soperpw ; return [list $Soper($this) $Soperpw($this)] }
   default { eval server_$op $this $args }
   }
}
#
proc server_delete {this} {
    global STO
    unset STO([string tolower [$this name]])
    foreach v {Shost Sport Soper Soperpw Sscript} {
	global $v ; unset ${v}($this)
    }
    rename $this {}
}
#
proc Server_select {host} {
    global zircon
    if [string match {nil} [set zircon(host) [Server :: find $host]]] {
	puts stderr "Cannot find host - $host"
    }
}
#
proc Server_list {} { global Shost ; return [array names Shost] }
#
proc Server_save {desc} {
    global STO Shost Sport Soper Soperpw Sscript
    foreach n [array names STO] {
	if [string match {default} $n] { continue }
	set ln "Server $n"
	set id $STO($n)
	if {[string tolower $Shost($id)] != $n} {
	    append ln " -host $Shost($id)"
	}
	if ![string match {6667} $Sport($id)] {
	    append ln " -port $Sport($id)"
	}
	if ![string match {} $Soper($id)] { append ln " -oper $Soper($id)" }
	if ![string match {} $Soperpw($id)] {
	    append ln " -operpw $Soperpw($id)"
	}
	if ![string match {} $Sscript($id)] {
	    append ln " -script {$Sscript($id)}"
	}
	puts $desc $ln
    }
}
#
proc Server_make {host} {
    if [string match {nil} [set s [Server :: find $host]]] {
	set s [Server $host]
    }
    return $s
}
#
proc server_pack {this where} {
    foreach v {Shost Sport Soper Soperpw Sscript Sname} {
	global $v ${where}${v}
	set ${where}${v}($this) [set ${v}($this)]
    }
    global newSTO
    set newSTO([string tolower $Shost($this)]) $this
}
#
proc server_unpack {this where} {
    foreach v {host port oper operpw script} {
	global $v ${where}S${v}
	$this configure -$v [set ${where}S${v}($this)]
	unset ${where}S${v}($this)
    }
    global ${where}Sname ${where}STO
    unset ${where}STO([string tolower [set ${where}Sname($this)]])
    unset ${where}Sname($this)
}
#
proc Server_pack {where} {
    foreach s [Server :: list] { $s pack $where }
}
#
proc Server_unpack {where} {
    global ${where}STO
    foreach s [array names ${where}STO] { [set ${where}STO($s)] unpack $where }
    Server :: cleanup $where
}
#
proc Server_cleanup {where} {
    foreach v {Shost Sport Soper Soperpw Sscript Sname STO} {
	global ${where}${v}
	catch {unset ${where}${v}}
    }
}
#
proc Server_find {name} {
    global STO
    set name [string tolower $name]
    if [info exists STO($name)] { return $STO($name) } { return nil }
}
