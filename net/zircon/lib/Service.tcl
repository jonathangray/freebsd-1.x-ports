#
proc svid {name} {
    global VTO
    set name [string tolower $name]
    if [info exists VTO($name)] { return $VTO($name) } { return nil }
}
#
proc Service {name args} {
    if [string match {::} $name] {
	return [eval Service_[lindex $args 0] [lrange $args 1 end]]
    }
    set this [newName Service]
    global VTO Vhost Vnick Vops Vname Vaddr
    set VTO([string tolower $name]) $this
    set Vname($this) $name
    set Vhost($this) {}
    set Vnick($this) $name
    set Vops($this) {}
    set Vaddr($this) $name
    proc $this {args} " eval service_call $this \$args "
    if ![string match {} $args] { eval $this configure $args }
    if ![string match {} $Vhost($this)] {
	global Secure
	set Vaddr($this) $Vnick($this)@$Vhost($this)
	set Secure([string tolower $Vnick($this)]) $Vaddr($this)
    }
    return $this
}
#
proc service_configure {this args} {
    while {![string match {} $args]} {
	set name "V[string range [lindex $args 0] 1 end]"
	global $name
	set ${name}($this) [lindex $args 1]
	set args [lrange $args 2 end]
    }
}
#
proc service_call {this op args} {
   switch $op {
   name { global Vname ; return $Vname($this) }
   host { global Vhost ; return $Vhost($this) }
   nick { global Vnick ; return $Vnick($this) }
   addr { global Vaddr ; return $Vaddr($this) }
   ops  { global Vops  ; return $Vops($this) }
   default { eval service_$op $this $args }
   }
}
#
proc service_delete {this} {
    global VTO
    unset VTO([string tolower [$this name]])
    foreach v {Vhost Vname Vnick Vaddr Vops} {
	global $v ; unset ${v}($this)
    }
    rename $this {}
}
#
proc service_send {this op par} { sendIRC PRIVMSG [$this addr] "$op $par" }
#
proc service_do {this op} {
    set sv [$this name]
    mkEntryBox .@sv$this $sv "Enter any parameters needed for $sv:" \
	[list [list $op {}]] "OK {$this send $op }" {Cancel {}}
}
#
proc Service_list {} { global Vname ; return [array names Vname] }
#
proc Service_save {desc} {
    foreach sv [Service :: list] {
	set nm [$sv name]
	switch [string tolower $nm] {
	nickserv -
	noteserv { }
	default {
		set ln "Service $nm"
		foreach p {nick host ops} {
		    if ![string match {} [set n [$sv $p]]] {
			append ln " -$p {$n}"
		    }
		}
		puts $desc $ln
	    }
	}
    }
}
