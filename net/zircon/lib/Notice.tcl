set defNotice {}
#
proc Notice {name args} {
    if [string match {::} $name] {
	set op [lindex $args 0]
	if ![string match {} [info procs Notice_$op]] {
	    return [eval Notice_$op [lrange $args 1 end] ]
	} {
	    return [eval Channel_$op [lrange $args 1 end] ]
	}
    }
    if [string match {nil} [set id [Notice :: find $name]]] {
	set id [makeNotice $name]
    }
    if ![string match {} $args] { eval $id configure $args }
    return $id
}
#
proc notice_call {this op args} {
    switch $op {
    default {
	    if [string match {} [info procs notice_$op]] {
		eval channel_call $this $op $args
	    } {
		eval notice_$op $this $args
	    }
	}
    }
}
#
proc makeNotice {chan} {
    global Clname NTO Cname defNotice defChan
    set this [newName Notice]
    proc $this {args} " eval notice_call $this \$args "
    set lchan [string tolower $chan]
    set Cname($this) ${chan}
    set Clname($this) $lchan
    $this configure -hpos 0 -hbuff {} -window {} -actions 0 -patterns {} \
      -logfile {} -log {} -menu 0 -join 0 -ops {} -bindings {} \
      -icon {} -topics {} -keep 0 -monitor 0 -crypt {}
    if [string match {} [set def $defNotice]] {
	set def $defChan
	set b 0
	set d 0
    } {
	set b [$def buttons]
	set d [$def draw]
    }
    $this configure -open [$def open] -close [$def close] \
      -jump [$def jump] -quiet [$def quiet] -draw $d \
      -history [$def history] -closetime [$def closetime] \
      -buttons $b
    set NTO($lchan) $this
    return $this
}
#
proc notice_delete {this} { mcnDelete $this NTO }
#
proc Notice_make {nk} {
    global NTO
    set usr [User :: make $nk]
    set ln [string tolower $nk]
    if [info exists NTO($ln)] { set id $NTO($ln) } { set id [Notice $nk] }
    $id configure -crypt [$usr crypt]
    $id show
    $id addUser $usr 0 0
    return $id
}
#
proc Notice_find {nk} {
    global NTO
    set ln [string tolower $nk]
    return [expr {[info exists NTO($ln)] ? $NTO($ln) : {nil}}]
}
#
proc Notice_save {desc} {
    global defNotice
    $defNotice save $desc
}
#
proc notice_save {this desc} {
    global defNotice
    set ln [mncSave $this $desc $defNotice]
    foreach op {logfile icon} {
	if ![string match {} [set v [$this $op]]] {
	    append ln " -$op {$v}"
	}
    }
    puts $desc $ln
    foreach  b [$this bindings] {
	puts $desc "zbind [$this name] [lindex $b 0] {[lindex $b 1]}"
    }
}
