set defMsg {}
#
proc Message {name args} {
    if [string match {::} $name] {
	set op [lindex $args 0]
	if ![string match {} [info procs Message_$op]] {
	    return [eval Message_$op [lrange $args 1 end] ]
	} {
	    return [eval Channel_$op [lrange $args 1 end] ]
	}
    }
    if [string match {nil} [set id [Message :: find $name]]] {
	set id [makeMessage $name]
    }
    if ![string match {} $args] { eval $id configure $args }
    return $id
}
#
proc message_call {this op args} {
    switch $op {
    default {
	    if [string match {} [info procs message_$op]] {
		eval channel_call $this $op $args
	    } {
		eval message_$op $this $args
	    }
	}
    }
}
#
proc makeMessage {chan} {
    global Clname MTO Cname defMsg defChan
    set this [newName Message]
    proc $this {args} "eval message_call $this \$args "
    set lchan [string tolower $chan]
    set Cname($this) ${chan}
    set Clname($this) $lchan
    $this configure -hpos 0 -hbuff {} -window {} -actions 0 -patterns {} \
      -logfile {} -log {} -menu 0 -join 0 -ops {} -bindings {} \
      -icon {} -topics {} -keep 0 -monitor 0 -crypt {}
    if [string match {} [set def $defMsg]] {
	set def $defChan
	set b 0
    } {
	set b [$def buttons]
    }
    $this configure -open [$def open] -close [$def close] \
      -jump [$def jump] -quiet [$def quiet] -draw [$def draw] \
      -history [$def history] -closetime [$def closetime] \
      -buttons $b
    set MTO($lchan) $this
    return $this
}
#
proc message_delete {this} { mcnDelete $this MTO }
#
proc Message_make {nk args} {
    global MTO
    set usr [User :: make $nk]
    set ln [string tolower $nk]
    if [info exists MTO($ln)] { set id $MTO($ln) } { set id [Message $nk] }
    $id configure -crypt [$usr crypt]
    $id show
    $id addUser $usr 0 0
    if ![string match {} $args] { eval $id configure $args }
    return $id
}
#
proc Message_find {nk} {
    global MTO
    set ln [string tolower $nk]
    return [expr {[info exists MTO($ln)] ? $MTO($ln) : {nil}}]
}
#
proc Message_save {desc} {
    global defMsg
    $defMsg save $desc
    foreach ch [Message :: list] {
	if {$ch != $defMsg && [$ch keep]} { $ch save $desc }
    }
}
#
proc message_save {this desc} {
    global defMsg
    set ln [mncSave $this $desc $defMsg]
    foreach op {draw} {
	if {$this == $defMsg || [$this $op] != [$defMsg $op]} {
	    append ln " -$op [$this $op]"
	}
    }
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
