global defChat
set defChat {}
#
proc Chat {name args} {
    if [string match {::} $name] {
	set op [lindex $args 0]
	if ![string match {} [info procs Chat_$op]] {
	    return [eval Chat_$op [lrange $args 1 end] ]
	} {
	    return [eval Channel_$op [lrange $args 1 end] ]
	}
    }
    if [string match {nil} [set id [Chat :: find $name]]] {
	set id [makeChat $name]
    }
    if ![string match {} $args] { eval $id configure $args }
    return $id
}
#
proc chat_action {this string} { $this send $string }
#
proc chat_send {this string args} {
    notIdle {}
    if ![string match {} $string] {
	global Chat monitorOut
	if [info exists Chat($this)] {
	    if [catch [list puts $Chat($this) $string] err] {
		$this addText {} "*** Error : $err"
	    } {
		flush $Chat($this)
		$this addText @me "= $string"
	        if $monitorOut { puts stderr ">= $string" }
	    }
	} {
	    $this addText {} {*** Connection is closed!!!!}
	}
    }
}
#
proc chat_call {this op args} {
    switch $op {
    default {
	    if [string match {} [info procs chat_$op]] {
		eval channel_call $this $op $args
	    } {
		eval chat_$op $this $args
	    }
	}
    }
}
#
proc makeChat {chan} {
    global Clname CHTO Cname defChat defChan
    set this [newName Chat]
    proc $this {args} " eval chat_call $this \$args "
    set lchan [string tolower $chan]
    set Cname($this) ${chan}
    set Clname($this) $lchan
    $this configure -hpos 0 -hbuff {} -window {} -actions 0 -patterns {} \
      -logfile {} -log {} -bindings {} -icon {} -crypt {}
    if [string match {} [set def $defChat]] {
	set def $defChan
	set b 0
    } {
	set b [$def buttons]
    }
    $this configure -open [$def open] -close [$def close] \
      -jump [$def jump] -quiet [$def quiet] \
      -history [$def history] -closetime [$def closetime] -buttons $b
    set CHTO($lchan) $this
    return $this
}
#
proc chat_delete {this} {
    global Clname Name Clog Chat CHTO
    if ![string match {} $Clog($this)] { close $Clog($this) }
    catch {unset Name([$this window])}
    $this configure -window {}
    set chan $Clname($this)
    unset Clname($this) Clog($this)
    unset CHTO(${chan})
    foreach v {Copen Cclose Chistory Cdraw BF Bg Fg Cjump Cquiet Chpos \
      Chbuff Cactions Clogfile Cclosetime Ccrypt Cbuttons \
      Cclosecount Cwindow OType Cpatterns Cname Cmenu Cjoin \
      Cbindings Cicon Ctext} {
	global $v ; catch {unset ${v}($this)}
    }
    rename $this {}
    if [info exists Chat($this)] { closeChat $this ${chan} $Chat($this) }
}
#
proc chat_leave {this} {
    set chan [$this name]
    set msg "Really leave DCC chat with ${chan}?"
    mkDialog LEAVE .@$this "Leaving ${chan}" $msg {} \
      "OK {$this doLeave}" {Cancel {}}
}
#
proc Chat_make {name} {
    global CHTO
    set ln [string tolower $name]
    if [info exists CHTO($ln)] { return $CHTO($ln) } { return [Chat $name]}
}
#
proc Chat_find {name} {
    global CHTO
    set ln [string tolower $name]
    return [expr {[info exists CHTO($ln)] ? $CHTO($ln) : {nil}}]
}
#
proc Chat_save {desc} {
    global defChat
    $defChat save $desc
}
#
proc chat_save {this desc} {
    global defChat
    set ln [mncSave $this $desc $defChat]
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
