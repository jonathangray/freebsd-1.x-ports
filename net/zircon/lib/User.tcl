#
set zircon(uvars) {Unick Ulnick Uchan Unotify Ufriend Uison \
  Ulimbo URef Ucrypt}
#
proc User {name args} {
    if [string match {::} $name] {
	return [eval User_[lindex $args 0] [lrange $args 1 end]]
    }
    set this [newName User]
    global Unick Ulnick UTO Uchan Unotify Ufriend Uison Ulimbo URef Ucrypt
    set Unick($this) $name
    set Ulnick($this) [string tolower $name]
    set UTO($Ulnick($this)) $this
    set Uchan($this) {}
    set Unotify($this) 0
    set Ufriend($this) 0
    set Uison($this) 0
    set Ucrypt($this) {}
    set Ulimbo($this) 0
    set URef($this) 0
    proc $this {args} " eval user_call $this \$args "
    if ![string match {} $args] { eval $this configure $args }
    return $this
}
#
proc user_configure {this args} {
    while {![string match {} $args]} {
	set op [lindex $args 0]
	set val [lindex $args 1]
	switch -- $op {
	-friend {
		global Ufriend
		if [set Ufriend($this) $val] {
		    $this ref
		} {
		    $this deref
		}
	    }
	-notify { $this notify $val }
	default
	    {
		set name "U[string range $op 1 end]"
		global $name
		set ${name}($this) $val
	    }
	}
	set args [lrange $args 2 end]
    }
}
#
proc user_call {this op args} {
   switch $op {
   name { global Unick ; return $Unick($this) }
   lname { global Ulnick ; return $Ulnick($this) }
   channels { global Uchan ; return $Uchan($this) }
   isFriend { global Ufriend ; return $Ufriend($this) }
   crypt { global Ucrypt ; return $Ucrypt($this) }
   isNotify { global Unotify ; return $Unotify($this) }
   ison { global Uison ; return $Uison($this) }
   ref { global URef ; incr URef($this) }
   deref { global URef ; incr URef($this) -1 }
   inLimbo { global Ulimbo ; return $Ulimbo($this) }
   default { eval user_$op $this $args }
   }
}
#
proc user_notify {this flag} {
   global Unotify notify
   set x [lsearch $notify [$this lname]]
   if [set Unotify($this) $flag] {
	if {$x < 0} { lappend notify [$this lname]}
   } {
	if {$x >= 0} {listdel notify $x }
   }
}
#
proc user_delete {this} {
    global UTO zircon
    $this notify 0
    unset UTO([$this lname])
    foreach v $zircon(uvars) { global $v ; unset ${v}($this) }
    rename $this {}
}
#
proc user_rename {this nk} {
    global Unick Ulnick UTO
    if [set x [$this isNotify]] { $this notify 0 }
    if [$this isFriend] { friends rename $this $nk }
    set Unick($this) $nk
    unset UTO($Ulnick($this))
    set Ulnick($this) [string tolower $nk]
    set UTO($Ulnick($this)) $this
    $this notify $x
}
#
proc user_join {this chan} {
    global Uchan
    lappend Uchan($this) $chan
    $this ref
}
#
proc user_leave {this chan} {
    global Uchan
    if {[set x [lsearch $Uchan($this) $chan]] >= 0} {
	listdel Uchan($this) $x
    }
    $this deref
}
#
proc user_doNotify {this} {
    if [$this isNotify] {
	$this notify 1
	sendISON
    } {
	global Uison
	if [info exists Uison($who)] {
	    friends mark $who {}
	    unset Uison($who)
	}
	$this notify 0
    }
}
#
proc user_on {this} {
    global Uison Ulimbo
    set Uison($this) 1
    set Ulimbo($this) 0
}
#
proc user_off {this} {
    global Uison
    set Ulimbo($this) 0
    set Uison($this) 0
    friends remove $this
}
#
proc user_limbo {this flag} {
    global Ulimbo
    set Ulimbo($this) $flag
}
#
proc user_finger {this} { finger [$this name] }
#
proc user_mode {this mode args} {
    sendIRC MODE [$this name] $mode [lindex $args 0]
}
#
proc user_dcc {this cmd} {
    set nk [$this name]
    switch $cmd {
    SEND {
	    mkFileBox .@dccSend$nk "Send $nk" "File to send to $nk" {}\
	      "Send {DCCSend $this}" {Cancel {}}
	}
    CHAT {
	    global hostIPAddress Cwho AChat
	    if [info exist AChat($this)] {
		mkDialog {} .@chat$this {Chat} \
		  "You already have a chat request open to $nk." {} \
		  "Close {$this unChat}" {Keep {}}
	    } \
	    elseif ![string match {nil} [Chat :: find $nk]] {
		mkDialog {} .@chat$this {Chat} \
		  "You already have a chat session open to $nk." {} \
		  {Keep {}} "Close {$this unChat}"
	    } \
	    elseif ![catch {dp_connect -server 0} sk] {
		set sock [lindex $sk 0]
		set Cwho($sock) $this
		set AChat($this) $sock
		dp_filehandler $sock re acceptChat
		sendCtcp DCC $nk \
		  "CHAT chat [ipPack $hostIPAddress] [lindex $sk 1]"
	    } {
		info0 addText ERROR "*** $hostIPAddress : $sk"
	    }
	}
    }
}
#
proc user_heal {this} {
    global Split Heal
    foreach sl [array names Split] {
	if {[set x [lsearch $Split($sl) $this]] >= 0} {
	    if ![info exists Heal($sl)] { info0 optText HEAL "*** Heal - $sl" }
	    set v $Split($sl)
	    listdel v $x
	    if ![string match "" $v] {
		set Split($sl) $v
		set Heal($sl) 120000
	    } {
		unset Split($sl)
		catch {unset TSplit($sl)}
		catch {unset Heal($sl)}
	    }
	}
    }
    friends enable $this 
}
#
proc user_unChat {this} {
    global AChat
    if [info exist AChat($this)] {
	global CWho
	catch {unset CWho($AChat($this))}
	catch {dp_shutdown $AChat($this) all}
	catch {close $AChat($this)}
	unset AChat($this)
    } \
    elseif ![string match {nil} [set id [Chat :: find [$this name]]]] {
	$id delete
    }
}
#
proc user_kill {this} { kill [$this name] }
#
proc user_pack {this where} {
    foreach v {Unick Ulnick Unotify Ufriend} {
	global $v ${where}${v}
	set ${where}${v}($this) [set ${v}($this)]
    }
    global UTO ${where}UTO
    set ln $Ulnick($this)
    set ${where}UTO($ln) $UTO($ln)
}
#
proc user_unpack {this where} {
    foreach v {nick notify friend} {
	global ${where}U${v}
	$this configure -$v [set ${where}U${v}($this)]
	unset ${where}U${v}($this)
    }
    global ${where}UTO
    catch {unset ${where}UTO([$this lname])}
}
#
proc user_copy {this what} {
    foreach v {Uchan Unotify Ufriend Uison Ulimbo URef Ucrypt} {
	global $v ; set ${v}($this) [set ${v}($what)]
    }
}
