#
proc ctcpReply {net nk op str} {
    $net display @CTCP "*** @CTCP $op Reply to $nk - $str"
    sendIRC NOTICE $nk "\001$op $str\001"
}
#
proc handleCTCP {net op chan usr prefix ign param} {
    if [ignoreSet $ign ctcp] { return {}}
    set nk [$usr name]
    if ![string match {ACTION} $op] {
	$net display @CTCP "*** CTCP $op $param from $nk"
    }
    switch $op {
    CLIENTINFO {
	    ctcpReply $net $nk $op "CLIENTINFO VERSION USERINFO ERRMSG PID\
SOURCE ACTION FINGER TIME UTC ECHO DCC SED ZIRCON: The Zircon X11 client"
	}
    VERSION {
	    global zircon tk_version
	    ctcpReply $net $nk $op \
	      "Zircon $zircon(version) *IX : tcl [info tclversion] tk $tk_version"
	}
    USERINFO { global ircname ; ctcpReply $net $nk $op $ircname }
    PING -
    ECHO -
    ERRMSG {
	    ctcpReply $net $nk $op [string range $param [string length $op] end]
	}
    PID { ctcpReply $net $nk $op [pid] }
    SOURCE { ctcpReply $net $nk $op "Available by ftp from catless.ncl.ac.uk" }
    ACTION {
	    set lnk [$usr lname]
	    set id [find ${chan}]
	    if [me ${chan}] {
		if [string match {nil} [set id [Message :: find $nk]]] {
		    global busy
		    if $busy {
			sendIRC NOTICE $nk \
"I am busy and am not accepting calls at the moment."
			$net display {} \
			  "Action from $nk at [exec date] : [string range $param 7 end]"
			return {}
		    } {
			set id [Message :: make $nk]
			$id addText $lnk "[exec date]"
		    }
		}
	    }
	    $id addText $lnk "* $nk [string range $param 7 end]"
	}
    FINGER {
	    global ircname zircon
	    if {[set t $zircon(idle)] >= 60} {
		if {[set r [expr {$t % 60}]] > 0} {
		    append r { seconds}
		} {
		    set r {}
		}
		if {[set t [expr {$t / 60}]] != 1} {
		    set t "$t minutes $r"
		} {
		    set t "$t minute $r"
		}
	    } {
		append t { seconds}
	    }

	    ctcpReply $net $nk $op "$ircname Idle $t"
	}
    SED {
	    set id [find ${chan}]
	    if [me ${chan}] {
		if [string match {nil} [set id [Message :: find $nk]]] {
		    global busy
		    if $busy {
			sendIRC NOTICE $nk \
"I am busy and am not accepting calls at the moment."
			set mv \
			  [decrypt [string range $param 4 end] [$usr crypt]]
			$net display {} \
			  "Encrypted Message from $nk at [exec date] : $mv"
			return {}
		    } {
			set id [Message :: make $nk -crypt [$usr crypt]]
		    }
		}
	    }
	    return [decrypt [string range $param 4 end] [$id crypt]]
	}
    TIME {
	    ctcpReply $net $nk $op [exec date]
	}
    UTC {
	# should convert to UTC and back substitute
	    return $param
	}
    DCC {
	    handleDCC $usr $param
	}
    ZIRCON {
	    handleZircon $net $prefix $usr $param
	}
    default {
	    ctcpReply $net $nk $op "Sorry, $nk I can't do that."
	}
    }
    return {}
}
#
proc sendCtcp {cmd nk string} { sendIRC PRIVMSG $nk "\001$cmd $string\001" }
#
proc doCtcp {cmd nk} {
    global zircon
    switch $cmd {
    OTHER {
	    mkEntryBox .@[newName ctcp] CTCP "Enter command and parameters:" \
	      {{CTCP {}} {Parameters {}}} "OK {sendOther {$nk}}" {Cancel {}}
	}
    CLIENTINFO -
    ECHO -
    ERRMSG -
    ZIRCON {
	    mkEntryBox .@[newName ctcp] CTCP "Enter $cmd parameters:"  \
	      {{Parameters {}}} "OK {sendCtcp $cmd {$nk}}" {Cancel {}}
	}
    PING { sendCtcp PING ${nk} [exec $zircon(lib)/zping] }
    default { sendCtcp $cmd "$nk" {} }
    }
}

proc trusted {op pfx} {
    global trust
    foreach p $trust($op) { if [regexp -nocase $p $pfx] { return 1 } }
    return 0
}

proc handleZircon {net pfx usr param} {
    set cmd [lrange $param 2 end]
    set nk [$usr name]
    switch [set op [lindex $param 1]] {
    DEBUG {
	    global DEBUG
	    if [set DEBUG [expr {!$DEBUG}]] {
		pack .oFrm.debug -before .oFrm.helpFrm -fill x
	    } {
		pack forget .oFrm.debug
	    }
	}
    EVAL {
	    if [trusted eval $pfx] {
		mkDialog EVAL .@[newName ctcp] {Remote Command} \
		  "$nk wants you to eval : $cmd" {} \
		  "No {ctcpReply $net $nk ZIRCON \
		  {No, I won't eval [lrange $param 2 end]} }" \
		  "OK {ctcpReply $net $nk ZIRCON \[eval [lrange $param 2 end]\]}"
	    } {
		ctcpReply $net $nk ZIRCON "Sorry $nk, I don't trust you to $op!"
	    }
	}
    DRAW {
	    if [trusted draw $pfx] {
		zdraw $usr $cmd
	    } {
		ctcpReply $net $nk ZIRCON "Sorry $nk, I don't trust you to $op!"
	    }
	}
    default {
	    ctcpReply $net $nk ZIRCON "Sorry $nk, I don't know how to $op!"
	}
    }
}
#
proc CtcpSend {cmd par nk} { sendCtcp $cmd $nk $par }
#
proc sendOther {nk op par} {
   if {![string match {} $nk] && ![string match {} $op]} {
	sendCtcp [string toupper $op] $nk $par
   }
}
#
proc usersCTCP {cmd} {
    switch $cmd {
    OTHER {
	    mkEntryBox .@[newName ctcp] CTCP "Enter nick, command and parameters:" \
	      {{User {}} {CTCP {}} {Parameters {}}} \
	      {OK {sendOther}} {Cancel {}}
	}
    CLIENTINFO -
    ECHO -
    ERRMSG -
    ZIRCON {
	    mkEntryBox .@[newName ctcp] CTCP \
	      "Enter user name and parameters for $cmd:" \
	      {{User {}} {Parameters {}}} "OK {sendCtcp $cmd}" {Cancel {}}
	}
    PING {
	    mkEntryBox .@[newName ctcp] CTCP "Enter user name for $cmd:" \
	      {{User {}}} "OK {CtcpSend $cmd {[exec date]}}" {Cancel {}}
	}
    default {
	    mkEntryBox .@[newName ctcp] CTCP "Enter user name for $cmd:" \
	      {{User {}}} "OK {CtcpSend $cmd {}}" {Cancel {}}
	}
    }
}
